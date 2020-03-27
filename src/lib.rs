mod exception;
mod ops;
mod util;
mod constant;

use crate::constant::*;
use exception::Exception;
use std::ops::{Add, Sub, Mul, Div, Rem, Shl, Shr, BitAnd, BitOr, Not, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use crate::ops::{add::add_impl, sub::sub_impl, mul::mul_impl, div::div_impl};
use crate::util::{BitWidth, LeadingZeros};

#[repr(C)]
union f32u32 {
    f: f32,
    u: u32,
}

#[repr(C)]
union f64u64 {
    f: f64,
    u: u64,
}

#[derive(Clone, Copy, PartialEq)]
pub enum RoundingMode {
    NearEven,
    NearMax,
    Zero,
    Max,
    Min,
}

#[derive(Clone, Copy)]
pub struct Float<T> { v: T }

impl<T> Float<T>
    where T: FloatFormat,
{
    fn new(sign: bool, exp: T::BaseType, sig: T::BaseType) -> Float<T> {
        let sign = T::BaseType::from(sign) << T::BaseType::try_from(T::exp_width() + T::sig_width()).unwrap();
        let exp_mask = (T::BaseType::from(1) << T::BaseType::try_from(T::exp_width()).unwrap()) - T::BaseType::from(1);
        let exp = (exp & exp_mask) << T::BaseType::try_from(T::exp_width()).unwrap();
        let sig_mask = (T::BaseType::from(1) << T::BaseType::try_from(T::sig_width()).unwrap()) - T::BaseType::from(1);
        let sig = sig & sig_mask;

        Float { v: T::from(sign | exp | sig) }
    }
}

impl Float<u32> {
  pub fn add_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    if self.sign() ^ other.sign() {
        sub_impl::<u32, i32>(self, other, mode)
      } else {
        add_impl::<u32, i32>(self, other, mode)
      }
  }

  pub fn sub_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    if self.sign() ^ other.sign() {
      add_impl::<u32, i32>(self, other, mode)
    } else {
      sub_impl::<u32, i32>(self, other, mode)
    }
  }

  pub fn mul_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    mul_impl::<u32, i32, u64>(self, other, mode)
  }

  pub fn div_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    div_impl::<u32, i32, u64>(self, other, mode)
  }
}

impl Float<u64> {
    pub fn add_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        if self.sign() ^ other.sign() {
            sub_impl::<u64, i64>(self, other, mode)
        } else {
            add_impl::<u64, i64>(self, other, mode)
        }
    }

    pub fn sub_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        if self.sign() ^ other.sign() {
          add_impl::<u64, i64>(self, other, mode)
        } else {
          sub_impl::<u64, i64>(self, other, mode)
        }
      }
    
      pub fn mul_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        mul_impl::<u64, i64, u128>(self, other, mode)
      }
    
      pub fn div_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        div_impl::<u64, i64, u128>(self, other, mode)
      }
}

impl Float<u16> {
    pub fn add_with_mode(self, other: Float<u16>, mode: RoundingMode) -> (Float<u16>, Exception) {
        if self.sign() ^ other.sign() {
            sub_impl::<u16, i16>(self, other, mode)
        } else {
            add_impl::<u16, i16>(self, other, mode)
        }
    }

    pub fn sub_with_mode(self, other: Float<u16>, mode: RoundingMode) -> (Float<u16>, Exception) {
        if self.sign() ^ other.sign() {
          add_impl::<u16, i16>(self, other, mode)
        } else {
          sub_impl::<u16, i16>(self, other, mode)
        }
      }
    
      pub fn mul_with_mode(self, other: Float<u16>, mode: RoundingMode) -> (Float<u16>, Exception) {
        mul_impl::<u16, i16, u32>(self, other, mode)
      }
    
      pub fn div_with_mode(self, other: Float<u16>, mode: RoundingMode) -> (Float<u16>, Exception) {
        div_impl::<u16, i16, u32>(self, other, mode)
      }
}

impl From<f32> for Float<u32> {
    fn from(f: f32) -> Float<u32> {
        let u = unsafe { (f32u32 { f }).u };
        Float::<u32>::construct(u)
    }
}

impl From<u32> for Float<u32> {
    fn from(u: u32) -> Float<u32> {
        Float::from(u as f32)
    }
}

impl Into<f32> for Float<u32> {
    fn into(self) -> f32 {        
        let uni = f32u32 { u: self.v };
        unsafe { uni.f }
    }
}

impl From<f64> for Float<u64> {
    fn from(f: f64) -> Float<u64> {
        let u = unsafe { (f64u64 { f }).u };
        Float::<u64>::construct(u)
    }
}

impl From<u64> for Float<u64> {
    fn from(u: u64) -> Float<u64> {
        Float::from(u as f64)
    }
}

impl Into<f64> for Float<u64> {
    fn into(self) -> f64 {
        unsafe { (f64u64 { u: self.v }).f }
    }
}


trait FloatFormat: Clone + Copy {
    type BaseType:
    Clone +
    Copy +
    From<u8> +
    From<bool> +
    From<Self> +
    TryFrom<usize> +
    TryFrom<Self::ExtendedType> +
    LeadingZeros +
    BitWidth +
    Add<Output=Self::BaseType> +
    Sub<Output=Self::BaseType> +
    Mul<Output=Self::BaseType> +
    Div<Output=Self::BaseType> +
    Rem<Output=Self::BaseType> +
    Shr<Output=Self::BaseType> +
    Shl<Output=Self::BaseType> +
    BitAnd<Output=Self::BaseType> +
    BitOr<Output=Self::BaseType> +
    PartialEq +
    PartialOrd;

    type ExtendedType:
    Clone +
    Copy +
    From<u8> +
    From<bool> +
    From<Self::BaseType> +
    BitWidth +
    Add<Output=Self::ExtendedType> +
    Sub<Output=Self::ExtendedType> +
    Mul<Output=Self::ExtendedType> +
    Div<Output=Self::ExtendedType> +
    Rem<Output=Self::ExtendedType> +
    Shr<Output=Self::ExtendedType> +
    Shl<Output=Self::ExtendedType> +
    BitAnd<Output=Self::ExtendedType> +
    BitOr<Output=Self::ExtendedType> +
    PartialEq +
    PartialOrd;

    type SignedType:
    Clone +
    Copy +
    From<u8> +
    From<bool> +
    TryFrom<Self::BaseType> +
    Add<Output=Self::SignedType> +
    Sub<Output=Self::SignedType> +
    Mul<Output=Self::SignedType> +
    Div<Output=Self::SignedType> +
    Rem<Output=Self::SignedType> +
    Neg<Output=Self::SignedType> +
    Shr<Output=Self::SignedType> +
    Shl<Output=Self::SignedType> +
    BitAnd<Output=Self::SignedType> +
    BitOr<Output=Self::SignedType> +
    PartialEq +
    PartialOrd;

    fn sig_width() -> usize;
    fn exp_width() -> usize;
}

#[derive(Clone, Copy)]
struct Float16(u16);
#[derive(Clone, Copy)]
struct Float32(u32);
#[derive(Clone, Copy)]
struct Float64(u64);

impl FloatFormat for Float16 {
    type BaseType = u16;
    type ExtendedType = u32;
    type SignedType = i16;

    fn sig_width() -> usize { 10 }
    fn exp_width() -> usize { 5 }
}

impl From<Float16> for u16 {
    fn from(v: Float16) -> u16 {
        v.0
    }
}



#[cfg(test)]
mod test {
    extern crate regex;
    
    use crate::{Float, Exception, RoundingMode};
    use crate::constant::*;    
    use regex::Regex;
    use std::process::Command;
    use std::str::from_utf8;

    #[repr(C)]
    union f32u32{ f: f32, u: u32 }

    #[repr(C)]
    union f64u64{ f: f64, u: u64 }

    #[test]
    fn f32_add() -> std::io::Result<()> {
        f32_test_harness("f32_add", |a, b| a.add_with_mode(b, RoundingMode::NearEven))
    }

    #[test]    
    fn f32_sub() -> std::io::Result<()> {
        f32_test_harness("f32_sub", |a, b| a.sub_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f32_mul() -> std::io::Result<()> {
        f32_test_harness("f32_mul", |a, b| a.mul_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f32_div() -> std::io::Result<()> {
        f32_test_harness("f32_div", |a, b| a.div_with_mode(b, RoundingMode::NearEven))
    }    

    #[test]
    fn f64_add() -> std::io::Result<()> {
        f64_test_harness("f64_add", |a, b| a.add_with_mode(b, RoundingMode::NearEven))
    }

    #[test]    
    fn f64_sub() -> std::io::Result<()> {
        f64_test_harness("f64_sub", |a, b| a.sub_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f64_mul() -> std::io::Result<()> {
        f64_test_harness("f64_mul", |a, b| a.mul_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f64_div() -> std::io::Result<()> {
        f64_test_harness("f64_div", |a, b| a.div_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_add() -> std::io::Result<()> {
        f16_test_harness("f16_add", |a, b| a.add_with_mode(b, RoundingMode::NearEven))
    }

    #[test]    
    fn f16_sub() -> std::io::Result<()> {
        f16_test_harness("f16_sub", |a, b| a.sub_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_mul() -> std::io::Result<()> {
        f16_test_harness("f16_mul", |a, b| a.mul_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_div() -> std::io::Result<()> {
        f16_test_harness("f16_div", |a, b| a.div_with_mode(b, RoundingMode::NearEven))
    }

    fn f32_test_harness(function: &str, f: impl Fn(Float<u32>, Float<u32>) -> (Float<u32>, Exception)) -> std::io::Result<()> {
        let output = Command::new("testfloat_gen")
            .arg("-precision32")
            .arg(function)
            .output()?;
        assert_eq!(output.status.code(), Some(0));
        assert!(output.stderr.is_empty());
        let re = Regex::new(r"([0-9A-Fa-f]{8}) ([0-9A-Fa-f]{8}) ([0-9A-Fa-f]{8}) ([0-9A-Fa-f]{2})")
            .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())            
            .map(|caps| {
                let a = u32::from_str_radix(caps.get(1).unwrap().as_str(), 16).unwrap();
                let b = u32::from_str_radix(caps.get(2).unwrap().as_str(), 16).unwrap();
                let c = u32::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = Float::<u32>::construct(a);
                let fb = Float::<u32>::construct(b); 
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = f(fa, fb);
                let actual_result = unsafe { 
                    let u = f32u32 { f: actual_result.into() };
                    u.u
                };

                let actual = (actual_result, actual_exception.0);
                
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| e_result != a_result || e_exception != a_exception)
            .collect::<Vec<(u32, u32, (u32, u8), (u32, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>10}, {:>10}, {:>10}, {:>10}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }

    fn f64_test_harness(function: &str, f: impl Fn(Float<u64>, Float<u64>) -> (Float<u64>, Exception)) -> std::io::Result<()> {
        let output = Command::new("testfloat_gen")
            .arg("-precision64")
            .arg(function)
            .output()?;
        assert_eq!(output.status.code(), Some(0));
        assert!(output.stderr.is_empty());
        let re = Regex::new(r"([0-9A-Fa-f]{16}) ([0-9A-Fa-f]{16}) ([0-9A-Fa-f]{16}) ([0-9A-Fa-f]{2})")
            .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())            
            .map(|caps| {
                let a = u64::from_str_radix(caps.get(1).unwrap().as_str(), 16).unwrap();
                let b = u64::from_str_radix(caps.get(2).unwrap().as_str(), 16).unwrap();
                let c = u64::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = Float::<u64>::construct(a);
                let fb = Float::<u64>::construct(b); 
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = f(fa, fb);
                let actual_result = unsafe { 
                    let u = f64u64 { f: actual_result.into() };
                    u.u
                };

                let actual = (actual_result, actual_exception.0);
                
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| e_result != a_result || e_exception != a_exception)
            .collect::<Vec<(u64, u64, (u64, u8), (u64, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>18}, {:>18}, {:>18}, {:>18}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:016x}, 0x{:016x}, 0x{:016x}, 0x{:016x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }

    fn f16_test_harness(function: &str, f: impl Fn(Float<u16>, Float<u16>) -> (Float<u16>, Exception)) -> std::io::Result<()> {
        let output = Command::new("testfloat_gen")            
            .arg(function)
            .output()?;
        assert_eq!(output.status.code(), Some(0), "status code is not 0(actual is {:?}). you need to install testfloat", output.status.code());
        assert!(output.stderr.is_empty());
        let re = Regex::new(r"([0-9A-Fa-f]{4}) ([0-9A-Fa-f]{4}) ([0-9A-Fa-f]{4}) ([0-9A-Fa-f]{2})")
            .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())            
            .map(|caps| {
                let a = u16::from_str_radix(caps.get(1).unwrap().as_str(), 16).unwrap();
                let b = u16::from_str_radix(caps.get(2).unwrap().as_str(), 16).unwrap();
                let c = u16::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = Float::<u16>::construct(a);
                let fb = Float::<u16>::construct(b); 
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = f(fa, fb);
                let actual = (actual_result.v, actual_exception.0);
                
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| e_result != a_result || e_exception != a_exception)
            .collect::<Vec<(u16, u16, (u16, u8), (u16, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>6}, {:>6}, {:>6}, {:>6}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:04x}, 0x{:04x}, 0x{:04x}, 0x{:04x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }

    #[test]    
    fn f32_add_0xfffffffe_0xfffffffe() {
        let a = Float::construct(0xffff_fffe);
        let b = Float::construct(0xffff_fffe);

        a + b;
    }

    #[test]    
    fn f32_add_0xc00007ef_0x3dfff7bf() {
        let a = Float::construct(0xc00007ef);
        let b = Float::construct(0x3dfff7bf);

        a + b;
    }

    #[test]    
    fn f32_add_0xc07fffee_0x4fff0010() {
        let a = Float::construct(0xc07fffee);
        let b = Float::construct(0x4fff0010);

        a + b;
    }

    #[test]    
    fn f32_add_0x0027fffe_0x808006fe() {
        let a = Float::construct(0x0027fffe);
        let b = Float::construct(0x808006fe);

        a + b;
    }

    #[test]    
    fn f32_mul_0x3f55e76b_0x80800000() {
        let a = Float::construct(0x3f55_e76b);
        let b = Float::construct(0x8080_0000);

        a * b;
    }

    #[test]    
    fn f32_mul_0xc07fffee_0x4fff0010() {
        let a = Float::construct(0xc07f_ffee);
        let b = Float::construct(0x4fff_0010);

        a * b;
    }

    #[test]    
    fn f32_mul_0xc1f4718a_0x80000102() {
        let a = Float::construct(0xc1f4_718a);
        let b = Float::construct(0x8000_0102);

        a * b;
    }

    #[test]    
    fn f32_mul_0xbf7fffff_0x80800000() {
        let a = Float::construct(0xbf7f_ffff);
        let b = Float::construct(0x8080_0000);

        a * b;
    }

    #[test]
    fn f32_mul_0x0032c625_0x80e00004() {
        let a = Float::construct(0x0032_c625);
        let b = Float::construct(0x80e0_0004);

        a * b;
    }

    #[test]
    fn f32_div_0xc00007ef_0x3dfff7bf() {
        let a = Float::construct(0xc000_07ef);
        let b = Float::construct(0x3dff_f7bf);

        a / b;
    }

    #[test]
    fn f32_div_0xdea0000e_0x41ff8003() {
        let a = Float::construct(0xdea0_000e);
        let b = Float::construct(0x41ff_8003);

        a / b;
    }
}