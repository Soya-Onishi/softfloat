mod exception;
mod ops;
mod util;
mod constant;

use exception::Exception;
use std::cmp::PartialEq;
use constant::FloatFormat;
use crate::ops::{add::add_impl, sub::sub_impl, mul::mul_impl, div::div_impl};

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

impl Float<u32> {
  pub fn add_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    if self.sign() ^ other.sign() {
        sub_impl::<u32, i32, _, _>(self, other, mode)
      } else {
        add_impl::<u32, i32, _, _>(self, other, mode)
      }
  }

  pub fn sub_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    if self.sign() ^ other.sign() {
      add_impl::<u32, i32, _, _>(self, other, mode)
    } else {
      sub_impl::<u32, i32, _, _>(self, other, mode)
    }
  }

  pub fn mul_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    mul_impl::<u32, i32, u64>(self, other, mode)
  }

  pub fn div_with_mode(self, other: Float<u32>, mode: RoundingMode) -> (Float<u32>, Exception) {
    div_impl::<u32, i32, u64, _, _, _>(self, other, mode)
  }
}

impl Float<u64> {
    pub fn add_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        if self.sign() ^ other.sign() {
            sub_impl::<u64, i64, _, _>(self, other, mode)
        } else {
            add_impl::<u64, i64, _, _>(self, other, mode)
        }
    }

    pub fn sub_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        if self.sign() ^ other.sign() {
          add_impl::<u64, i64, _, _>(self, other, mode)
        } else {
          sub_impl::<u64, i64, _, _>(self, other, mode)
        }
      }
    
      pub fn mul_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        mul_impl::<u64, i64, u128>(self, other, mode)
      }
    
      pub fn div_with_mode(self, other: Float<u64>, mode: RoundingMode) -> (Float<u64>, Exception) {
        div_impl::<u64, i64, u128, _, _, _>(self, other, mode)
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