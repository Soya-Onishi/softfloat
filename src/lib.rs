mod exception;
mod ops;
mod constant;
mod constructor;
mod getter;
mod format;

extern crate either;
use either::Either;

use exception::Exception;
use constant::FloatConstant;
use constructor::FloatConstructor;
use getter::FloatGetter;
use format::FloatFormat;

#[repr(C)]
union f32u32 {
    f: f32,
    u: u32,
}

/* #[derive(Clone, Copy)]
pub struct Float<T> where T: Clone + Copy {
    sign: bool,
    exp: u32,
    sig: u32,
} */


#[derive(Clone, Copy, PartialEq)]
pub enum RoundingMode {
    NearEven,
    NearMax,
    Zero,
    Max,
    Min,
}

struct Float<T> { v: T }

impl<T> Float<T> where T: FloatConstant<T> + FloatConstructor<T> + FloatGetter<T> + FloatFormat {
    fn propagate_nan(a: Float<T>, b: Float<T>) -> (Float<T>, Exception) {
        let make_qnan = |f: Float<T>| {
            Float::<T>::constructor(f.sign(), f.exp(), f.sig() | Float::<T>::signal_bit());
        };

        let a_sig_qnan = make_qnan(a);
        let b_sig_qnan = make_qnan(b);    

        let nan = if a.is_nan() { a_sig_qnan } else { b_sig_qnan };

        let exception = if a.is_signal_nan() | b.is_signal_nan() {
            Exception::invalid()
        } else {
            Exception::none()
        };

        (nan, exception)
    }

    fn pack_float32(sign: bool, exp: i32, sig: u32, mode: RoundingMode) -> (Float, Exception) {
        let is_near_even = mode == RoundingMode::NearEven;
        let round_inc = match mode {
            RoundingMode::NearEven => 0x04,
            RoundingMode::NearMax => 0x04,
            RoundingMode::Zero => 0x00,
            RoundingMode::Max => {
                if sign {
                    0x00
                } else {
                    0x07
                }
            }
            RoundingMode::Min => {
                if sign {
                    0x07
                } else {
                    0x00
                }
            }
        };

        let (exp, sig, is_underflow) = if exp <= 0 {
            let is_tiny = (sig + round_inc < 0x0800_0000) || exp < 0;
            let shamt = -exp as u32 + 1;
            let sig = Float::right_shift_32(sig, shamt);
            let round_bits = sig & ROUND_MASK != 0;

            (0, sig, is_tiny && round_bits)
        } else {
            (exp as u32, sig, false)
        };

        let (exp, sig) = match Float::normalize(sign, exp, sig, round_inc) {
            either::Left(result) => return result,
            either::Right(pair) => pair,
        };

        let round_bits = sig & ROUND_MASK;
        let sig = sig + round_inc;

        let (exp, sig) = match Float::normalize(sign, exp, sig, round_inc) {
            either::Left(result) => return result,
            either::Right(pair) => pair,
        };

        let exception = if round_bits != 0 {
            let underflow = if is_underflow {
                Exception::underflow()
            } else {
                Exception::none()
            };
            underflow | Exception::inexact()
        } else {
            Exception::none()
        };

        let sig =
            (sig >> ROUND_WIDTH) & !((round_bits == 0x04 && is_near_even) as u32) & SIG_MASK_F32;

        (Float { sign, exp, sig }, exception)
    }

    fn normalize(
        sign: bool,
        exp: u32,
        sig: u32,
        round_inc: u32,
    ) -> Either<(Float, Exception), (u32, u32)> {
        match (exp, sig) {
            (exp, _) if exp >= 0xFF => {
                let infinite = if round_inc == 0 {
                    Float {
                        sign,
                        exp: 0xFE,
                        sig: 0x007F_FFFF,
                    }
                } else {
                    Float {
                        sign,
                        exp: 0xFF,
                        sig: 0x0000_0000,
                    }
                };

                either::Left((infinite, Exception::overflow() | Exception::inexact()))
            }
            (exp, sig) if sig >= 0x0800_0000 => {
                Float::normalize(sign, exp + 1, Float::right_shift_32(sig, 1), round_inc)
            }
            // if exp == 0 && sig >= 0x0400_0000 means 10.0 * 2^-127 => 1.0 * 2^-126
            (0x00, sig) if sig >= 0x0400_0000 => Float::normalize(sign, 1, sig, round_inc),
            (exp, sig) => either::Right((exp, sig)),
        }
    }

    fn right_shift_32(sig: u32, shamt: u32) -> u32 {
        if shamt > 31 {
            (sig != 0) as u32
        } else {
            (sig >> shamt) | (((sig & ((1 << shamt) - 1)) != 0) as u32)
        }
    }

    fn right_shift_64(sig: u64, shamt: u64) -> u64 {
        if shamt > 63 {
            (sig != 0) as u64
        } else {
            (sig >> shamt) | (((sig & ((1 << shamt) - 1)) != 0) as u64)
        }
    }

    fn normalize_subnormal_f32(sig: u32, bias: u32) -> (i32, u32) {
        let shamt = sig.leading_zeros() - bias;
        (-(shamt as i32), sig << shamt)
    }
}

impl From<f32> for Float {
    fn from(f: f32) -> Float {
        let u = unsafe { (f32u32 { f }).u };
        Float::from(u)
    }
}

impl From<u32> for Float {
    fn from(u: u32) -> Float {
        Float::from(u as f32)
    }
}

impl Into<f32> for Float {
    fn into(self) -> f32 {
        let sign = (self.sign as u32) << 31;
        let exp = self.exp << 23;
        let sig = self.sig;

        let uni = f32u32 {
            u: sign | exp | sig,
        };
        unsafe { uni.f }
    }
}

#[cfg(test)]
mod test {
    extern crate regex;
    
    use crate::{Float, Exception, RoundingMode};
    use regex::Regex;
    use std::process::Command;
    use std::str::from_utf8;

    #[repr(C)]
    union f32u32{ f: f32, u: u32 }

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

    fn f32_test_harness(function: &str, f: impl Fn(Float, Float) -> (Float, Exception)) -> std::io::Result<()> {
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
                let fa = Float::new(a);
                let fb = Float::new(b);                
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

    #[test]    
    fn f32_mul_0xc07fffee_0x4fff0010() {
        let a = Float::new(0xc07f_ffee);
        let b = Float::new(0x4fff_0010);

        a * b;
    }

    #[test]    
    fn f32_mul_0xc1f4718a_0x80000102() {
        let a = Float::new(0xc1f4_718a);
        let b = Float::new(0x8000_0102);

        a * b;
    }

    #[test]    
    fn f32_mul_0xbf7fffff_0x80800000() {
        let a = Float::new(0xbf7f_ffff);
        let b = Float::new(0x8080_0000);

        a * b;
    }

    #[test]
    fn f32_mul_0x0032c625_0x80e00004() {
        let a = Float::new(0x0032_c625);
        let b = Float::new(0x80e0_0004);

        a * b;
    }

    #[test]
    fn f32_div_0xc00007ef_0x3dfff7bf() {
        let a = Float::new(0xc000_07ef);
        let b = Float::new(0x3dff_f7bf);

        a / b;
    }

    #[test]
    fn f32_div_0xdea0000e_0x41ff8003() {
        let a = Float::new(0xdea0_000e);
        let b = Float::new(0x41ff_8003);

        a / b;
    }
}

const BIAS_F32: i32 = 127;
const SIG_WIDTH_F32: u32 = 23;
const SIG_MASK_F32: u32 = (1 << SIG_WIDTH_F32) - 1;
const HIDDEN_SIGNIFICAND: u32 = 0x0080_0000;
const ROUND_WIDTH: u32 = 3;
const ROUND_MASK: u32 = (1 << ROUND_WIDTH) - 1;