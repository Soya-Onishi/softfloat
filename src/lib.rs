mod exception;

extern crate either;
use either::Either;

use exception::*;

#[repr(C)]
union f32u32 {
    f: f32,
    u: u32,
}

#[derive(Clone, Copy)]
pub struct Float {
    sign: bool,
    exp: u32,
    sig: u32,
}

#[derive(Clone, Copy, PartialEq)]
pub enum RoundingMode {
    NearEven,
    NearMax,
    Zero,
    Max,
    Min,
}

impl Float {
    pub fn default_nan() -> Float {
        Float {
            sign: true,
            exp: 0xFF,
            sig: 0x0040_0000,
        }
    }

    pub fn zero(sign: bool) -> Float {
        Float {
            sign,
            exp: 0x00,
            sig: 0x00,
        }
    }

    pub fn infinite(sign: bool) -> Float {
        Float {
            sign,
            exp: 0xFF,
            sig: 0x00,
        }
    }

    fn is_nan(&self) -> bool {
        self.exp == 0xFF && self.sig != 0
    }

    fn is_signal_nan(&self) -> bool {
        self.exp == 0xFF && (self.sig & 0x003F_FFFF != 0) && (self.sig & 0x0040_0000 == 0)
    }

    fn is_inf(&self) -> bool {
        self.exp == 0xFF && self.sig == 0
    }

    fn is_zero(&self) -> bool {
        (self.exp | self.sig) == 0
    }

    fn propagate_nan(a: Float, b: Float) -> (Float, Exception) {
        let a_sig_qnan = Float {
            sign: a.sign,
            exp: a.exp,
            sig: a.sig | 0x0040_0000,
        };
        let b_sig_qnan = Float {
            sign: b.sign,
            exp: b.exp,
            sig: b.sig | 0x0040_0000,
        };

        let nan = if a.is_nan() { a_sig_qnan } else { b_sig_qnan };

        let exception = if a.is_signal_nan() | b.is_signal_nan() {
            Exception(EXCEPTION_INVALID)
        } else {
            Exception(EXCEPTION_NONE)
        };

        (nan, exception)
    }

    pub fn new(value: u32) -> Float {
        let sign = (value & 0x8000_0000) != 0;
        let exp = (value & 0x7FFF_FFFF) >> 23;
        let sig = value & 0x007F_FFFF;

        Float { sign, exp, sig }
    }

    pub fn add(self, other: Float) -> (Float, Exception) {
        self.add_with_mode(other, RoundingMode::NearEven)
    }

    pub fn sub(self, other: Float) -> (Float, Exception) {
        self.sub_with_mode(other, RoundingMode::NearEven)
    }

    pub fn mul(self, other: Float) -> (Float, Exception) {
        self.mul_with_mode(other, RoundingMode::NearEven)
    }

    pub fn div(self, other: Float) -> (Float, Exception) {
        self.div_with_mode(other, RoundingMode::NearEven)
    }

    pub fn add_with_mode(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        if self.sign ^ other.sign {
            self.sub_impl(other, mode)
        } else {
            self.add_impl(other, mode)
        }
    }

    pub fn sub_with_mode(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        if self.sign ^ other.sign {
            self.add_impl(other, mode)
        } else {
            self.sub_impl(other, mode)
        }
    }

    pub fn div_with_mode(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        self.div_impl(other, mode)
    }

    fn add_impl(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        let (exp_diff, fa, fb) = if self.exp < other.exp {
            (other.exp - self.exp, other, self)
        } else {
            (self.exp - other.exp, self, other)
        };
        let sign = self.sign;

        let sig_a_hidden = if fa.exp == 0 { 0 } else { HIDDEN_SIGNIFICAND };
        let sig_b_hidden = if fb.exp == 0 { 0 } else { HIDDEN_SIGNIFICAND };
        let sig_a = if fa.exp == 0 {
            fa.sig << (ROUND_WIDTH + 1)
        } else {
            (sig_a_hidden | fa.sig) << ROUND_WIDTH
        };
        let sig_b = if fb.exp == 0 {
            fb.sig << (ROUND_WIDTH + 1)
        } else {
            (sig_b_hidden | fb.sig) << ROUND_WIDTH
        };

        if fa.exp == 0 && fb.exp == 0 {
            let fc_sig = fa.sig + fb.sig;
            let exp = fc_sig >> 23;
            let sig = fc_sig & 0x007F_FFFF;

            return (
                Float {
                    sign: self.sign,
                    exp,
                    sig,
                },
                Exception(EXCEPTION_NONE),
            );
        }

        if self.exp == 0xFF || other.exp == 0xFF {
            let result = if (self.exp == 0xFF && self.sig != 0) || (other.exp == 0xFF && other.sig != 0) {
                Float::propagate_nan(self, other)
            } else if self.exp == 0xFF {
                (self, Exception(EXCEPTION_NONE))
            } else {
                (Float::infinite(sign), Exception(EXCEPTION_NONE))
            };

            return result;
        }
        let exp = fa.exp;
        let sig_b = Float::right_shift_32(sig_b, exp_diff);
        let sig = sig_a + sig_b;

        Float::pack_float32(sign, exp as i32, sig, mode)
    }

    fn sub_impl(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        let (exp_diff, fa, fb, sign): (_, _, _, bool) = if self.exp < other.exp {
            (other.exp - self.exp, other, self, !self.sign)
        } else if self.exp > other.exp {
            (self.exp - other.exp, self, other, self.sign)
        } else {
            if self.sig < other.sig {
                (0, other, self, !self.sign)
            } else {
                (0, self, other, self.sign)
            }
        };
        let sig_a = if fa.exp == 0 { fa.sig << 1 } else { fa.sig | HIDDEN_SIGNIFICAND };
        let sig_b = if fb.exp == 0 { fb.sig << 1 } else { fb.sig | HIDDEN_SIGNIFICAND };
        let sig_a = sig_a << ROUND_WIDTH;
        let sig_b = sig_b << ROUND_WIDTH;
        let sig_b = Float::right_shift_32(sig_b, exp_diff);

        // there is no need to calculate fb.exp == 0xFF
        // because fa.exp >= fb.exp must be true.
        if fa.exp == 0xFF || fb.exp == 0xFF {
            if fa.is_inf() && fb.is_inf() {
                return (Float::default_nan(), Exception(EXCEPTION_INVALID))
            }

            if fa.is_nan() || fb.is_nan() {
                return Float::propagate_nan(self, other);
            }

            return (Float::infinite(sign), Exception(EXCEPTION_NONE));
        }
        let sig = sig_a - sig_b;

        if sig == 0 {
            return (
                Float::zero(mode == RoundingMode::Min),
                Exception(EXCEPTION_NONE),
            );
        }
        let (exp, sig) = Float::normalize_subnormal_f32(sig, 5);
        let exp = fa.exp as i32 + exp;

        Float::pack_float32(sign, exp, sig, mode)
    }

    pub fn mul_with_mode(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        // TODO: implement subnormal pattern

        let try_mul_inf_or_nan = |sign: bool, f: Float, g: Float| -> Option<(Float, Exception)> {        
            if f.is_inf() && !g.is_nan() { 
                if g.is_zero() { Some((Float::default_nan(), Exception(EXCEPTION_INVALID))) }
                else { Some((Float::infinite(sign), Exception(EXCEPTION_NONE)))}
            } else if f.is_nan() || g.is_nan() {
                Some(Float::propagate_nan(f, g))
            } else {
                None
            }
        };

        let make_exp_sig = |sign: bool, f: Float| -> Either<(Float, Exception), (i32, u32)> {
            if f.exp != 0 {
                Either::Right((f.exp as i32, f.sig | HIDDEN_SIGNIFICAND))
            } else if f.sig == 0 {
                Either::Left((Float::zero(sign), Exception(EXCEPTION_NONE)))
            } else {
                // 1.0 * (2 ^ -127) cannot be represented as normal 
                // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
                // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
                // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
                let shamt = f.sig.leading_zeros() - 8;
                Either::Right((-(shamt as i32) + 1, f.sig << shamt))
            }
        };

        let sign = self.sign ^ other.sign;

        match try_mul_inf_or_nan(sign, self, other) {
            Some(f) => return f,
            None => {}
        }

        match try_mul_inf_or_nan(sign, other, self) {
            Some(f) => return f,
            None => {}
        }

        // zero multiplication derive zero
        let (exp_a, sig_a) = match make_exp_sig(sign, self) {
            Either::Left(zero) => return zero,
            Either::Right(pair) => pair,
        };

        let (exp_b, sig_b) = match make_exp_sig(sign, other) {
            Either::Left(zero) => return zero,
            Either::Right(pair) => pair,
        };

    
        let (exp, sig) = {
            let mul_result = sig_a as u64 * sig_b as u64;

            /*
              Why SIG_WIDTH_F32 - ROUND_WITH ? not (SIG_WIDTH_F32 + 1) - ROUND_WITH ?
              
                  1.1
              *   1.1
              -------
                  1 1
                1 1
             --------
              1 0 0 1 ==> 10.01

              That's why exp need to be added by 1 in pack_float32.
              So, shifted must be 28 bit width.
            */
                    
            
            let exp = exp_a + exp_b - BIAS_F32; 
            let shifted =
                Float::right_shift_64(mul_result, (SIG_WIDTH_F32 - ROUND_WIDTH) as u64) as u32;            
            
            (exp, shifted)
        };

        Float::pack_float32(sign, exp, sig, mode)
    }

    fn div_impl(self, other: Float, mode: RoundingMode) -> (Float, Exception) {
        let sign = self.sign ^ other.sign;

        if self.is_nan() || other.is_nan() { return Float::propagate_nan(self, other); }
        if self.is_inf() { 
            if other.is_inf() { return (Float::default_nan(), Exception(EXCEPTION_INVALID)); }
            else { return (Float::infinite(sign), Exception(EXCEPTION_NONE)); }
        }        
        if other.is_inf() { return (Float::zero(sign), Exception(EXCEPTION_NONE)); }
        if other.is_zero() { 
            if self.is_zero() { return (Float::default_nan(), Exception(EXCEPTION_INVALID)); }
            else { return (Float::infinite(sign), Exception(EXCEPTION_INFINITE)); }
        }

        // This is copy of make_exp_sig in mul_impl
        let make_exp_sig = |sign: bool, f: Float| -> Either<(Float, Exception), (i32, u32)> {
            if f.exp != 0 {
                Either::Right((f.exp as i32, f.sig | HIDDEN_SIGNIFICAND))
            } else if f.sig == 0 {
                Either::Left((Float::zero(sign), Exception(EXCEPTION_NONE)))
            } else {
                // 1.0 * (2 ^ -127) cannot be represented as normal 
                // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
                // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
                // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
                let shamt = f.sig.leading_zeros() - 8;
                Either::Right((-(shamt as i32) + 1, f.sig << shamt))
            }
        };

        let (exp_a, sig_a) = match make_exp_sig(sign, self) {
            Either::Left(pair) => return pair,
            Either::Right(pair) => pair,
        };

        let (exp_b, sig_b) = match make_exp_sig(sign, other) {
            Either::Left(pair) => return pair,
            Either::Right(pair) => pair,
        };

        let dividend = (sig_a as u64) << (23_u64 + ROUND_WIDTH as u64);
        let divisor = sig_b as u64;
        let sig = (dividend / divisor) as u32;
        let rem = ((dividend % divisor) != 0) as u32;
        let (mod_exp, sig) = Float::normalize_subnormal_f32(sig, 5);
        let exp = exp_a - exp_b + mod_exp + BIAS_F32;

        Float::pack_float32(sign, exp, sig | rem, mode)
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
                Exception(EXCEPTION_UNDERFLOW)
            } else {
                Exception(EXCEPTION_NONE)
            };
            underflow | Exception(EXCEPTION_INEXACT)
        } else {
            Exception(EXCEPTION_NONE)
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

                either::Left((infinite, Exception(EXCEPTION_OVERFLOW | EXCEPTION_INEXACT)))
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
    
    use crate::Float;
    use crate::Exception;
    use regex::Regex;
    use std::process::Command;
    use std::str::from_utf8;

    #[repr(C)]
    union f32u32{ f: f32, u: u32 }

    #[test]
    fn f32_add() -> std::io::Result<()> {
        f32_test_harness("f32_add", |a, b| a.add(b))
    }

    #[test]    
    fn f32_sub() -> std::io::Result<()> {
        f32_test_harness("f32_sub", |a, b| a.sub(b))
    }

    #[test]
    fn f32_mul() -> std::io::Result<()> {
        f32_test_harness("f32_mul", |a, b| a.mul(b))
    }

    #[test]
    fn f32_div() -> std::io::Result<()> {
        f32_test_harness("f32_div", |a, b| a.div(b))
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

        a.mul(b);
    }

    #[test]    
    fn f32_mul_0xc1f4718a_0x80000102() {
        let a = Float::new(0xc1f4_718a);
        let b = Float::new(0x8000_0102);

        a.mul(b);
    }

    #[test]    
    fn f32_mul_0xbf7fffff_0x80800000() {
        let a = Float::new(0xbf7f_ffff);
        let b = Float::new(0x8080_0000);

        a.mul(b);
    }

    #[test]
    fn f32_mul_0x0032c625_0x80e00004() {
        let a = Float::new(0x0032_c625);
        let b = Float::new(0x80e0_0004);

        a.mul(b);
    }

    #[test]
    fn f32_div_0xc00007ef_0x3dfff7bf() {
        let a = Float::new(0xc000_07ef);
        let b = Float::new(0x3dff_f7bf);

        a.div(b);
    }

    #[test]
    fn f32_div_0xdea0000e_0x41ff8003() {
        let a = Float::new(0xdea0_000e);
        let b = Float::new(0x41ff_8003);

        a.div(b);
    }
}

const BIAS_F32: i32 = 127;
const SIG_WIDTH_F32: u32 = 23;
const SIG_MASK_F32: u32 = (1 << SIG_WIDTH_F32) - 1;
const HIDDEN_SIGNIFICAND: u32 = 0x0080_0000;
const ROUND_WIDTH: u32 = 3;
const ROUND_MASK: u32 = (1 << ROUND_WIDTH) - 1;
