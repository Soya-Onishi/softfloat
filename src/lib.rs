extern crate either;
use either::Either;

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

    fn propagate_nan(a: Float, b: Float) -> Float {
        if a.is_nan() {
            Float {
                sign: a.sign,
                exp: a.exp,
                sig: a.sig | 0x0040_0000,
            }
        } else {
            Float {
                sign: b.sign,
                exp: b.exp,
                sig: b.sig | 0x0040_0000,
            }
        }
    }

    pub fn new(value: u32) -> Float {
        let sign = (value & 0x8000_0000) != 0;
        let exp = (value & 0x7FFF_FFFF) >> 23;
        let sig = value & 0x007F_FFFF;

        Float { sign, exp, sig }
    }

    pub fn add(self, other: Float) -> Float {
        self.add_with_mode(other, RoundingMode::NearEven)
    }

    pub fn sub(self, other: Float) -> Float {
        self.sub_with_mode(other, RoundingMode::NearEven)
    }

    pub fn mul(self, other: Float) -> Float {
        self.mul_with_mode(other, RoundingMode::NearEven)
    }

    pub fn add_with_mode(self, other: Float, mode: RoundingMode) -> Float {
        if self.sign ^ other.sign {
            self.sub_impl(other, mode)
        } else {
            self.add_impl(other, mode)
        }
    }

    pub fn sub_with_mode(self, other: Float, mode: RoundingMode) -> Float {
        if self.sign ^ other.sign {
            self.add_impl(other, mode)
        } else {
            self.sub_impl(other, mode)
        }
    }

    fn add_impl(self, other: Float, mode: RoundingMode) -> Float {
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
        let sig_b = Float::right_shift_32(sig_b, exp_diff);

        if fa.exp == 0 && fb.exp == 0 {
            let fc_sig = fa.sig + fb.sig;
            let exp = fc_sig >> 23;
            let sig = fc_sig & 0x007F_FFFF;

            return Float {
                sign: self.sign,
                exp,
                sig,
            };
        }

        if fa.exp == 0xFF && fb.exp == 0xFF {
            if fa.sig != 0 || fb.sig != 0 {
                return Float::propagate_nan(self, other);
            }
            return self;
        }

        if fa.exp == 0xFF {
            if fa.sig != 0 {
                return Float::propagate_nan(self, other);
            }
            return Float {
                sign,
                exp: 0xFF,
                sig: 0x0000_0000,
            };
        }

        let exp = fa.exp;
        let sig = sig_a + sig_b;

        Float::pack_float32(sign, exp as i32, sig, mode)
    }

    fn sub_impl(self, other: Float, mode: RoundingMode) -> Float {
        let (exp_diff, fa, fb, subtract): (_, _, _, Box<dyn Fn(i32, i32) -> i32>) =
            if self.exp < other.exp {
                (
                    other.exp - self.exp,
                    other,
                    self,
                    Box::new(|a: i32, b: i32| b - a),
                )
            } else {
                (
                    self.exp - other.exp,
                    self,
                    other,
                    Box::new(|a: i32, b: i32| a - b),
                )
            };
        let sig_a_hidden = if fa.exp == 0 { 0 } else { HIDDEN_SIGNIFICAND };
        let sig_b_hidden = if fb.exp == 0 { 0 } else { HIDDEN_SIGNIFICAND };
        let sig_a = (sig_a_hidden | fa.sig) << ROUND_WIDTH;
        let sig_b = (sig_b_hidden | fb.sig) << ROUND_WIDTH;
        let sig_b = Float::right_shift_32(sig_b, exp_diff);

        if fa.exp == 0xFF && fb.exp == 0xFF {
            if fa.sig != 0 || fb.sig != 0 {
                return Float::propagate_nan(self, other);
            }
            return Float::default_nan();
        }
        let sig = subtract(sig_a as i32, sig_b as i32);

        if sig == 0 {
            return Float::zero(mode == RoundingMode::Min);
        }

        let sign = if sig > 0 { self.sign } else { !self.sign };
        let (exp, sig) = Float::normalize_subnormal_f32(sig as u32);
        let exp = fa.exp as i32 + exp;

        Float::pack_float32(sign, exp, sig, mode)
    }

    pub fn mul_with_mode(self, other: Float, mode: RoundingMode) -> Float {
        // TODO: implement subnormal pattern

        let try_mul_inf_or_nan = |sign: bool, f: Float, g: Float| -> Option<Float> {
            if f.exp != 0xFF {
                return None;
            };
            if f.sig != 0 {
                return Some(Float::propagate_nan(f, g));
            }

            if g.exp | g.sig == 0 {
                Some(Float::default_nan())
            } else {
                Some(Float::infinite(sign))
            }
        };

        let make_exp_sig = |sign: bool, f: Float| -> Either<Float, (i32, u32)> {
            if f.exp != 0 {
                Either::Right((f.exp as i32, f.sig | HIDDEN_SIGNIFICAND))
            } else if f.sig == 0 {
                Either::Left(Float::zero(sign))
            } else {
                Either::Right(Float::normalize_subnormal_f32(f.sig))
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

        let exp = exp_a + exp_b - BIAS_F32;
        let sig = {
            let mul_result = sig_a as u64 * sig_b as u64;
            let shifted =
                Float::right_shift_64(mul_result, (SIG_WIDTH_F32 - ROUND_WIDTH) as u64) as u32;
            shifted
        };

        Float::pack_float32(sign, exp, sig, mode)
    }

    fn pack_float32(sign: bool, exp: i32, sig: u32, mode: RoundingMode) -> Float {
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

        let (exp, sig) = if exp <= 0 {
            let shamt = -exp as u32 + 1;
            let sig = Float::right_shift_32(sig, shamt);

            (0, sig)
        } else {
            (exp as u32, sig)
        };
        let (exp, sig) = match Float::normalize(sign, exp, sig, round_inc) {
            either::Left(float) => return float,
            either::Right(pair) => pair,
        };

        let round_bits = sig & ROUND_MASK;
        let sig = sig + round_inc;

        let (exp, sig) = match Float::normalize(sign, exp, sig, round_inc) {
            either::Left(float) => return float,
            either::Right(pair) => pair,
        };

        let sig =
            (sig >> ROUND_WIDTH) & !((round_bits == 0x04 && is_near_even) as u32) & SIG_MASK_F32;
        Float { sign, exp, sig }
    }

    fn normalize(sign: bool, exp: u32, sig: u32, round_inc: u32) -> Either<Float, (u32, u32)> {
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

                either::Left(infinite)
            }
            (exp, sig) if sig >= 0x0800_0000 => {
                Float::normalize(sign, exp + 1, Float::right_shift_32(sig, 1), round_inc)
            }
            (0x00, sig) if sig >= 0x0400_0000 => Float::normalize(sign, exp + 1, sig, round_inc),
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

    fn normalize_subnormal_f32(sig: u32) -> (i32, u32) {
        let shamt = sig.leading_zeros() - 8;

        // When floating point is 1.xx, exp must be 1 or more.
        // This means if sig.leading_zeros() - 8 == 0, exp must be 1.
        // That is why, left side of tuple must be 1 - shamt, not -shamt.
        (1 - shamt as i32, sig << shamt)
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
    extern crate rand;

    use crate::{f32u32, Float};
    use rand::{rngs::StdRng, Rng, SeedableRng};

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct float32_t {
        v: u32,
    }

    impl Into<f32> for float32_t {
        fn into(self) -> f32 {
            let uni = f32u32 { u: self.v };
            unsafe { uni.f }
        }
    }

    impl From<f32> for float32_t {
        fn from(f: f32) -> float32_t {
            let uni = f32u32 { f };
            float32_t {
                v: unsafe { uni.u },
            }
        }
    }

    impl From<u32> for float32_t {
        fn from(u: u32) -> float32_t {
            float32_t { v: u }
        }
    }

    #[link(name = "softfloat")]
    extern "C" {
        fn f32_mul(a: float32_t, b: float32_t) -> float32_t;
        fn f32_add(a: float32_t, b: float32_t) -> float32_t;
    }

    #[test]
    fn mult_with_softfloat() {
        let mut rng: StdRng = SeedableRng::seed_from_u64(0);
        let mut is_failure = false;

        for _ in 0..10000000 {
            let a: u32 = rng.gen();
            let b: u32 = rng.gen();

            let (c_soft, c_float) = mult(a, b);

            if c_soft != c_float {
                if !is_failure {
                    println!(
                        "{:>10}, {:>10}, {:>10}, {:>10}",
                        "a", "b", "expect", "actual"
                    );
                }

                is_failure = true;

                println!(
                    "0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x}",
                    a, b, c_soft, c_float
                );
            }
        }
        assert!(!is_failure, "test is failure");
    }

    #[test]
    fn mult_0x90ba4d56_0xaf16e52d() {
        mult_once(0x90ba4d56, 0xaf16e52d)
    }
    #[test]
    fn mult_0xf942bc00_0x993de000() {
        mult_once(0xf942bc00, 0x993de000);
    }

    #[test]
    fn mult_0x6ecab647_0x8c400000() {
        mult_once(0x6eca_b647, 0x8c40_0000)
    }

    #[test]
    fn mult_0x03698281_0x7c15b40f() {
        mult_once(0x03698281, 0x7c15b40f)
    }

    #[test]
    fn add_with_softfloat() {
        let mut rng: StdRng = SeedableRng::seed_from_u64(0);
        let mut is_failure = false;

        for _ in 0..10000000 {
            let a: u32 = rng.gen();
            let b: u32 = rng.gen();

            let (c_soft, c_float) = add(a, b);

            if c_soft != c_float {
                if !is_failure {
                    println!(
                        "{:>10}, {:>10}, {:>10}, {:>10}",
                        "a", "b", "expect", "actual"
                    );
                }

                is_failure = true;

                println!(
                    "0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x}",
                    a & 0x7FFF_FFFF,
                    b & 0x7FFF_FFFF,
                    c_soft,
                    c_float
                );
            }
        }
        assert!(!is_failure, "test is failure");
    }

    #[test]
    fn add_0x000c90f2_0x01997658() {
        add_once(0x000c90f2, 0x01997658);
    }

    #[test]
    fn add_0x0024a100_0x007c7c6f() {
        add_once(0x0024a100, 0x007c7c6f);
    }

    fn mult(a: u32, b: u32) -> (u32, u32) {
        let a_float = Float::new(a);
        let b_float = Float::new(b);
        let a_soft = float32_t::from(a);
        let b_soft = float32_t::from(b);

        let c_float = a_float.mul(b_float);
        let c_soft = unsafe { f32_mul(a_soft, b_soft) };

        let c_soft: u32 = unsafe { (f32u32 { f: c_soft.into() }).u };
        let c_float: u32 = unsafe { (f32u32 { f: c_float.into() }).u };

        (c_soft, c_float)
    }

    fn mult_once(a: u32, b: u32) {
        let (c_soft, c_float) = mult(a, b);
        assert_eq!(c_soft, c_float, "0x{:08x}, 0x{:08x}", c_soft, c_float);
    }

    fn add(a: u32, b: u32) -> (u32, u32) {
        let a = a & 0x7FFF_FFFF;
        let b = b & 0x7FFF_FFFF;

        let a_float = Float::new(a);
        let b_float = Float::new(b);
        let a_soft = float32_t::from(a);
        let b_soft = float32_t::from(b);

        let c_float = a_float.add(b_float);
        let c_soft = unsafe { f32_add(a_soft, b_soft) };

        let c_soft: u32 = unsafe { (f32u32 { f: c_soft.into() }).u };
        let c_float: u32 = unsafe { (f32u32 { f: c_float.into() }).u };

        (c_soft, c_float)
    }

    fn add_once(a: u32, b: u32) {
        let (c_soft, c_float) = add(a, b);
        assert_eq!(c_soft, c_float, "0x{:08x}, 0x{:08x}", c_soft, c_float);
    }
}

const BIAS_F32: i32 = 127;
const SIG_WIDTH_F32: u32 = 23;
const SIG_MASK_F32: u32 = (1 << SIG_WIDTH_F32) - 1;
const HIDDEN_SIGNIFICAND: u32 = 0x0080_0000;
const ROUND_WIDTH: u32 = 3;
const ROUND_MASK: u32 = (1 << ROUND_WIDTH) - 1;
