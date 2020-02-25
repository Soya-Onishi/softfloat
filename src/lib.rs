extern crate either;
use either::Either;

#[repr(C)] union f32u32 { f: f32, u: u32 }

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
        Float { sign: true, exp: 0xFF, sig: 0x0040_0000 }
    }

    pub fn zero(sign: bool) -> Float {
        Float { sign, exp: 0x00, sig: 0x00 }
    }

    pub fn infinite(sign: bool) -> Float {
        Float { sign, exp: 0xFF, sig: 0x00 }
    }

    fn is_nan(&self) -> bool {
        self.exp == 0xFF && self.sig != 0
    }

    fn is_signal_nan(&self) -> bool {
        self.exp == 0xFF && (self.sig & 0x003F_FFFF != 0) && (self.sig & 0x0040_0000 == 0)
    }

    fn propagate_nan(a: Float, b: Float) -> Float {
        if a.is_nan() { Float { sign: a.sign, exp: a.exp, sig: a.sig | 0x0040_0000 } }
        else          { Float { sign: b.sign, exp: b.exp, sig: b.sig | 0x0040_0000 } }
    }

    pub fn new(value: u32) -> Float {
        let sign = (value & 0x8000_0000) != 0;
        let exp = (value & 0x7FFF_FFFF) >> 23;
        let sig = value & 0x007F_FFFF;

        Float { sign, exp, sig }
    }

    pub fn mul(self, other: Float) -> Float {
        self.mul_with_mode(other, RoundingMode::NearEven)
    }

    pub fn mul_with_mode(self, other: Float, mode: RoundingMode) -> Float {
        // TODO: implement subnormal pattern

        let try_mul_inf_or_nan = |sign: bool, f: Float, g: Float| -> Option<Float> {
            if f.exp != 0xFF { return None };
            if f.sig != 0 { return Some(Float::propagate_nan(f, g)) }

            if g.exp | g.sig == 0 { Some(Float::default_nan()) }
            else                  { Some(Float::infinite(sign)) }
        };

        let make_exp_sig = |sign: bool, f: Float| -> Either<Float, (i32, u32)> {
            if f.exp != 0      { Either::Right((f.exp as i32, f.sig | HIDDEN_SIGNIFICAND)) }
            else if f.sig == 0 { Either::Left(Float::zero(sign)) }
            else               { Either::Right(Float::normalize_subnormal_f32(f.sig)) }
        };

        let sign = self.sign ^ other.sign;

        match try_mul_inf_or_nan(sign, self, other) {
            Some(f) => return f,
            None           => {}
        }

        match try_mul_inf_or_nan(sign, other, self) {
            Some(f) => return f,
            None           => {}
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
            let shifted = Float::right_shift_64(mul_result, (SIG_WIDTH_F32 - ROUND_WIDTH) as u64) as u32;            
            shifted
        };

        Float::pack_float32(sign, exp, sig, mode)
    }

    fn pack_float32(sign: bool, exp: i32, sig: u32, mode: RoundingMode) -> Float {
        let is_near_even = mode == RoundingMode::NearEven;
        let round_inc = match mode {
            RoundingMode::NearEven => 0x04,
            RoundingMode::NearMax  => 0x04,
            RoundingMode::Zero     => 0x00,
            RoundingMode::Max      => if sign { 0x00 } else { 0x07 },
            RoundingMode::Min      => if sign { 0x07 } else { 0x00 },
        };

        let round_bits = sig & ROUND_MASK;

        let (exp, sig) =
            if exp < 0 {                                
                let shamt = -exp as u32 + 1;
                let sig = Float::right_shift_32(sig, shamt);

                (0, sig)
            } else if exp >= 0xFF || (exp == 0xFE && sig + round_inc >= 0x0800_0000) {
                let infinite =
                    if round_inc == 0 { Float { sign, exp: 0xFE, sig: 0x007F_FFFF } }
                    else              { Float::infinite(sign) };

                return infinite;

            } else {
                (exp as u32, sig)
            };

        let (exp, sig) = Float::normalize(exp, sig);
        let sig = sig + round_inc;        
        let (exp, sig) = Float::normalize(exp, sig);
        
        let sig = (sig >> ROUND_WIDTH) & !((round_bits == 0x04 && is_near_even) as u32) & SIG_MASK_F32;

        Float { sign, exp, sig }
    }

    fn normalize(exp: u32, sig: u32) -> (u32, u32) {
        if sig >= 0x0800_0000 { Float::normalize(exp + 1, sig >> 1) }
        else                  { (exp, sig) }
    }

    fn right_shift_32(sig: u32, shamt: u32) -> u32 {
        if shamt > 31 { (sig != 0) as u32 }
        else          { (sig >> shamt) | (((sig & ((1 << shamt) - 1)) != 0) as u32) }
    }

    fn right_shift_64(sig: u64, shamt: u64) -> u64 {
        if shamt > 63 { (sig != 0) as u64 }
        else          { (sig >> shamt) | (((sig & ((1 << shamt) - 1)) != 0) as u64) }
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

        let uni = f32u32 { u: sign | exp | sig };
        unsafe { uni.f }
    }
}

#[cfg(test)]
mod test {
    extern crate rand;

    use crate::{ Float, f32u32 };
    use rand::{ rngs::StdRng, Rng, SeedableRng };
    
    #[repr(C)] 
    #[derive(Clone, Copy)]
    struct float32_t { v: u32 }

    impl Into<f32> for float32_t {
        fn into(self) -> f32 {
            let uni = f32u32 { u: self.v };
            unsafe { uni.f }
        }
    }

    impl From<f32> for float32_t {
        fn from(f: f32) -> float32_t {
            let uni = f32u32 { f };
            float32_t { v: unsafe { uni.u } }
        }
    }

    impl From<u32> for float32_t {
        fn from(u: u32) -> float32_t {
            float32_t { v: u }
        }
    }

    #[link(name="softfloat")]
    extern "C" {
        fn f32_mul(a: float32_t, b: float32_t) -> float32_t;
    }    

    #[test]
    fn mult() {
        let a = Float { sign: false, exp: 0x7F, sig: 0 };
        let b = Float { sign: false, exp: 0x67, sig: 0x12345 };
        let c = a.mul(b);

        assert_eq!(c.sign, false);
        assert_eq!(c.exp,  0x67);
        assert_eq!(c.sig,  0x12345, "0x{:08x} != 0x{:08x}", c.sig, 0x12345);        
    }    

    #[test]
    fn mult_with_softfloat() {
        let mut rng: StdRng = SeedableRng::seed_from_u64(0);
        let mut is_failure = false;

        for _ in 0..10 {
            let a: u32 = rng.gen();
            let b: u32 = rng.gen();

            let a_float = Float::new(a);
            let b_float = Float::new(b);
            let a_soft  = float32_t::from(a);
            let b_soft  = float32_t::from(b);

            let c_float = a_float.mul(b_float);
            let c_soft  = unsafe { f32_mul(a_soft, b_soft) };

            let c_soft: f32 = c_soft.into();
            let c_float: f32 = c_float.into();

            if c_soft != c_float {
                if !is_failure {
                    println!("{:>10}, {:>10}, {:>10}, {:>10}", "a", "b", "expect", "actual");
                }

                is_failure = true;                
                let c_soft_u32  = unsafe { (f32u32 { f: c_soft  }).u };
                let c_float_u32 = unsafe { (f32u32 { f: c_float }).u };

                println!("0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x}", a, b, c_soft_u32, c_float_u32);
            }    
        }
                
        assert!(!is_failure, "test is failure");
    }    
}

const BIAS_F32: i32 = 127;
const SIG_WIDTH_F32: u32 = 23;
const SIG_MASK_F32: u32 = (1 << SIG_WIDTH_F32) - 1;
const HIDDEN_SIGNIFICAND: u32 = 0x0080_0000;
const ROUND_WIDTH: u32 = 3;
const ROUND_MASK: u32 = (1 << ROUND_WIDTH) - 1;

