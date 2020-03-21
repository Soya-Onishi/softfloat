extern crate either;

use std::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, Not, Shr, Shl};
use std::cmp::{PartialOrd, PartialEq};
use crate::{
  Float, 
  RoundingMode, 
  Exception,
  constant::FloatConstant,
  constructor::FloatConstructor,
  format::FloatFormat,
  getter::FloatGetter,
};


use either::Either;

impl<T> Add for Float<T> {
  type Output = Self;

  fn add(self, other: Self) -> Self {
    self.add_with_mode(other, RoundingMode::NearEven).0
  }
}

impl<T> Sub for Float<T> {
  type Output = Self;

  fn sub(self, other: Self) -> Self {
    self.sub_with_mode(other, RoundingMode::NearEven).0
  }
}

impl<T> Mul for Float<T> {
  type Output = Self;

  fn mul(self, other: Self) -> Self {
    self.mul_with_mode(other, RoundingMode::NearEven).0
  }
}

impl<T> Div for Float<T> {
  type Output = Self;

  fn div(self, other: Self) -> Self {
    self.div_with_mode(other, RoundingMode::NearEven).0
  }
}

impl<T> Float<T> 
  where Float<T>: FloatGetter<T> + FloatConstructor<T> + FloatConstant<T> + FloatPack<T> + FloatCommonOps<T> + FloatFormat<T>,
        T: PartialEq + PartialOrd + Add<Output=T> + Sub<Output=T> + BitAnd<Output=T> + BitOr<Output=T> + Shr<T, Output=T> + Shl<T, Output=T> + From<usize> + ToSigned<Output=i32>,
{
  pub fn add_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
    if self.sign() ^ other.sign() {
      self.sub_impl(other, mode)
    } else {
      self.add_impl(other, mode)
    }
  }

  pub fn sub_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
    if self.sign() ^ other.sign() {
      self.add_impl(other, mode)
    } else {
      self.sub_impl(other, mode)
    }
  }

  pub fn mul_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
    self.mul_impl(other, mode)
  }

  pub fn div_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
    self.div_impl(other, mode)
  }

  fn add_impl(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
    let (exp_diff, fa, fb) = if self.exp() < other.exp() {
        (other.exp() - self.exp(), other, self)
    } else {
        (self.exp() - other.exp(), self, other)
    };
    let sign = self.sign();

    let sig_a_hidden = if fa.exp() == T::from(0) { T::from(0) } else { Float::<T>::hidden_bit() };
    let sig_b_hidden = if fb.exp() == T::from(0) { T::from(0) } else { Float::<T>::hidden_bit() };
    let sig_a = if fa.exp() == T::from(0) {
      fa.sig() << (Float::<T>::round_width() + T::from(1))
    } else {
      (sig_a_hidden | fa.sig()) << Float::<T>::round_width()
    };
    let sig_b = if fb.exp() == T::from(0) {
      fb.sig() << (Float::<T>::round_width() + T::from(1))
    } else {
      (sig_b_hidden | fb.sig()) << Float::<T>::round_width()
    };

    if fa.exp() == T::from(0) && fb.exp() == T::from(0) {
      let fc_sig = fa.sig() + fb.sig();
      let exp = fc_sig >> T::from(23);
      let sig = fc_sig & T::from(0x007F_FFFF);

      return (
          Float::<T>::constructor(self.sign(), exp, sig),
          Exception::none(),
      );
    }

    if self.exp() == T::from(0xFF) || other.exp() == T::from(0xFF) {
      let result = if (self.exp() == T::from(0xFF) && self.sig() != T::from(0)) || (other.exp() == T::from(0xFF) && other.sig() != T::from(0)) {
        Float::<T>::propagate_nan(self, other)
      } else if self.exp() == T::from(0xFF) {
        (self, Exception::none())
      } else {
        (Float::<T>::infinite(sign), Exception::none())
      };

      return result;
    }

    let exp = fa.exp();
    let sig_b = Float::<T>::right_shift(sig_b, exp_diff);
    let sig = sig_a + sig_b;

    Float::<T>::pack(sign, exp.to_signed(), sig, mode)
  }

  fn sub_impl(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
      let (exp_diff, fa, fb, sign): (_, _, _, bool) = if self.exp() < other.exp() {
          (other.exp() - self.exp(), other, self, !self.sign())
      } else if self.exp() > other.exp() {
          (self.exp() - other.exp(), self, other, self.sign())
      } else {
          if self.sig() < other.sig() {
              (T::from(0), other, self, !self.sign())
          } else {
              (T::from(0), self, other, self.sign())
          }
      };
      let sig_a = if fa.exp() == T::from(0) { fa.sig() << T::from(1) } else { fa.sig() | Float::<T>::hidden_bit() };
      let sig_b = if fb.exp() == T::from(0) { fb.sig() << T::from(1) } else { fb.sig() | Float::<T>::hidden_bit() };
      let sig_a = sig_a << Float::<T>::round_width();
      let sig_b = sig_b << Float::<T>::round_width();
      let sig_b = Float::<T>::right_shift(sig_b, exp_diff);

      // there is no need to calculate fb.exp == 0xFF
      // because fa.exp >= fb.exp must be true.
      if fa.exp() == T::from(0xFF) || fb.exp() == T::from(0xFF) {
          if fa.is_inf() && fb.is_inf() {
              return (Float::default_nan(), Exception::invalid())
          }

          if fa.is_nan() || fb.is_nan() {
              return Float::<T>::propagate_nan(self, other);
          }

          return (Float::infinite(sign), Exception::none());
      }
      let sig = sig_a - sig_b;

      if sig == T::from(0) {
          return (
              Float::zero(mode == RoundingMode::Min),
              Exception::none(),
          );
      }
      let (exp, sig) = Float::<T>::normalize_subnormal(sig, T::from(5));
      let exp = fa.exp().to_signed() + exp;

      Float::<T>::pack(sign, exp, sig, mode)
  }

  pub fn mul_impl(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
      // TODO: implement subnormal pattern

      let try_mul_inf_or_nan = |sign: bool, f: Float<T>, g: Float<T>| -> Option<(Float<T>, Exception)> {        
          if f.is_inf() && !g.is_nan() { 
              if g.is_zero() { Some((Float::<T>::default_nan(), Exception::invalid())) }
              else { Some((Float::infinite(sign), Exception::none())) }
          } else if f.is_nan() || g.is_nan() {
              Some(Float::<T>::propagate_nan(f, g))
          } else {
              None
          }
      };

      let make_exp_sig = |sign: bool, f: Float<T>| -> Either<(Float<T>, Exception), (i32, T)> {
          if f.exp() != T::from(0) {
              Either::Right((f.exp().to_signed(), f.sig() | Float::<T>::hidden_bit()))
          } else if f.sig() == T::from(0) {
              Either::Left((Float::zero(sign), Exception::none()))
          } else {
              // 1.0 * (2 ^ -127) cannot be represented as normal 
              // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
              // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
              // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
              let shamt = f.sig().leading_zeros() - 8;
              Either::Right((-(shamt.to_signed()) + 1, f.sig() << shamt))
          }
      };

      let sign = self.sign() ^ other.sign();

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

  fn div_impl(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
    let sign = self.sign() ^ other.sign();

    if self.is_nan() || other.is_nan() { return Float::<T>::propagate_nan(self, other); }
    if self.is_inf() { 
        if other.is_inf() { return (Float::<T>::default_nan(), Exception::invalid()); }
        else { return (Float::<T>::infinite(sign), Exception::none()); }
    }        
    if other.is_inf() { return (Float::<T>::zero(sign), Exception::none()); }
    if other.is_zero() { 
        if self.is_zero() { return (Float::<T>::default_nan(), Exception::invalid()); }
        else { return (Float::<T>::infinite(sign), Exception::infinite()); }
    }

    // TODO: 
    //   This is copy of make_exp_sig in mul_impl
    //   Make refactoring.
    let make_exp_sig = |sign: bool, f: Float<T>| -> Either<(Float<T>, Exception), (i32, T)> {
        if f.exp() != T::from(0) {
            Either::Right((f.exp().to_signed(), f.sig() | Float::<T>::hidden_bit()))
        } else if f.sig() == T::from(0) {
            Either::Left((Float::<T>::zero(sign), Exception::none()))
        } else {
            // 1.0 * (2 ^ -127) cannot be represented as normal 
            // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
            // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
            // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
            let shamt = f.sig().leading_zeros() - 8;
            Either::Right((-(shamt.to_signed()) + 1, f.sig() << shamt))
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

    let dividend = (sig_a as u64) << (T::from(23) + Float::<T>::round_width());
    let divisor = sig_b as u64;
    let sig = (dividend / divisor) as u32;
    let rem = ((dividend % divisor) != 0) as u32;
    let (mod_exp, sig) = Float::normalize_subnormal_f32(sig, 5);
    let exp = exp_a - exp_b + mod_exp + BIAS_F32;

    Float::pack_float32(sign, exp, sig | rem, mode)
  }
}

trait FloatCommonOps<A> {    
  fn right_shift(sig: A, shamt: A) -> A;
  fn normalize_subnormal(sig: A, bias: A) -> (i32, A);
  fn normalize(sign: bool, exp: A, sig: A, round_inc: A) -> Either<(Float<A>, Exception), (A, A)>;
  fn make_round_value(sing: bool, mode: RoundingMode) -> A;
}

impl FloatCommonOps<u32> for Float<u32> {
  fn right_shift(sig: u32, shamt: u32) -> u32 {
    if shamt > 31 {
      (sig != 0) as u32
    } else {
      (sig >> shamt) | (((sig & ((1 << shamt) - 1)) != 0) as u32)
    }
  }

  fn normalize_subnormal(sig: u32, bias: u32) -> (i32, u32) {
    let shamt = sig.leading_zeros() - bias;
    (-(shamt as i32), sig << shamt)
  }

  fn normalize(sign: bool, exp: u32, sig: u32, round_inc: u32) -> Either<(Float<u32>, Exception), (u32, u32)> {
    match (exp, sig) {
    (exp, _) if exp >= 0xFF => {
        let infinite = if round_inc == 0 {
          Float::<u32>::constructor(sign, 0xFE, 0x007F_FFFF)
        } else {
          Float::<u32>::constructor(sign, 0xFF, 0x0000_0000)            
        };

        either::Left((infinite, Exception::overflow() | Exception::inexact()))
    }
    (exp, sig) if sig >= 0x0800_0000 => {
        Float::normalize(sign, exp + 1, Float::<u32>::right_shift(sig, 1), round_inc)
    }
    // if exp == 0 && sig >= 0x0400_0000 means 10.0 * 2^-127 => 1.0 * 2^-126
    (0x00, sig) if sig >= 0x0400_0000 => Float::<u32>::normalize(sign, 1, sig, round_inc),
    (exp, sig) => either::Right((exp, sig)),
    }
  }

  fn make_round_value(sign: bool, mode: RoundingMode) -> u32 {
    match mode {
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
    }
  }
}

pub trait FloatPack<T> {
  fn propagate_nan(a: Float<T>, b: Float<T>) -> (Float<T>, Exception);
  fn pack(sign: bool, exp: i32, sig: T, mode: RoundingMode) -> (Float<T>, Exception);
}

impl<T> FloatPack<T> for Float<T> 
  where Float<T>: FloatCommonOps<T> + FloatConstant<T> + FloatConstructor<T> + FloatGetter<T> + FloatFormat<T>,
        T: PartialOrd + PartialEq + Add + BitAnd<Output=T> + BitOr<Output=T> + Not + Shr<T, Output=T> + From<u32> + From<i32> + From<bool> 
{
  fn propagate_nan(a: Float<T>, b: Float<T>) -> (Float<T>, Exception) {
    let make_qnan = |f: Float<T>| -> Float<T> {
        Float::<T>::constructor(f.sign(), f.exp(), f.sig() | Float::<T>::signal_bit())
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

  fn pack(sign: bool, exp: i32, sig: T, mode: RoundingMode) -> (Float<T>, Exception) {
    let is_near_even = mode == RoundingMode::NearEven;
    let round_value = Float::<T>::make_round_value(sign, mode);

    let (exp, sig, is_underflow) = if exp <= 0 {
      let is_tiny = (sig + round_value < 0x0800_0000) || exp < 0;
      let shamt = T::from(-exp) + 1;
      let sig = Float::<T>::right_shift(sig, shamt);
      let round_bits = sig & Float::<T>::round_mask() != 0;

      (0, sig, is_tiny && round_bits)
    } else {
      (T::from(exp), sig, false)
    };

    let (exp, sig) = match Float::<T>::normalize(sign, exp, sig, round_value) {
      either::Left(result) => return result,
      either::Right(pair) => pair,
    };

    let round_bits = sig & Float::<T>::round_mask();
    let sig = sig + round_value;

    let (exp, sig) = match Float::<T>::normalize(sign, exp, sig, round_value) {
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
      (sig >> Float::<T>::round_with()) & 
      !(T::from((round_bits == T::from(0x4)) && is_near_even)) & 
      Float::<T>::significand_mask();

    (Float::<T>::constructor(sign, exp, sig), exception)
  }
}

trait ToSigned {
  type Output;

  fn to_signed(self) -> Self::Output;
}

impl ToSigned for u32 {
  type Output = i32;

  fn to_signed(self) -> i32 { self as i32}
}

impl ToSigned for u64 {
  type Output = i32;

  fn to_signed(self) -> i32 { self as i32 }
}



const BIAS_F32: i32 = 127;
const SIG_WIDTH_F32: u32 = 23;
const SIG_MASK_F32: u32 = (1 << SIG_WIDTH_F32) - 1;
const HIDDEN_SIGNIFICAND: u32 = 0x0080_0000;
const ROUND_WIDTH: u32 = 3;
const ROUND_MASK: u32 = (1 << ROUND_WIDTH) - 1;