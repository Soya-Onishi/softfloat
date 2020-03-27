extern crate either;

use either::Either;
use std::ops::{Add, Sub, Shl, Shr, BitAnd, BitOr, Not, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use crate::{Float, FloatFormat, Exception, RoundingMode};
use crate::constant::*;

pub(crate) fn propagate_nan<T>(a: Float<T>, b: Float<T>) -> (Float<T>, Exception)
  where T: FloatFormat,
{
  let make_qnan = |f: Float<T>| -> Float<T> {
      Float::<T>::new(sign(f), exp(f), sig(f) | T::signal_bit())
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

pub(crate) fn pack<T>(sign: bool, exp: T::SignedType, sig: T::BaseType, mode: RoundingMode) -> (Float<T>, Exception)
    where T: FloatFormat
{
  let is_near_even = mode == RoundingMode::NearEven;
  let round_value = make_round_value(sign, mode);

  let (exp, sig, is_underflow) = if exp <= T::SignedType::from(0) {
    let is_sufficient_size = (sig + round_value) >= (T::BaseType::from(1) << (T::BaseType::try_from(T::sig_width()).unwrap() + round_width() + T::from(1)));
    let is_neg_exp = exp < T::SignedType::from(0);
    let is_tiny = !is_sufficient_size || is_neg_exp;
    let shamt = T::BaseType::try_from(-exp).unwrap() + T::BaseType::from(1);
    let sig = right_shift(sig, shamt);
    let round_bits = (sig & round_mask()) != T::BaseType::from(0);

    (T::BaseType::from(0), sig, is_tiny && round_bits)
  } else {
    (T::BaseType::try_from(exp).unwrap(), sig, false)
  };

  let (exp, sig) = match normalize(sign, exp, sig, round_value) {
    either::Left(result) => return result,
    either::Right(pair) => pair,
  };

  let round_bits = sig & round_mask();
  let sig = sig + round_value;

  let (exp, sig) = match normalize(sign, exp, sig, round_value) {
    either::Left(result) => return result,
    either::Right(pair) => pair,
  };

  let exception = if round_bits != T::BaseType::from(0) {
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
    (sig >> round_width()) &
    !(T::BaseType::from((round_bits == T::BaseType::from(0x4)) && is_near_even)) &
    sig_max();

  (Float::<T>::new(sign, exp, sig), exception)
}

pub(crate) fn round_width<T>() -> T::BaseType
    where T: FloatFormat,
{
  T::BaseType::from(3)
}

pub(crate) fn round_mask<T>() -> T 
  where T: FloatFormat
{
  (T::BaseType::from(1) << round_width()) - T::BaseType::from(1)
}

pub(crate) fn right_shift<T>(sig: T::BaseType, shamt: T::BaseType) -> T::BaseType
  where T: FloatFormat
{
  if shamt >= T::BaseType::width() {
    T::BaseType::from(sig != T::BaseType::from(0))
  } else {
    let shifted = sig >> shamt;
    let sticky = (sig & ((T::BaseType::from(1) << shamt) - T::BaseType::from(1))) != T::BaseType::from(0);

    shifted | T::BaseType::from(sticky)
  }
}

pub(crate) fn make_round_value<T>(sign: bool, mode: RoundingMode) -> T::BaseType
  where T: FloatFormat
{
  match mode {
    RoundingMode::NearEven => T::BaseType::from(0x04),
    RoundingMode::NearMax => T::BaseType::from(0x04),
    RoundingMode::Zero => T::BaseType::from(0x00),
    RoundingMode::Max => {
        if sign {
            T::BaseType::from(0x00)
        } else {
            T::BaseType::from(0x07)
        }
    }
    RoundingMode::Min => {
        if sign {
            T::BaseType::from(0x07)
        } else {
            T::BaseType::from(0x00)
        }
    }
  }
}

pub(crate) fn normalize<T>(sign: bool, exp: T, sig: T, round_inc: T) -> Either<(Float<T>, Exception), (T, T)> 
  where T: FloatFormat
{
  match (exp, sig) {
  (exp, _) if exp >= exp_max() => {
      let infinite = if round_inc == T::BaseType::from(0) {
        Float::<T>::new(sign, exp_max() - T::BaseType::from(1), sig_max())
      } else {
        Float::<T>::new(sign, exp_max(), T::BaseType::from(0))
      };

      either::Left((infinite, Exception::overflow() | Exception::inexact()))
  }
  (exp, sig) if sig >= (T::BaseType::from(1) << (T::sig_width() + round_width() + T::BaseType::from(1))) => {
      normalize(sign, exp + T::BaseType::from(1), right_shift(sig, T::BaseType::from(1)), round_inc)
  }
  // if exp == 0 && sig >= 0x0400_0000 means 10.0 * 2^-127 => 1.0 * 2^-126
  (exp, sig) if exp == T::BaseType::from(0) && sig >= (T::BaseType::from(1) << (T::BaseType::try_from(T::sig_width()).unwrap() + round_width())) => normalize(sign, T::BaseType::from(1), sig, round_inc),
  (exp, sig) => either::Right((exp, sig)),
  }
}

pub(crate) fn normalize_subnormal<T: FloatFormat>(sig: T::BaseType, bias: T::BaseType) -> (T::SignedType, T::BaseType)
{
  let shamt = sig.count_leading_zeros() - bias;
  (-(T::SignedType::try_from(shamt).unwrap()), sig << shamt)
}

pub(crate) fn make_exp_sig<T: FloatFormat>(sign: bool, f: Float<T>) -> Either<(Float<T>, Exception), (T::SignedType, T::BaseType)>
{
    if exp(f) != T::BaseType::from(0) {
        Either::Right((T::SignedType::try_from(exp(f)).unwrap(), sig(f) | hidden_bit()))
    } else if sig(f) == T::BaseType::from(0) {
        Either::Left((zero(sign), Exception::none()))
    } else {
        // 1.0 * (2 ^ -127) cannot be represented as normal 
        // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
        // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
        // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
        let shamt = sig(f).count_leading_zeros() - (T::BaseType::width() - (T::sig_width() + T::BaseType::from(1)));
        Either::Right((-T::SignedType::try_from(shamt).unwrap() + T::SignedType::from(1), sig(f) << shamt))
    }
}

pub(crate) trait Extends {
  type Output;
  fn extend(v: Self) -> Self::Output;
}

impl Extends for u16 {
  type Output = u32;
  fn extend(v: u16) -> u32 { v as u32 }
}

impl Extends for u32 {
  type Output = u64;

  fn extend(v: u32) -> u64 { v as u64 }
}

impl Extends for u64 {
  type Output = u128;

  fn extend(v: u64) -> u128 { v as u128 }
}

pub(crate) trait LeadingZeros {
  fn count_leading_zeros(self) -> Self;
}

impl LeadingZeros for u16 {
  fn count_leading_zeros(self) -> u16 { u16::try_from(self.leading_zeros()).unwrap() }
}

impl LeadingZeros for u32 {
  fn count_leading_zeros(self) -> u32 { self.leading_zeros() }
}

impl LeadingZeros for u64 {
  fn count_leading_zeros(self) -> u64 { self.leading_zeros().into() }
}

pub(crate) trait BitWidth {
  fn width() -> Self;
}

impl BitWidth for u16 {
  fn width() -> u16 { 16 }
}

impl BitWidth for u32 {
  fn width() -> u32 { 32 }
}

impl BitWidth for u64 {
  fn width() -> u64 { 64 }
}

impl BitWidth for u128 {
  fn width() -> u128 { 128 }
}