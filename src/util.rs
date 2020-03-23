extern crate either;

use either::Either;
use std::ops::{Add, Sub, Shl, Shr, BitAnd, BitOr, Not, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use crate::{Float, Exception, RoundingMode};
use crate::constant::*;

pub(crate) fn propagate_nan<T>(a: Float<T>, b: Float<T>) -> (Float<T>, Exception)
  where Float<T>: FloatFormat<T> + FloatConstant<T>,
        T: BitOr<Output=T> + Clone + Copy
{
  let make_qnan = |f: Float<T>| -> Float<T> {
      Float::<T>::new(f.sign(), f.exp(), f.sig() | Float::<T>::signal_bit())
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

pub(crate) fn pack<A, B>(sign: bool, exp: B, sig: A, mode: RoundingMode) -> (Float<A>, Exception) 
  where Float<A>: FloatConstant<A> + FloatFormat<A>,
        A: From<u8> + From<bool> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + TryFrom<B> + BitWidth + Clone + Copy,
        B: From<u8> + PartialOrd + Neg<Output=B> + Clone + Copy,
        A::Error: std::fmt::Debug
{
  let is_near_even = mode == RoundingMode::NearEven;
  let round_value = make_round_value(sign, mode);

  let (exp, sig, is_underflow) = if exp <= B::from(0) {
    let is_sufficient_size = (sig + round_value) >= (A::from(1) << (Float::<A>::sig_width() + round_width() + A::from(1)));
    let is_neg_exp = exp < B::from(0);
    let is_tiny = !is_sufficient_size || is_neg_exp;
    let shamt = A::try_from(-exp).unwrap() + A::from(1);
    let sig = right_shift(sig, shamt);
    let round_bits = (sig & round_mask()) != A::from(0);

    (A::from(0), sig, is_tiny && round_bits)
  } else {
    (A::try_from(exp).unwrap(), sig, false)
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

  let exception = if round_bits != A::from(0) {
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
    (sig >> round_width::<A>()) & 
    !(A::from((round_bits == A::from(0x4)) && is_near_even)) & 
    sig_max::<A>();

  (Float::<A>::new(sign, exp, sig), exception)
}

pub(crate) fn round_width<T>() -> T
  where T: From<u8> + Shl<Output=T> + Sub<Output=T>
{
  T::from(3)
}

pub(crate) fn round_mask<T>() -> T 
  where T: From<u8> + Shl<Output=T> + Sub<Output=T>
{
  (T::from(1) << round_width::<T>()) - T::from(1)
}

pub(crate) fn right_shift<T>(sig: T, shamt: T) -> T 
  where T: From<u8> + From<bool> + PartialEq + PartialOrd + Sub<Output=T> + Shr<Output=T> + Shl<Output=T> + BitAnd<Output=T> + BitOr<Output=T> + BitWidth + Clone + Copy
{
  if shamt >= T::width() {
    T::from(sig != T::from(0))
  } else {
    let shifted = sig >> shamt;
    let sticky = (sig & ((T::from(1) << shamt) - T::from(1))) != T::from(0);

    shifted | T::from(sticky)
  }
}

pub(crate) fn make_round_value<T>(sign: bool, mode: RoundingMode) -> T 
  where T: From<u8>
{
  match mode {
    RoundingMode::NearEven => T::from(0x04),
    RoundingMode::NearMax => T::from(0x04),
    RoundingMode::Zero => T::from(0x00),
    RoundingMode::Max => {
        if sign {
            T::from(0x00)
        } else {
            T::from(0x07)
        }
    }
    RoundingMode::Min => {
        if sign {
            T::from(0x07)
        } else {
            T::from(0x00)
        }
    }
  }
}

pub(crate) fn normalize<T>(sign: bool, exp: T, sig: T, round_inc: T) -> Either<(Float<T>, Exception), (T, T)> 
  where Float<T>: FloatFormat<T> + FloatConstant<T>,
        T: From<u8> + From<bool> + PartialEq + PartialOrd + Add<Output=T> + Sub<Output=T> + Shl<Output=T> + Shr<Output=T> + BitAnd<Output=T> + BitOr<Output=T> + BitWidth + Clone + Copy
{
  match (exp, sig) {
  (exp, _) if exp >= exp_max::<T>() => {
      let infinite = if round_inc == T::from(0) {
        Float::<T>::new(sign, exp_max::<T>() - T::from(1), sig_max::<T>())
      } else {
        Float::<T>::new(sign, exp_max::<T>(), T::from(0))
      };

      either::Left((infinite, Exception::overflow() | Exception::inexact()))
  }
  (exp, sig) if sig >= (T::from(1) << (Float::<T>::sig_width() + round_width() + T::from(1))) => {
      normalize(sign, exp + T::from(1), right_shift(sig, T::from(1)), round_inc)
  }
  // if exp == 0 && sig >= 0x0400_0000 means 10.0 * 2^-127 => 1.0 * 2^-126
  (exp, sig) if exp == T::from(0) && sig >= (T::from(1) << (Float::<T>::sig_width() + round_width())) => normalize(sign, T::from(1), sig, round_inc),
  (exp, sig) => either::Right((exp, sig)),
  }
}

pub(crate) fn normalize_subnormal<A, B>(sig: A, bias: A) -> (B, A) 
  where A: LeadingZeros + Sub<Output=A> + Shl<Output=A> + Clone + Copy,
        B: TryFrom<A> + Neg<Output=B> + Clone + Copy,
        B::Error: std::fmt::Debug,
        
{
  let shamt = sig.count_leading_zeros() - bias;
  (-(B::try_from(shamt).unwrap()), sig << shamt)
}

// TODO: 
  //   This is copy of make_exp_sig in mul_impl
  //   Make refactoring.
pub(crate) fn make_exp_sig<A, B>(sign: bool, f: Float<A>) -> Either<(Float<A>, Exception), (B, A)> 
  where Float<A>: FloatConstant<A> + FloatFormat<A>,
        A: From<u8> + PartialEq + Add<Output=A> + Sub<Output=A> + Shl<Output=A> +  BitOr<Output=A> + LeadingZeros + BitWidth + Clone + Copy,
        B: From<u8> + Neg<Output=B> + TryFrom<A> + Add<Output=B> + Clone + Copy,
        B::Error: std::fmt::Debug,
{
    if f.exp() != A::from(0) {
        Either::Right((B::try_from(f.exp()).unwrap(), f.sig() | Float::<A>::hidden_bit()))
    } else if f.sig() == A::from(0) {
        Either::Left((Float::<A>::zero(sign), Exception::none()))
    } else {
        // 1.0 * (2 ^ -127) cannot be represented as normal 
        // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
        // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
        // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
        let shamt = f.sig().count_leading_zeros() - (A::width() - (Float::<A>::sig_width() + A::from(1)));          
        Either::Right((-B::try_from(shamt).unwrap() + B::from(1), f.sig() << shamt))
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