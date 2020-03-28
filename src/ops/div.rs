extern crate either;

use std::ops::Div;
use std::convert::TryFrom;
use crate::util::BitWidth;
use crate::{
    Float,
    FloatFormat,
    Exception,
    RoundingMode
};
use either::Either;

impl<T> Div for Float<T>
    where
        T: FloatFormat,
        T::BaseType: TryFrom<usize> + TryFrom<T::ExtendedType> + TryFrom<T::SignedType> + From<u8> + From<bool>,
        T::ExtendedType: From<u8> + From<bool> + From<T::BaseType>,
        T::SignedType: From<u8> + From<bool> + TryFrom<T::BaseType>,
        <T::BaseType as TryFrom<usize>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::ExtendedType>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::SignedType>>::Error: std::fmt::Debug,
        <T::SignedType as TryFrom<T::BaseType>>::Error: std::fmt::Debug,
{
  type Output = Self;

  fn div(self, other: Self) -> Self {
    self.div_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn div_impl<T>(fx: Float<T>, fy: Float<T>, mode: RoundingMode) -> (Float<T>, Exception)
    where
        T: FloatFormat,
        T::BaseType: TryFrom<usize> + TryFrom<T::ExtendedType> + TryFrom<T::SignedType> + From<u8> + From<bool>,
        T::ExtendedType: From<u8> + From<bool> + From<T::BaseType>,
        T::SignedType: From<u8> + From<bool> + TryFrom<T::BaseType>,
        <T::BaseType as TryFrom<usize>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::ExtendedType>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::SignedType>>::Error: std::fmt::Debug,
        <T::SignedType as TryFrom<T::BaseType>>::Error: std::fmt::Debug,
{
  let sign = fx.sign() ^ fy.sign();

  if fx.is_nan() || fy.is_nan() { return Float::<T>::propagate_nan(fx, fy); }
  if fx.is_inf() {
      if fy.is_inf() { return (Float::<T>::default_nan(), Exception::invalid()); }
      else { return (Float::<T>::infinite(sign), Exception::none()); }
  }        
  if fy.is_inf() { return (Float::<T>::zero(sign), Exception::none()); }
  if fy.is_zero() {
      if fx.is_zero() { return (Float::<T>::default_nan(), Exception::invalid()); }
      else { return (Float::<T>::infinite(sign), Exception::infinite()); }
  }

  let (exp_a, sig_a) = match Float::<T>::make_exp_sig(sign, fx) {
      Either::Left(pair) => return pair,
      Either::Right(pair) => pair,
  };

  let (exp_b, sig_b) = match Float::<T>::make_exp_sig(sign, fy) {
      Either::Left(pair) => return pair,
      Either::Right(pair) => pair,
  };

  let (exp, sig) = ext_div::<T>(sig_a, exp_a, sig_b, exp_b);

  Float::<T>::pack(sign, exp, sig, mode)
}

fn ext_div<T>(sig_a: T::BaseType, exp_a: T::SignedType, sig_b: T::BaseType, exp_b: T::SignedType) -> (T::SignedType, T::BaseType)
    where
        T: FloatFormat,
        T::BaseType: TryFrom<usize> + TryFrom<T::ExtendedType> + TryFrom<T::SignedType> + From<u8> + From<bool>,
        T::ExtendedType: From<u8> + From<bool> + From<T::BaseType>,
        T::SignedType: From<u8> + From<bool> + TryFrom<T::BaseType>,
        <T::BaseType as TryFrom<usize>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::ExtendedType>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::SignedType>>::Error: std::fmt::Debug,
        <T::SignedType as TryFrom<T::BaseType>>::Error: std::fmt::Debug,
{
    let shamt = T::sig_width() + Float::<T>::round_width();
    let dividend = T::ExtendedType::from(sig_a) << T::ExtendedType::from(shamt);
    let divisor = T::ExtendedType::from(sig_b);

    let sig = T::BaseType::try_from(dividend / divisor).unwrap();
    let is_there_rem = (dividend % divisor) != T::ExtendedType::from(0);

    let bias = T::BaseType::width() - (T::sig_width() + Float::<T>::round_width() + T::BaseType::from(1));
    let (mod_exp, sig) = Float::<T>::normalize_subnormal(sig, bias);
    let exp = exp_a - exp_b + T::SignedType::try_from(Float::<T>::bias()).unwrap();
    let exp = exp + mod_exp;

    (exp, sig | T::BaseType::from(is_there_rem))
  }