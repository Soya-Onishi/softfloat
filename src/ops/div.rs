extern crate either;

use std::ops::Div;
use std::convert::TryFrom;
use crate::{
    Float,
    FloatFormat,
    Exception,
    RoundingMode
};
use either::Either;
use crate::constant::*;
use crate::util::*;    

impl<T: FloatFormat> Div for Float<T> {
  type Output = Self;

  fn div(self, other: Self) -> Self {
    self.div_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn div_impl<T: FloatFormat>(fx: Float<T>, fy: Float<T>, mode: RoundingMode) -> (Float<T>, Exception)
{
  let sign = fx.sign() ^ fy.sign();

  if is_nan(fx) || is_nan(fy) { return propagate_nan(fx, fy); }
  if is_inf(fx) {
      if is_inf(fy) { return (default_nan(), Exception::invalid()); }
      else { return (infinite(sign), Exception::none()); }
  }        
  if is_inf(fy) { return (zero(sign), Exception::none()); }
  if is_zero(fy) {
      if is_zero(fx) { return (default_nan(), Exception::invalid()); }
      else { return (infinite(sign), Exception::infinite()); }
  }

  let (exp_a, sig_a) = match make_exp_sig(sign, fx) {
      Either::Left(pair) => return pair,
      Either::Right(pair) => pair,
  };

  let (exp_b, sig_b) = match make_exp_sig(sign, fy) {
      Either::Left(pair) => return pair,
      Either::Right(pair) => pair,
  };

  let (exp, sig) = ext_div(sig_a, exp_a, sig_b, exp_b);

  pack(sign, exp, sig, mode)
}

fn ext_div<T: FloatFormat>(sig_a: T::BaseType, exp_a: T::SignedType, sig_b: T::BaseType, exp_b: T::SignedType) -> (T::SignedType, T::BaseType)
{
    let shamt = T::BaseType::try_from(T::sig_width()).unwrap() + round_width();
    let dividend = T::ExtendedType::from(sig_a) << T::ExtendedType::from(shamt);
    let divisor = T::ExtendedType::from(sig_b);

    let sig = T::BaseType::try_from(dividend / divisor).unwrap();
    let is_there_rem = (dividend % divisor) != T::ExtendedType::from(0);
    let (mod_exp, sig) = normalize_subnormal(sig, T::BaseType::width() - (T::BaseType::try_from(T::sig_width()).unwrap() + round_width() + T::BaseType::from(1)));
    let exp = exp_a - exp_b + T::SignedType::try_from(bias()).unwrap();
    let exp = exp + mod_exp;

    (exp, sig | T::BaseType::from(is_there_rem))
  }