extern crate either;

use std::ops::Mul;
use std::convert::TryFrom;
use either::Either;
use crate::{
    Float,
    FloatFormat,
    RoundingMode,
    Exception
};
use crate::constant::*;
use crate::util::*;

impl<T: FloatFormat> Mul for Float<T> {
  type Output = Self;

  fn mul(self, other: Self) -> Self {
    self.mul_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn mul_impl<T: FloatFormat>(fx: Float<T>, fy: Float<T>, mode: RoundingMode) -> (Float<T>, Exception)
{   
    let try_mul_inf_or_nan = |sign: bool, f: Float<T>, g: Float<T>| -> Option<(Float<T>, Exception)> {
        if is_inf(f) && !is_nan(g) {
            if is_zero(g) { Some((default_nan(), Exception::invalid())) }
            else { Some((infinite(sign), Exception::none())) }
        } else if is_nan(f) || is_nan(g) {
            Some(propagate_nan(f, g))
        } else {
            None
        }
    };

    let sign = sign(fx) ^ sign(fy);

    match try_mul_inf_or_nan(sign, fx, fy) {
        Some(f) => return f,
        None => {}
    }

    match try_mul_inf_or_nan(sign, fy, fx) {
        Some(f) => return f,
        None => {}
    }

    // zero multiplication derive zero
    let (exp_a, sig_a) = match make_exp_sig(sign, fx) {
        Either::Left(zero) => return zero,
        Either::Right(pair) => pair,
    };

    let (exp_b, sig_b) = match make_exp_sig(sign, fy) {
        Either::Left(zero) => return zero,
        Either::Right(pair) => pair,
    };

    let (exp, sig) = ext_mul(sig_a, exp_a, sig_b, exp_b);
    pack(sign, exp, sig, mode)
}

fn ext_mul<T: FloatFormat>(sig_a: T::BaseType, exp_a: T::SignedType, sig_b: T::BaseType, exp_b: T::SignedType) -> (T::SignedType, T::BaseType)
{   
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
    let mul_result = T::ExtendedType::from(sig_a) * T::ExtendedType::from(sig_b);
    let exp = exp_a + exp_b - T::SignedType::try_from(bias()).unwrap();
    let shifted = right_shift(mul_result, T::ExtendedType::from(T::BaseType::try_from(T::sig_width()).unwrap() - round_width()));

    (exp, T::BaseType::try_from(shifted).unwrap())
}
