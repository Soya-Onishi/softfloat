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

impl<T> Mul for Float<T>
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

  fn mul(self, other: Self) -> Self {
    self.mul_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn mul_impl<T>(fx: Float<T>, fy: Float<T>, mode: RoundingMode) -> (Float<T>, Exception)
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
    let try_mul_inf_or_nan = |sign: bool, f: Float<T>, g: Float<T>| -> Option<(Float<T>, Exception)> {
        if f.is_inf() && !g.is_nan() {
            if g.is_zero() { Some((Float::<T>::default_nan(), Exception::invalid())) }
            else { Some((Float::<T>::infinite(sign), Exception::none())) }
        } else if f.is_nan() || g.is_nan() {
            Some(Float::<T>::propagate_nan(f, g))
        } else {
            None
        }
    };

    let sign = fx.sign() ^ fy.sign();

    match try_mul_inf_or_nan(sign, fx, fy) {
        Some(f) => return f,
        None => {}
    }

    match try_mul_inf_or_nan(sign, fy, fx) {
        Some(f) => return f,
        None => {}
    }

    // zero multiplication derive zero
    let (exp_a, sig_a) = match Float::<T>::make_exp_sig(sign, fx) {
        Either::Left(zero) => return zero,
        Either::Right(pair) => pair,
    };

    let (exp_b, sig_b) = match Float::<T>::make_exp_sig(sign, fy) {
        Either::Left(zero) => return zero,
        Either::Right(pair) => pair,
    };

    let (exp, sig) = ext_mul::<T>(sig_a, exp_a, sig_b, exp_b);
    Float::<T>::pack(sign, exp, sig, mode)
}

fn ext_mul<T>(sig_a: T::BaseType, exp_a: T::SignedType, sig_b: T::BaseType, exp_b: T::SignedType) -> (T::SignedType, T::BaseType)
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
    let exp = exp_a + exp_b - T::SignedType::try_from(Float::<T>::bias()).unwrap();
    let shifted = Float::<T>::right_shift::<T::ExtendedType>(mul_result, T::ExtendedType::from(T::sig_width() - Float::<T>::round_width()));

    (exp, T::BaseType::try_from(shifted).unwrap())
}
