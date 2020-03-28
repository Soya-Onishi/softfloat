use crate::{Exception, Float, FloatFormat, RoundingMode};
use std::convert::TryFrom;
use std::ops::Add;

impl<T> Add for Float<T>
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

  fn add(self, other: Self) -> Self {
    self.add_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn add_impl<T>(fx: Float<T>, fy: Float<T>, mode: RoundingMode, ) -> (Float<T>, Exception)
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
  let (exp_diff, fa, fb) = if fx.exp() < fy.exp() {
    (fy.exp() - fx.exp(), fy, fx)
  } else {
    (fx.exp() - fy.exp(), fx, fy)
  };
  let sign = fx.sign();

  let sig_a_hidden = if fa.exp() == T::BaseType::from(0) {
    T::BaseType::from(0)
  } else {
    Float::<T>::hidden_bit()
  };
  let sig_b_hidden = if fb.exp() == T::BaseType::from(0) {
    T::BaseType::from(0)
  } else {
    Float::<T>::hidden_bit()
  };
  let sig_a = if fa.exp() == T::BaseType::from(0) {
    fa.sig() << (Float::<T>::round_width() + T::BaseType::from(1))
  } else {
    (sig_a_hidden | fa.sig()) << Float::<T>::round_width()
  };
  let sig_b = if fb.exp() == T::BaseType::from(0) {
    fb.sig() << (Float::<T>::round_width() + T::BaseType::from(1))
  } else {
    (sig_b_hidden | fb.sig()) << Float::<T>::round_width()
  };

  if fa.exp() == T::BaseType::from(0) && fb.exp() == T::BaseType::from(0) {
    let fc_sig = fa.sig() + fb.sig();
    let exp = fc_sig >> T::sig_width();
    let sig = fc_sig & ((T::BaseType::from(1) << T::sig_width()) - T::BaseType::from(1));

    return (Float::<T>::new(fx.sign(), exp, sig), Exception::none());
  }

  if fx.is_nan() || fy.is_nan() {
    return Float::<T>::propagate_nan(fx, fy);
  }
  if fx.is_inf() {
    return (fx, Exception::none());
  }
  if fy.is_inf() {
    return (Float::<T>::infinite(sign), Exception::none());
  }

  let exp = fa.exp();
  let sig_b = Float::<T>::right_shift::<T::BaseType>(sig_b, exp_diff);
  let sig = sig_a + sig_b;

  Float::<T>::pack(sign, T::SignedType::try_from(exp).unwrap(), sig, mode)
}
