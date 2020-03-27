
use std::ops::Add;
use std::convert::TryFrom;
use crate::{
  Float,
  FloatFormat,
  RoundingMode,
  Exception,
};
use crate::constant::*;
use crate::util::*;

impl<T: FloatFormat> Add for Float<T> {
  type Output = Self;

  fn add(self, other: Self) -> Self {
    self.add_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn add_impl<T: FloatFormat>(fx: Float<T>, fy: Float<T>, mode: RoundingMode) -> (Float<T>, Exception)
{
  let (exp_diff, fa, fb) = if exp(fx) < exp(fy) {
      (exp(fy) - exp(fx), fy, fx)
  } else {
      (exp(fx) - exp(fy), fx, fy)
  };
  let sign = sign(fx);

  let sig_a_hidden = if exp(fa) == T::BaseType::from(0) { T::BaseType::from(0) } else { hidden_bit() };
  let sig_b_hidden = if exp(fb) == T::BaseType::from(0) { T::BaseType::from(0) } else { hidden_bit() };
  let sig_a = if exp(fa) == T::BaseType::from(0) {
    sig(fa) << (round_width() + T::BaseType::from(1))
  } else {
    (sig_a_hidden | sig(fa)) << round_width()
  };
  let sig_b = if exp(fb) == T::BaseType::from(0) {
    sig(fb) << (round_width() + T::BaseType::from(1))
  } else {
    (sig_b_hidden | sig(fb)) << round_width()
  };

  if exp(fa) == T::BaseType::from(0) && exp(fb) == T::BaseType::from(0) {
    let fc_sig = sig(fa) + sig(fb);
    let exp = fc_sig >> T::sig_width();
    let sig = fc_sig & ((T::BaseType::from(1) << T::sig_width()) - T::BaseType::from(1));

    return (
        Float::<T>::new(sign(fx), exp, sig),
        Exception::none(),
    );
  }

  if is_nan(fx) || is_nan(fy) { return propagate_nan(fx, fy) }
  if is_inf(fx) { return (fx, Exception::none()) }
  if is_inf(fy) { return (infinite(sign), Exception::none())}

  let exp = exp(fa);
  let sig_b = right_shift(sig_b, exp_diff);
  let sig = sig_a + sig_b;

  pack(sign, T::SignedType::try_from(exp).unwrap(), sig, mode)
}