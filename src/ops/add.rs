
use std::ops::{Add, Sub, BitAnd, BitOr, Not, Shl, Shr, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use crate::{
  Float,
  RoundingMode,
  Exception,
};
use crate::constant::*;
use crate::util::*;

impl Add for Float<u32> {
  type Output = Self;

  fn add(self, other: Self) -> Self {
    self.add_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn add_impl<A, B, C, D>(fx: Float<A>, fy: Float<A>, mode: RoundingMode) -> (Float<A>, Exception) 
where Float<A>: FloatConstant<A> + FloatFormat<A>,
      A : TryFrom<B, Error=C> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + From<u8> + From<bool> + LeadingZeros + BitWidth + Clone + Copy,
      B : TryFrom<A, Error=D> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + From<u8> + PartialOrd + Clone + Copy,
      C : std::fmt::Debug,
      D : std::fmt::Debug,
{
  let (exp_diff, fa, fb) = if fx.exp() < fy.exp() {
      (fy.exp() - fx.exp(), fy, fx)
  } else {
      (fx.exp() - fy.exp(), fx, fy)
  };
  let sign = fx.sign();

  let sig_a_hidden = if fa.exp() == A::from(0) { A::from(0) } else { Float::<A>::hidden_bit() };
  let sig_b_hidden = if fb.exp() == A::from(0) { A::from(0) } else { Float::<A>::hidden_bit() };
  let sig_a = if fa.exp() == A::from(0) {
    fa.sig() << (round_width::<A>() + A::from(1))
  } else {
    (sig_a_hidden | fa.sig()) << round_width::<A>()
  };
  let sig_b = if fb.exp() == A::from(0) {
    fb.sig() << (round_width::<A>() + A::from(1))
  } else {
    (sig_b_hidden | fb.sig()) << round_width::<A>()
  };

  if fa.exp() == A::from(0) && fb.exp() == A::from(0) {
    let fc_sig = fa.sig() + fb.sig();
    let exp = fc_sig >> Float::<A>::sig_width();
    let sig = fc_sig & ((A::from(1) << Float::<A>::sig_width()) - A::from(1));

    return (
        Float::<A>::new(fx.sign(), exp, sig),
        Exception::none(),
    );
  }

  if fx.is_nan() || fy.is_nan() { return propagate_nan(fx, fy) }
  if fx.is_inf() { return (fx, Exception::none()) }
  if fy.is_inf() { return (Float::<A>::infinite(sign), Exception::none())}  

  let exp = fa.exp();
  let sig_b = right_shift(sig_b, exp_diff);
  let sig = sig_a + sig_b;

  pack(sign, B::try_from(exp).unwrap(), sig, mode)
}