extern crate either;

use std::ops::{Add, Sub, BitAnd, BitOr, Not, Shl, Shr, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use crate::{
    Float,
    RoundingMode,
    Exception
};
use crate::constant::*;
use crate::util::*;


impl Sub for Float<u32> {
  type Output = Self;

  fn sub(self, other: Self) -> Self {
    self.sub_with_mode(other, RoundingMode::NearEven).0
  }
}

/*
pub(crate) fn sub_impl<A, B, C, D, E, F>(fx: Float<A>, fy: Float<A>, mode: RoundingMode) -> (Float<A>, Exception)     
where Float<A>: FloatConstant<A> + FloatFormat<A>,
A : Extends<Output=C> + TryFrom<B, Error=D> + TryFrom<C, Error=E> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + From<u8> + From<bool> + LeadingZeros + BitWidth,
B : TryFrom<A, Error=F> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + From<u8> + PartialOrd,
C : Shl<Output=C> + Div<Output=C> + Rem<Output=C> + From<u8> + PartialEq,
D : std::fmt::Debug,
E : std::fmt::Debug,
F : std::fmt::Debug,
{
  let (exp_diff, fa, fb, sign): (_, _, _, bool) = if fx.exp() < fy.exp() {
      (fy.exp() - fx.exp(), fy, fx, !fx.sign())
  } else if fx.exp() > fy.exp() {
      (fx.exp() - fy.exp(), fx, fy, fx.sign())
  } else {
      if fx.sig() < fy.sig() {
          (A::from(0), fy, fx, !fx.sign())
      } else {
          (A::from(0), fx, fy, fx.sign())
      }
  };
  let sig_a = if fa.exp() == A::from(0) { fa.sig() << A::from(1) } else { fa.sig() | Float::<A>::hidden_bit() };
  let sig_b = if fb.exp() == A::from(0) { fb.sig() << A::from(1) } else { fb.sig() | Float::<A>::hidden_bit() };
  let sig_a = sig_a << round_width::<A>();
  let sig_b = sig_b << round_width::<A>();
  let sig_b = right_shift(sig_b, exp_diff);

  // there is no need to calculate fb.exp == 0xFF
  // because fa.exp >= fb.exp must be true.
  if fa.exp() == A::from(0xFF) || fb.exp() == A::from(0xFF) {
      if fa.is_inf() && fb.is_inf() {
          return (Float::default_nan(), Exception::invalid())
      }

      if fa.is_nan() || fb.is_nan() {
          return propagate_nan(fx, fy);
      }

      return (Float::infinite(sign), Exception::none());
  }
  let sig = sig_a - sig_b;

  if sig == A::from(0) {
      return (
          Float::zero(mode == RoundingMode::Min),
          Exception::none(),
      );
  }
  let (exp, sig) = normalize_subnormal(sig, A::from(5));
  let exp = B::try_from(fa.exp()).unwrap() + exp;

  pack(sign, exp, sig, mode)
}
*/

pub(crate) fn sub_impl<A, B, D, F>(fx: Float<A>, fy: Float<A>, mode: RoundingMode) -> (Float<A>, Exception)     
where Float<A>: FloatConstant<A> + FloatFormat<A>,
A : TryFrom<B, Error=D> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + From<u8> + From<bool> + LeadingZeros + BitWidth + Clone + Copy,
B : TryFrom<A, Error=F> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + From<u8> + PartialOrd + Clone + Copy,
D : std::fmt::Debug,
F : std::fmt::Debug,
{
  let (exp_diff, fa, fb, sign): (_, _, _, bool) = if fx.exp() < fy.exp() {
      (fy.exp() - fx.exp(), fy, fx, !fx.sign())
  } else if fx.exp() > fy.exp() {
      (fx.exp() - fy.exp(), fx, fy, fx.sign())
  } else {
      if fx.sig() < fy.sig() {
          (A::from(0), fy, fx, !fx.sign())
      } else {
          (A::from(0), fx, fy, fx.sign())
      }
  };
  let sig_a = if fa.exp() == A::from(0) { fa.sig() << A::from(1) } else { fa.sig() | Float::<A>::hidden_bit() };
  let sig_b = if fb.exp() == A::from(0) { fb.sig() << A::from(1) } else { fb.sig() | Float::<A>::hidden_bit() };
  let sig_a = sig_a << round_width::<A>();
  let sig_b = sig_b << round_width::<A>();
  let sig_b = right_shift(sig_b, exp_diff);

  // there is no need to calculate fb.exp == 0xFF
  // because fa.exp >= fb.exp must be true.
  if is_max_exp(fa) || is_max_exp(fb) {
    if fa.is_inf() && fb.is_inf() {
        return (Float::default_nan(), Exception::invalid())
    }
    if fa.is_nan() || fb.is_nan() {
        return propagate_nan(fx, fy);
    }
    return (Float::infinite(sign), Exception::none());
  }

  let sig = sig_a - sig_b;

  if sig == A::from(0) {
      return (
          Float::zero(mode == RoundingMode::Min),
          Exception::none(),
      );
  }
  let (exp, sig) = normalize_subnormal(sig, A::width() - (Float::<A>::sig_width() + round_width() + A::from(1)));
  let exp = B::try_from(fa.exp()).unwrap() + exp;

  pack(sign, exp, sig, mode)
}