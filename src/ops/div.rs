extern crate either;

use std::ops::{Add, Sub, Div, Rem, BitAnd, BitOr, Not, Shl, Shr, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use either::Either;
use crate::{
    Float,
    Exception,
    RoundingMode
};
use crate::constant::*;
use crate::util::*;    

impl Div for Float<u32> {
  type Output = Self;

  fn div(self, other: Self) -> Self {
    self.div_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn div_impl<A, B, C>(fx: Float<A>, fy: Float<A>, mode: RoundingMode) -> (Float<A>, Exception) 
where Float<A>: FloatConstant<A> + FloatFormat<A>,
A : Extends<Output=C> + TryFrom<B> + TryFrom<C> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + From<u8> + From<bool> + TryFrom<C> + LeadingZeros + BitWidth + Clone + Copy,
B : TryFrom<A> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + From<u8> + PartialOrd + Clone + Copy,
C : Shl<Output=C> + Div<Output=C> + Rem<Output=C> + From<u8> + PartialEq + Clone + Copy,
<A as TryFrom<B>>::Error: std::fmt::Debug,
<A as TryFrom<C>>::Error: std::fmt::Debug,
B::Error: std::fmt::Debug,
{
  let sign = fx.sign() ^ fy.sign();

  if fx.is_nan() || fy.is_nan() { return propagate_nan(fx, fy); }
  if fx.is_inf() { 
      if fy.is_inf() { return (Float::<A>::default_nan(), Exception::invalid()); }
      else { return (Float::<A>::infinite(sign), Exception::none()); }
  }        
  if fy.is_inf() { return (Float::<A>::zero(sign), Exception::none()); }
  if fy.is_zero() { 
      if fx.is_zero() { return (Float::<A>::default_nan(), Exception::invalid()); }
      else { return (Float::<A>::infinite(sign), Exception::infinite()); }
  }

  let (exp_a, sig_a) = match make_exp_sig::<A, B>(sign, fx) {
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

fn ext_div<A, B, C>(sig_a: A, exp_a: B, sig_b: A, exp_b: B) -> (B, A) 
    where Float<A>: FloatConstant<A>,
          A : Extends<Output=C> + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + BitOr<Output=A> + From<u8> + From<bool> + TryFrom<C> + LeadingZeros + BitWidth + Clone + Copy,
          B : TryFrom<A> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + Clone + Copy,
          C : Shl<Output=C> + Div<Output=C> + Rem<Output=C> + From<u8> + PartialEq + Clone + Copy,
          A::Error : std::fmt::Debug,
          B::Error : std::fmt::Debug,
{
    let shamt = Float::<A>::sig_width() + round_width::<A>();
    let dividend = A::extend(sig_a) << A::extend(shamt);
    let divisor = A::extend(sig_b);

    let sig = A::try_from(dividend / divisor).unwrap();
    let is_there_rem = (dividend % divisor) != C::from(0);
    let (mod_exp, sig) = normalize_subnormal(sig, A::width() - (Float::<A>::sig_width() + round_width() + A::from(1)));
    let exp = exp_a - exp_b + B::try_from(Float::<A>::bias()).unwrap();
    let exp = exp + mod_exp;

    (exp, sig | A::from(is_there_rem))
  }