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

pub(crate) fn div_impl<A, B, C, D, E, F>(fx: Float<A>, fy: Float<A>, mode: RoundingMode) -> (Float<A>, Exception) 
where Float<A>: FloatConstant<A> + FloatFormat<A>,
A : Extends<Output=C> + TryFrom<B, Error=D> + TryFrom<C, Error=F> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + From<u8> + From<bool> + TryFrom<C> + LeadingZeros + BitWidth + Clone + Copy,
B : TryFrom<A, Error=E> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + From<u8> + PartialOrd + Clone + Copy,
C : Shl<Output=C> + Div<Output=C> + Rem<Output=C> + From<u8> + PartialEq + Clone + Copy,
D : std::fmt::Debug,
E : std::fmt::Debug,
F : std::fmt::Debug,
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

  // TODO: 
  //   This is copy of make_exp_sig in mul_impl
  //   Make refactoring.
  let make_exp_sig = |sign: bool, f: Float<A>| -> Either<(Float<A>, Exception), (B, A)> {
      if f.exp() != A::from(0) {
          Either::Right((B::try_from(f.exp()).unwrap(), f.sig() | Float::<A>::hidden_bit()))
      } else if f.sig() == A::from(0) {
          Either::Left((Float::<A>::zero(sign), Exception::none()))
      } else {
          // 1.0 * (2 ^ -127) cannot be represented as normal 
          // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
          // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
          // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
          let shamt = f.sig().count_leading_zeros() - A::from(8);
          Either::Right((-B::try_from(shamt).unwrap() + B::from(1), f.sig() << shamt))
      }
  };

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

fn ext_div<A, B, C, D, E>(sig_a: A, exp_a: B, sig_b: A, exp_b: B) -> (B, A) 
    where Float<A>: FloatConstant<A>,
          A : Extends<Output=C> + Add<Output=A> + Sub<Output=A> + Shl<Output=A> + BitOr<Output=A> + From<u8> + From<bool> + TryFrom<C, Error=D> + LeadingZeros + Clone + Copy,
          B : TryFrom<A, Error=E> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + Clone + Copy,
          C : Shl<Output=C> + Div<Output=C> + Rem<Output=C> + From<u8> + PartialEq + Clone + Copy,
          D : std::fmt::Debug,
          E : std::fmt::Debug,
{
    let shamt = Float::<A>::sig_width() + round_width::<A>();
    let dividend = A::extend(sig_a) << A::extend(shamt);
    let divisor = A::extend(sig_b);

    let sig = A::try_from(dividend / divisor).unwrap();
    let is_there_rem = (dividend % divisor) != C::from(0);
    let (mod_exp, sig) = normalize_subnormal(sig, A::from(5));
    let exp = exp_a - exp_b + B::try_from(Float::<A>::bias()).unwrap();
    let exp = exp + mod_exp;

    (exp, sig | A::from(is_there_rem))
  }