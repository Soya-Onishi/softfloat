extern crate either;

use std::ops::{Add, Sub, Mul, BitAnd, BitOr, Not, Shl, Shr, Neg};
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use either::Either;
use crate::{
    Float,
    RoundingMode,
    Exception
};
use crate::constant::*;
use crate::util::*;

impl Mul for Float<u32> {
  type Output = Self;

  fn mul(self, other: Self) -> Self {
    self.mul_with_mode(other, RoundingMode::NearEven).0
  }
}

pub(crate) fn mul_impl<A, B, C, D, E, F>(fx: Float<A>, fy: Float<A>, mode: RoundingMode) -> (Float<A>, Exception)
    where Float<A>: FloatConstant<A> + FloatFormat<A>,          
          A : Extends<Output=C> + TryFrom<B, Error=D> + TryFrom<C, Error=E> + PartialEq + PartialOrd + Add<Output=A> + Sub<Output=A> + Mul<Output=A> + Shl<Output=A> + Shr<Output=A> + BitAnd<Output=A> + BitOr<Output=A> + Not<Output=A> + From<u8> + From<bool> + LeadingZeros + BitWidth + Clone + Copy,
          B : TryFrom<A, Error=F> + Add<Output=B> + Sub<Output=B> + Neg<Output=B> + From<u8> + PartialOrd + Clone + Copy,
          C: From<u8> + From<bool> + PartialEq + PartialOrd + Sub<Output=C> + Mul<Output=C> + Shr<Output=C> + Shl<Output=C> + BitAnd<Output=C> + BitOr<Output=C> + BitWidth + Clone + Copy,
          D : std::fmt::Debug,
          E : std::fmt::Debug,
          F : std::fmt::Debug,
{   
    let try_mul_inf_or_nan = |sign: bool, f: Float<A>, g: Float<A>| -> Option<(Float<A>, Exception)> {        
        if f.is_inf() && !g.is_nan() { 
            if g.is_zero() { Some((Float::default_nan(), Exception::invalid())) }
            else { Some((Float::<A>::infinite(sign), Exception::none())) }
        } else if f.is_nan() || g.is_nan() {
            Some(propagate_nan(f, g))
        } else {
            None
        }
    };

    let make_exp_sig = |sign: bool, f: Float<A>| -> Either<(Float<A>, Exception), (B, A)> {
        if f.exp() != A::from(0) {
            Either::Right((B::try_from(f.exp()).unwrap(), f.sig() | Float::<A>::hidden_bit()))
        } else if f.sig() == A::from(0) {
            Either::Left((Float::zero(sign), Exception::none()))
        } else {
            // 1.0 * (2 ^ -127) cannot be represented as normal 
            // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
            // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
            // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
            let shamt = f.sig().count_leading_zeros() - A::from(8);
            Either::Right((-B::try_from(shamt).unwrap() + B::from(1), f.sig() << shamt))
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

fn ext_mul<A, B, C, D, E>(sig_a: A, exp_a: B, sig_b: A, exp_b: B) -> (B, A) 
    where Float<A>: FloatConstant<A>,          
          A: Extends<Output=C> + TryFrom<C, Error=D> + From<u8> + Sub<Output=A> + Shl<Output=A> + Clone + Copy,
          B: Add<Output=B> + Sub<Output=B> + TryFrom<A, Error=E> + Clone + Copy,
          C: From<u8> + From<bool> + PartialEq + PartialOrd + Sub<Output=C> + Mul<Output=C> + Shr<Output=C> + Shl<Output=C> + BitAnd<Output=C> + BitOr<Output=C> + BitWidth + Clone + Copy,
          D: std::fmt::Debug,
          E: std::fmt::Debug,
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
    let mul_result = A::extend(sig_a) * A::extend(sig_b);
    let exp = exp_a + exp_b - B::try_from(Float::<A>::bias()).unwrap();
    let shifted = right_shift(mul_result, A::extend(Float::<A>::sig_width() - round_width()));

    (exp, A::try_from(shifted).unwrap())
  }
