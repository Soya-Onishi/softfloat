use crate::Float;

use std::ops::{Add, Sub, BitAnd, BitOr, Shl, Shr};
use std::cmp::PartialEq;

pub trait FloatConstant<T> {
  fn sig_width() -> T;
  fn exp_width() -> T;  
  fn bias() -> T;
  fn width() -> T;
}

impl FloatConstant<u32> for Float<u32> {
  fn sig_width() -> u32 { 23 }
  fn exp_width() -> u32 { 8 }
  fn bias() -> u32 { 127 }
  fn width() -> u32 { 32 }
}

pub trait FloatFormat<T> {
  fn sig(self) -> T;  
  fn exp(self) -> T;
  fn sign(self) -> bool;
  fn is_nan(self) -> bool;
  fn is_signal_nan(self) -> bool;
  fn is_inf(self) -> bool;
  fn is_zero(self) -> bool;
  fn is_subnormal(self) -> bool;  
  fn new(sign: bool, exp: T, sig: T) -> Float<T>;  
  fn construct(v: T) -> Float<T>;
  fn default_nan() -> Float<T>;
  fn zero(sign: bool) -> Float<T>;
  fn infinite(sign: bool) -> Float<T>;
  fn hidden_bit() -> T;  
  fn signal_bit() -> T;
}

impl<T> FloatFormat<T> for Float<T> 
  where Float<T>: FloatConstant<T>,
        T: From<u8> + From<bool> + Add<Output=T> + Sub<Output=T> + Shr<Output=T> + Shl<Output=T> + BitAnd<Output=T> + BitOr<Output=T> + PartialEq + Clone + Copy
{
  fn sig(self) -> T {
    let mask = (T::from(1) << Float::<T>::sig_width()) - T::from(1);
    self.v & mask
  }
  
  fn exp(self) -> T {
    let shifted = self.v >> Float::<T>::sig_width();
    let mask = (T::from(1) << Float::<T>::exp_width()) - T::from(1);
    shifted & mask
  }

  fn sign(self) -> bool {
    let mask = T::from(1) << (Float::<T>::exp_width() + Float::<T>::sig_width());
    (self.v & mask) != T::from(0)
  }

  fn is_nan(self) -> bool {    
    is_max_exp(self) && !is_zero_sig(self)    
  }

  fn is_signal_nan(self) -> bool {
    is_max_exp(self) && !is_zero_sig(self) && is_signal_sig(self)
  }
  
  fn is_inf(self) -> bool {
    is_max_exp(self) && is_zero_sig(self) 
  }

  fn is_zero(self) -> bool {
    is_zero_exp(self) && is_zero_sig(self)      
  }

  fn is_subnormal(self) -> bool {
    is_zero_exp(self) && !is_zero_sig(self)      
  }
  
  fn new(sign: bool, exp: T, sig: T) -> Float<T> {    
    let sign = T::from(sign) << (Float::<T>::sig_width() + Float::<T>::exp_width());
    let exp_mask = (T::from(1) << Float::<T>::exp_width()) - T::from(1);
    let exp = (exp & exp_mask) << Float::<T>::sig_width();
    let sig_mask = (T::from(1) << Float::<T>::sig_width()) - T::from(1);
    let sig = sig & sig_mask;

    Float::<T> { v: sign | exp | sig }
  }

  fn construct(v: T) -> Float<T> { Float::<T> { v } }

  fn default_nan() -> Float<T> {
    let sig = T::from(1) << (Float::<T>::sig_width() - T::from(1));
    Float::<T>::new(true, exp_max::<T>(), sig)
  }

  fn zero(sign: bool) -> Float<T> {
    Float::<T>::new(sign, T::from(0), T::from(0))
  }

  fn infinite(sign: bool) -> Float<T> {
    Float::<T>::new(sign, exp_max::<T>(), T::from(0))
  }

  fn hidden_bit() -> T {
    T::from(1) << Float::<T>::sig_width()
  }  

  fn signal_bit() -> T {
    T::from(1) << (Float::<T>::sig_width() - T::from(1))
  }
}

pub(crate) fn exp_max<T>() -> T 
  where Float<T>: FloatFormat<T> + FloatConstant<T>, 
        T: Sub<Output=T> + Shl<Output=T> + From<u8>
{
  let width = Float::<T>::exp_width();
  (T::from(1) << width) - T::from(1)
}

pub(crate) fn sig_max<T>() -> T 
  where Float<T>: FloatFormat<T> + FloatConstant<T>,
        T: Sub<Output=T> + Shl<Output=T> + From<u8> + From<bool>
{
  let width = Float::<T>::sig_width();
  (T::from(1) << width) - T::from(1)
}

fn is_max_exp<T>(f: Float<T>) -> bool
  where Float<T>: FloatFormat<T> + FloatConstant<T>,
        T: Sub<Output=T> + Shl<Output=T> + From<u8> + PartialEq
{
  f.exp() == exp_max::<T>()
}

fn is_zero_exp<T>(f: Float<T>) -> bool
where Float<T>: FloatFormat<T>,
T: Sub<Output=T> + Shl<Output=T> + From<u8> + PartialEq
{
  f.exp() == T::from(0)
}

fn is_zero_sig<T>(f: Float<T>) -> bool
where Float<T>: FloatFormat<T>,
T: Sub<Output=T> + Shl<Output=T> + From<u8> + PartialEq
{
  f.sig() == T::from(0)
}

fn is_signal_sig<T>(f: Float<T>) -> bool
  where Float<T>: FloatFormat<T> + FloatConstant<T>,
  T: Sub<Output=T> + Shl<Output=T> + From<u8> + PartialEq + BitAnd<Output=T>
{ 
  let mask = T::from(1) << (Float::<T>::sig_width() - T::from(1));
  (f.sig() & mask) == T::from(0)
}