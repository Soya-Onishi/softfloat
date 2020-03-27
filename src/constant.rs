use crate::{Float, FloatFormat};

use std::convert::TryFrom;

pub(crate) fn sig<T>(v: Float<T>) -> T::BaseType
  where T: FloatFormat
{
    let v = T::BaseType::from(v);
    let mask = (T::BaseType::from(1) << T::sig_width()) - T::BaseType::from(1);
    T::BaseType::from(v) & mask
}

pub(crate) fn exp<T>(v: T) -> T::BaseType
  where T: FloatFormat
{
    let v = T::BaseType::from(v);
    let shifted = T::BaseType::from(v) >> T::BaseType::try_from(T::sig_width()).unwrap();
    let mask = (T::BaseType::from(1) << T::BaseType::try_from(T::exp_width()).unwrap()) - T::BaseType::from(1);
    shifted & mask
}

pub(crate) fn sign<T>(v: Float<T>) -> bool
  where T: FloatFormat
{
    let v = T::BaseType::from(v);
    let mask = T::BaseType::from(1) << T::BaseType::try_from((T::exp_width() + T::sig_width())).unwrap();
    (T::BaseType::from(v) & mask) != T::BaseType::from(0)
}

pub(crate) fn bias<T: FloatFormat>() -> T::BaseType {
    let exp_width = T::BaseType::try_from(T::exp_width()).unwrap();
    (T::BaseType::from(1) << (exp_width - T::BaseType::from(1))) - T::BaseType::from(1)
}

pub(crate) fn is_nan<T>(v: Float<T>) -> bool
  where T: FloatFormat
{
    is_max_exp(v) && !is_zero_sig(v)
}

pub(crate) fn is_signal_nan<T>(v: Float<T>) -> bool
  where T: FloatFormat
{
    is_max_exp(v) && !is_zero_sig(v) && is_signal_sig(v)
}

pub(crate) fn is_inf<T>(v: Float<T>) -> bool
  where T: FloatFormat
{
    is_max_exp(v) && is_zero_sig(v)
}

pub(crate) fn is_zero<T>(v: Float<T>) -> bool
    where T: FloatFormat
{
    is_zero_exp(v) && is_zero_sig(v)
}

pub(crate) fn is_subnormal<T>(v: Float<T>) -> bool
    where T: FloatFormat
{
    is_zero_exp(v) && !is_zero_sig(v)
}

/*
fn new(sign: bool, exp: T, sig: T) -> Float<T> {
    let sign = T::from(sign) << (Float::<T>::sig_width() + Float::<T>::exp_width());
    let exp_mask = (T::from(1) << Float::<T>::exp_width()) - T::from(1);
    let exp = (exp & exp_mask) << Float::<T>::sig_width();
    let sig_mask = (T::from(1) << Float::<T>::sig_width()) - T::from(1);
    let sig = sig & sig_mask;

    Float::<T> { v: sign | exp | sig }
}

fn construct(v: T) -> Float<T> { Float::<T> { v } }
*/

pub(crate) fn default_nan<T>() -> Float<T>
    where T: FloatFormat
{
    let sig = T::BaseType::from(1) << (T::BaseType::try_from(T::sig_width()).unwrap() - T::BaseType::from(1));
    Float::<T>::new(true, exp_max::<T>(), sig)
}

pub(crate) fn zero<T>(sign: bool) -> Float<T>
    where T: FloatFormat
{
    Float::<T>::new(sign, T::BaseType::from(0), T::BaseType::from(0))
}

pub(crate) fn infinite<T>(sign: bool) -> Float<T>
    where T: FloatFormat
{
    Float::<T>::new(sign, exp_max::<T>(), T::BaseType::from(0))
}

pub(crate) fn hidden_bit<T>() -> T::BaseType
    where T: FloatFormat
{
    T::BaseType::from(1) << T::BaseType::try_from(T::sig_width()).unwrap()
}

pub(crate) fn signal_bit<T>() -> T::BaseType
    where T:FloatFormat
{
    T::BaseType::from(1) << (T::BaseType::try_from(T::sig_width()).unwrap() - T::BaseType::from(1))
}


pub(crate) fn exp_max<T>() -> T::BaseType
    where T: FloatFormat
{
    let width = T::BaseType::try_from(T::exp_width()).unwrap();
    (T::BaseType::from(1) << width) - T::BaseType::from(1)
}

pub(crate) fn sig_max<T>() -> T::BaseType
    where T: FloatFormat,
{
    let width = T::BaseType::try_from(T::sig_width()).unwrap();
    (T::BaseType::from(1) << width) - T::BaseType::from(1)
}

pub(crate) fn is_max_exp<T>(f: Float<T>) -> bool
    where T: FloatFormat,
{
    exp(f) == exp_max()
}

pub(crate) fn is_zero_exp<T>(f: Float<T>) -> bool
    where T: FloatFormat,
{
    exp(f) == T::BaseType::from(0)
}

pub(crate) fn is_zero_sig<T>(f: Float<T>) -> bool
    where T: FloatFormat,
{
    sig(f) == T::BaseType::from(0)
}

pub(crate) fn is_signal_sig<T>(f: Float<T>) -> bool
    where T: FloatFormat
{
    let mask = T::BaseType::from(1) << (T::sig_width() - T::BaseType::from(1));
    (sig(f) & mask) == T::BaseType::from(0)
}