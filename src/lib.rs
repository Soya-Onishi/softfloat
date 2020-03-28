extern crate either;

mod constant;
mod exception;
mod ops;
mod util;

use crate::ops::{add::add_impl, div::div_impl, mul::mul_impl, sub::sub_impl};
use crate::util::{BitWidth, LeadingZeros};
use exception::Exception;
use std::cmp::{PartialEq, PartialOrd};
use std::convert::TryFrom;
use std::ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use either::Either;

#[derive(Clone, Copy, PartialEq)]
pub enum RoundingMode {
    NearEven,
    NearMax,
    Zero,
    Max,
    Min,
}

#[derive(Clone, Copy)]
pub struct Float<T> {
    v: T,
}

impl<T> Float<T>
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
    pub fn new(sign: bool, exp: T::BaseType, sig: T::BaseType) -> Float<T> {
        let sign = T::BaseType::from(sign) << (T::exp_width() + T::sig_width());
        let exp_mask = (T::BaseType::from(1) << T::exp_width()) - T::BaseType::from(1);
        let exp = (exp & exp_mask) << T::sig_width();
        let sig_mask = (T::BaseType::from(1) << T::sig_width()) - T::BaseType::from(1);
        let sig = sig & sig_mask;

        T::construct(sign | exp | sig)
    }

    pub fn add_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
        if self.sign() ^ other.sign() {
            sub_impl(self, other, mode)
        } else {
            add_impl(self, other, mode)
        }
    }

    pub fn sub_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
        if self.sign() ^ other.sign() {
            add_impl(self, other, mode)
        } else {
            sub_impl(self, other, mode)
        }
    }

    pub fn mul_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
        mul_impl(self, other, mode)
    }

    pub fn div_with_mode(self, other: Float<T>, mode: RoundingMode) -> (Float<T>, Exception) {
        div_impl(self, other, mode)
    }

    fn propagate_nan(a: Float<T>, b: Float<T>) -> (Float<T>, Exception) {
        let make_qnan =
            |f: Float<T>| -> Float<T> { Float::<T>::new(f.sign(), f.exp(), f.sig() | Float::<T>::signal_bit()) };

        let a_sig_qnan = make_qnan(a);
        let b_sig_qnan = make_qnan(b);

        let nan = if a.is_nan() { a_sig_qnan } else { b_sig_qnan };

        let exception = if a.is_signal_nan() | b.is_signal_nan() {
            Exception::invalid()
        } else {
            Exception::none()
        };

        (nan, exception)
    }

    fn pack(
        sign: bool,
        exp: T::SignedType,
        sig: T::BaseType,
        mode: RoundingMode,
    ) -> (Float<T>, Exception) {
        let is_near_even = mode == RoundingMode::NearEven;
        let round_value = Float::<T>::make_round_value(sign, mode);

        let (exp, sig, is_underflow) = if exp <= T::SignedType::from(0) {
            let is_sufficient_size = (sig + round_value)
                >= (T::BaseType::from(1)
                << (T::sig_width() + Float::<T>::round_width() + T::BaseType::from(1)));
            let is_neg_exp = exp < T::SignedType::from(0);
            let is_tiny = !is_sufficient_size || is_neg_exp;
            let shamt = T::BaseType::try_from(-exp).unwrap() + T::BaseType::from(1);
            let sig = Float::<T>::right_shift(sig, shamt);
            let round_bits = (sig & Float::<T>::round_mask()) != T::BaseType::from(0);

            (T::BaseType::from(0), sig, is_tiny && round_bits)
        } else {
            (T::BaseType::try_from(exp).unwrap(), sig, false)
        };

        let (exp, sig) = match Float::<T>::normalize(sign, exp, sig, round_value) {
            either::Left(result) => return result,
            either::Right(pair) => pair,
        };

        let round_bits = sig & Float::<T>::round_mask();
        let sig = sig + round_value;

        let (exp, sig) = match Float::<T>::normalize(sign, exp, sig, round_value) {
            either::Left(result) => return result,
            either::Right(pair) => pair,
        };

        let exception = if round_bits != T::BaseType::from(0) {
            let underflow = if is_underflow {
                Exception::underflow()
            } else {
                Exception::none()
            };

            underflow | Exception::inexact()
        } else {
            Exception::none()
        };

        let sig = (sig >> Float::<T>::round_width())
            & !(T::BaseType::from((round_bits == T::BaseType::from(0x4)) && is_near_even))
            & Float::<T>::sig_max();

        (Float::<T>::new(sign, exp, sig), exception)
    }

    fn round_width() -> T::BaseType {
        T::BaseType::from(3)
    }

    fn round_mask() -> T::BaseType {
        (T::BaseType::from(1) << Float::<T>::round_width()) - T::BaseType::from(1)
    }

    fn right_shift<A>(sig: A, shamt: A) -> A
    where
        A: Clone
        + Copy
        + BitWidth
        + From<bool>
        + From<u8>
        + PartialEq
        + PartialOrd
        + Sub<Output=A>
        + Shr<Output=A>
        + Shl<Output=A>
        + BitAnd<Output=A>
        + BitOr<Output=A>
    {
        if shamt >= A::width() {
            A::from(sig != A::from(0))
        } else {
            let shifted = sig >> shamt;
            let sticky =
                (sig & ((A::from(1) << shamt) - A::from(1))) != A::from(0);

            shifted | A::from(sticky)
        }
    }

    fn make_round_value(sign: bool, mode: RoundingMode) -> T::BaseType {
        match mode {
            RoundingMode::NearEven => T::BaseType::from(0x04),
            RoundingMode::NearMax => T::BaseType::from(0x04),
            RoundingMode::Zero => T::BaseType::from(0x00),
            RoundingMode::Max => {
                if sign {
                    T::BaseType::from(0x00)
                } else {
                    T::BaseType::from(0x07)
                }
            }
            RoundingMode::Min => {
                if sign {
                    T::BaseType::from(0x07)
                } else {
                    T::BaseType::from(0x00)
                }
            }
        }
    }

    fn normalize(
        sign: bool,
        exp: T::BaseType,
        sig: T::BaseType,
        round_inc: T::BaseType,
    ) -> Either<(Float<T>, Exception), (T::BaseType, T::BaseType)> {
        match (exp, sig) {
            (exp, _) if exp >= Float::<T>::exp_max() => {
                let infinite = if round_inc == T::BaseType::from(0) {
                    Float::<T>::new(sign, Float::<T>::exp_max() - T::BaseType::from(1), Float::<T>::sig_max())
                } else {
                    Float::<T>::new(sign, Float::<T>::exp_max(), T::BaseType::from(0))
                };

                either::Left((infinite, Exception::overflow() | Exception::inexact()))
            }
            (exp, sig)
            if sig
                >= (T::BaseType::from(1) << (T::sig_width() + Float::<T>::round_width() + T::BaseType::from(1))) =>
                {
                    Float::<T>::normalize(
                        sign,
                        exp + T::BaseType::from(1),
                        Float::<T>::right_shift(sig, T::BaseType::from(1)),
                        round_inc,
                    )
                }
            // if exp == 0 && sig >= 0x0400_0000 means 10.0 * 2^-127 => 1.0 * 2^-126
            (exp, sig)
            if exp == T::BaseType::from(0)
                && sig
                >= (T::BaseType::from(1)
                << (T::sig_width() + Float::<T>::round_width())) =>
                {
                    Float::<T>::normalize(sign, T::BaseType::from(1), sig, round_inc)
                }
            (exp, sig) => either::Right((exp, sig)),
        }
    }

    fn normalize_subnormal(sig: T::BaseType, bias: T::BaseType) -> (T::SignedType, T::BaseType) {
        let shamt = sig.count_leading_zeros() - bias;
        (-(T::SignedType::try_from(shamt).unwrap()), sig << shamt)
    }

    fn make_exp_sig(sign: bool, f: Float<T>) -> Either<(Float<T>, Exception), (T::SignedType, T::BaseType)> {
        if f.exp() != T::BaseType::from(0) {
            Either::Right((
                T::SignedType::try_from(f.exp()).unwrap(),
                f.sig() | Float::<T>::hidden_bit(),
            ))
        } else if f.sig() == T::BaseType::from(0) {
            Either::Left((Float::<T>::zero(sign), Exception::none()))
        } else {
            // 1.0 * (2 ^ -127) cannot be represented as normal
            // because 2 ^ -127  means exp == 0 and when exp is 0, there is no hidden bit(ケチ表現).
            // That's why we need to treat 0x0040_0000 as 1.0 * (2 ^ -127) instead of 1.0 * (2 ^ -128).
            // Also, -(shamt as i32) + 1 is corrent and -(shamt as i32) is invalid.
            let bias = T::BaseType::width() - (T::sig_width() + T::BaseType::from(1));
            let shamt = f.sig().count_leading_zeros() - bias;
            Either::Right((
                -T::SignedType::try_from(shamt).unwrap() + T::SignedType::from(1),
                f.sig() << shamt,
            ))
        }
    }

    fn sig(self) -> T::BaseType {
        let mask = (T::BaseType::from(1) << T::sig_width()) - T::BaseType::from(1);
        T::BaseType::from(self.v) & mask
    }

    fn exp(self) -> T::BaseType {
        let shamt = T::sig_width();
        let shifted = T::BaseType::from(self.v) >> shamt;
        let mask = (T::BaseType::from(1) << T::exp_width()) - T::BaseType::from(1);
        shifted & mask
    }

    fn sign(self) -> bool {
        let mask = T::BaseType::from(1) << (T::exp_width() + T::sig_width());
        (T::BaseType::from(self.v) & mask) != T::BaseType::from(0)
    }

    fn bias() -> T::BaseType {
        (T::BaseType::from(1) << (T::exp_width() - T::BaseType::from(1))) - T::BaseType::from(1)
    }

    pub fn is_nan(self) -> bool {
        self.is_max_exp() && !self.is_zero_sig()
    }

    pub fn is_signal_nan(self) -> bool {
        self.is_max_exp() && !self.is_zero_sig() && self.is_signal_sig()
    }

    pub fn is_inf(self) -> bool {
        self.is_max_exp() && self.is_zero_sig()
    }

    pub fn is_zero(self) -> bool {
        self.is_zero_exp() && self.is_zero_sig()
    }

    pub fn is_subnormal(self) -> bool {
        self.is_zero_exp() && !self.is_zero_sig()
    }

    pub fn default_nan() -> Float<T> {
        let sig = T::BaseType::from(1)
            << (T::BaseType::try_from(T::sig_width()).unwrap() - T::BaseType::from(1));
        Float::<T>::new(true, Float::<T>::exp_max(), sig)
    }

    pub fn zero(sign: bool) -> Float<T> {
        Float::<T>::new(sign, T::BaseType::from(0), T::BaseType::from(0))
    }

    pub fn infinite(sign: bool) -> Float<T> {
        Float::<T>::new(sign, Float::<T>::exp_max(), T::BaseType::from(0))
    }

    fn hidden_bit() -> T::BaseType {
        T::BaseType::from(1) << T::BaseType::try_from(T::sig_width()).unwrap()
    }

    fn signal_bit() -> T::BaseType {
        T::BaseType::from(1) << (T::BaseType::try_from(T::sig_width()).unwrap() - T::BaseType::from(1))
    }

    fn exp_max() -> T::BaseType {
        let width = T::BaseType::try_from(T::exp_width()).unwrap();
        (T::BaseType::from(1) << width) - T::BaseType::from(1)
    }

    fn sig_max() -> T::BaseType {
        let width = T::BaseType::try_from(T::sig_width()).unwrap();
        (T::BaseType::from(1) << width) - T::BaseType::from(1)
    }

    fn is_max_exp(self) -> bool {
        self.exp() == Float::<T>::exp_max()
    }

    fn is_zero_exp(self) -> bool {
        self.exp() == T::BaseType::from(0)
    }

    fn is_zero_sig(self) -> bool {
        self.sig() == T::BaseType::from(0)
    }

    fn is_signal_sig(self) -> bool {
        let mask = T::BaseType::from(1) << (T::sig_width() - T::BaseType::from(1));
        (self.sig() & mask) == T::BaseType::from(0)
    }
}

pub trait FloatFormat
where
    Self: Clone + Copy,
    <Self::BaseType as TryFrom<usize>>::Error: std::fmt::Debug,
    <Self::BaseType as TryFrom<Self::ExtendedType>>::Error: std::fmt::Debug,
    <Self::BaseType as TryFrom<Self::SignedType>>::Error: std::fmt::Debug,
    <Self::SignedType as TryFrom<Self::BaseType>>::Error: std::fmt::Debug,
{
    type BaseType: Clone
        + Copy
    + From<u8>
    + From<bool>
    + From<Self>
        + TryFrom<usize>
        + TryFrom<Self::ExtendedType>
    + TryFrom<Self::SignedType>
        + LeadingZeros
        + BitWidth
        + Add<Output = Self::BaseType>
        + Sub<Output = Self::BaseType>
        + Mul<Output = Self::BaseType>
        + Div<Output = Self::BaseType>
        + Rem<Output = Self::BaseType>
        + Shr<Output = Self::BaseType>
        + Shl<Output = Self::BaseType>
        + BitAnd<Output = Self::BaseType>
        + BitOr<Output = Self::BaseType>
    + Not<Output = Self::BaseType>
        + PartialEq
        + PartialOrd;

    type ExtendedType: Clone
        + Copy
        + From<u8>
    + From<bool>
    + From<Self::BaseType>
        + BitWidth
        + Add<Output = Self::ExtendedType>
        + Sub<Output = Self::ExtendedType>
        + Mul<Output = Self::ExtendedType>
        + Div<Output = Self::ExtendedType>
        + Rem<Output = Self::ExtendedType>
        + Shr<Output = Self::ExtendedType>
        + Shl<Output = Self::ExtendedType>
        + BitAnd<Output = Self::ExtendedType>
        + BitOr<Output = Self::ExtendedType>
        + PartialEq
        + PartialOrd;

    type SignedType: Clone
        + Copy
        + From<u8>
    + From<bool>
    + TryFrom<Self::BaseType>
        + Add<Output = Self::SignedType>
        + Sub<Output = Self::SignedType>
        + Mul<Output = Self::SignedType>
        + Div<Output = Self::SignedType>
        + Rem<Output = Self::SignedType>
        + Neg<Output = Self::SignedType>
        + Shr<Output = Self::SignedType>
        + Shl<Output = Self::SignedType>
        + BitAnd<Output = Self::SignedType>
        + BitOr<Output = Self::SignedType>
        + PartialEq
        + PartialOrd;

    fn construct(v: Self::BaseType) -> Float<Self>;
    fn sig_width() -> Self::BaseType;
    fn exp_width() -> Self::BaseType;
}

#[derive(Clone, Copy)]
struct Float16(u16);
#[derive(Clone, Copy)]
struct Float32(u32);
#[derive(Clone, Copy)]
struct Float64(u64);

impl FloatFormat for Float16 {
    type BaseType = u16;
    type ExtendedType = u32;
    type SignedType = i16;

    fn construct(n: u16) -> Float<Float16> {
        Float { v: Float16(n) }
    }

    fn sig_width() -> u16 {
        10
    }
    fn exp_width() -> u16 {
        5
    }
}

impl FloatFormat for Float32 {
    type BaseType = u32;
    type ExtendedType = u64;
    type SignedType = i32;

    fn construct(n: u32) -> Float<Float32> {
        Float { v: Float32(n) }
    }

    fn sig_width() -> u32 {
        23
    }

    fn exp_width() -> u32 {
        8
    }
}

impl FloatFormat for Float64 {
    type BaseType = u64;
    type ExtendedType = u128;
    type SignedType = i64;

    fn construct(n: u64) -> Float<Float64> {
        Float { v: Float64(n) }
    }

    fn sig_width() -> u64 {
        52
    }

    fn exp_width() -> u64 {
        11
    }
}

impl From<Float16> for u16 {
    fn from(v: Float16) -> u16 {
        v.0
    }
}

impl From<Float32> for u32 {
    fn from(v: Float32) -> u32 {
        v.0
    }
}

impl From<Float64> for u64 {
    fn from(v: Float64) -> u64 {
        v.0
    }
}

#[cfg(test)]
mod test {
    extern crate regex;

    use std::convert::TryFrom;
    use crate::{Float, FloatFormat, Exception, RoundingMode};
    use crate::{Float16, Float32, Float64};
    use regex::Regex;
    use std::process::Command;
    use std::str::from_utf8;

    #[test]
    fn f32_add() -> std::io::Result<()> {
        f32_test_harness("f32_add", |a, b| a.add_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f32_sub() -> std::io::Result<()> {
        f32_test_harness("f32_sub", |a, b| a.sub_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f32_mul() -> std::io::Result<()> {
        f32_test_harness("f32_mul", |a, b| a.mul_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f32_div() -> std::io::Result<()> {
        f32_test_harness("f32_div", |a, b| a.div_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f64_add() -> std::io::Result<()> {
        f64_test_harness("f64_add", |a, b| a.add_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f64_sub() -> std::io::Result<()> {
        f64_test_harness("f64_sub", |a, b| a.sub_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f64_mul() -> std::io::Result<()> {
        f64_test_harness("f64_mul", |a, b| a.mul_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f64_div() -> std::io::Result<()> {
        f64_test_harness("f64_div", |a, b| a.div_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_add() -> std::io::Result<()> {
        f16_test_harness("f16_add", |a, b| a.add_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_sub() -> std::io::Result<()> {
        f16_test_harness("f16_sub", |a, b| a.sub_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_mul() -> std::io::Result<()> {
        f16_test_harness("f16_mul", |a, b| a.mul_with_mode(b, RoundingMode::NearEven))
    }

    #[test]
    fn f16_div() -> std::io::Result<()> {
        f16_test_harness("f16_div", |a, b| a.div_with_mode(b, RoundingMode::NearEven))
    }

    fn f16_test_harness(function: &str, op: impl Fn(Float<Float16>, Float<Float16>) -> (Float<Float16>, Exception)) -> std::io::Result<()> {
        test_harness(
            function,
            r"([a-zA-Z0-9]{4}) ([a-zA-Z0-9]{4}) ([a-zA-Z0-9]{4}) ([a-zA-Z0-9]{2})",
            op,
            |value| { u16::from_str_radix(value, 16).unwrap() }
        )
    }

    fn f32_test_harness(function: &str, op: impl Fn(Float<Float32>, Float<Float32>) -> (Float<Float32>, Exception)) -> std::io::Result<()> {
        test_harness(
            function,
            r"([a-zA-Z0-9]{8}) ([a-zA-Z0-9]{8}) ([a-zA-Z0-9]{8}) ([a-zA-Z0-9]{2})",
            op,
            |value| { u32::from_str_radix(value, 16).unwrap() }
        )
    }

    fn f64_test_harness(function: &str, op: impl Fn(Float<Float64>, Float<Float64>) -> (Float<Float64>, Exception)) -> std::io::Result<()> {
        test_harness(
            function,
            r"([a-zA-Z0-9]{16}) ([a-zA-Z0-9]{16}) ([a-zA-Z0-9]{16}) ([a-zA-Z0-9]{2})",
            op,
            |value| { u64::from_str_radix(value, 16).unwrap() }
        )
    }

    fn test_harness<T>(
        function: &str,
        regex_format: &str,
        op: impl Fn(Float<T>, Float<T>) -> (Float<T>, Exception),
        into_value: impl Fn(&str) -> T::BaseType,
    ) -> std::io::Result<()>
    where
        T: FloatFormat,
        T::BaseType: TryFrom<usize> + TryFrom<T::ExtendedType> + TryFrom<T::SignedType> + From<u8> + From<bool> + From<T> + std::fmt::Debug + std::fmt::LowerHex,
        T::ExtendedType: From<u8> + From<bool> + From<T::BaseType>,
        T::SignedType: From<u8> + From<bool> + TryFrom<T::BaseType>,
        <T::BaseType as TryFrom<usize>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::ExtendedType>>::Error: std::fmt::Debug,
        <T::BaseType as TryFrom<T::SignedType>>::Error: std::fmt::Debug,
        <T::SignedType as TryFrom<T::BaseType>>::Error: std::fmt::Debug,
    {
        let output = Command::new("testfloat_gen")
            .arg(function)
            .output()?;
        assert_eq!(output.status.code(), Some(0));
        assert!(output.stderr.is_empty());
        let re = Regex::new(regex_format)
            .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())
            .map(|caps| {
                let a = into_value(caps.get(1).unwrap().as_str());
                let b = into_value(caps.get(2).unwrap().as_str());
                let c = into_value(caps.get(3).unwrap().as_str());
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = T::construct(a);
                let fb = T::construct(b);
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = op(fa, fb);
                let actual_result = T::BaseType::from(actual_result.v);

                let actual = (actual_result, actual_exception.0);
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| {
                e_result != a_result || e_exception != a_exception
            })
            .collect::<Vec<(T::BaseType, T::BaseType, (T::BaseType, u8), (T::BaseType, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>18}, {:>18}, {:>18}, {:>18}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:016x}, 0x{:016x}, 0x{:016x}, 0x{:016x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }

    /*
    fn f32_test_harness(
        function: &str,
        f: impl Fn(Float<u32>, Float<u32>) -> (Float<u32>, Exception),
    ) -> std::io::Result<()> {
        let output = Command::new("testfloat_gen")
            .arg("-precision32")
            .arg(function)
            .output()?;
        assert_eq!(output.status.code(), Some(0));
        assert!(output.stderr.is_empty());
        let re = Regex::new(r"([0-9A-Fa-f]{8}) ([0-9A-Fa-f]{8}) ([0-9A-Fa-f]{8}) ([0-9A-Fa-f]{2})")
            .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())
            .map(|caps| {
                let a = u32::from_str_radix(caps.get(1).unwrap().as_str(), 16).unwrap();
                let b = u32::from_str_radix(caps.get(2).unwrap().as_str(), 16).unwrap();
                let c = u32::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = Float::<u32>::construct(a);
                let fb = Float::<u32>::construct(b);
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = f(fa, fb);
                let actual_result = unsafe {
                    let u = f32u32 {
                        f: actual_result.into(),
                    };
                    u.u
                };

                let actual = (actual_result, actual_exception.0);
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| {
                e_result != a_result || e_exception != a_exception
            })
            .collect::<Vec<(u32, u32, (u32, u8), (u32, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>10}, {:>10}, {:>10}, {:>10}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:08x}, 0x{:08x}, 0x{:08x}, 0x{:08x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }

    fn f64_test_harness(
        function: &str,
        f: impl Fn(Float<u64>, Float<u64>) -> (Float<u64>, Exception),
    ) -> std::io::Result<()> {
        let output = Command::new("testfloat_gen")
            .arg("-precision64")
            .arg(function)
            .output()?;
        assert_eq!(output.status.code(), Some(0));
        assert!(output.stderr.is_empty());
        let re =
            Regex::new(r"([0-9A-Fa-f]{16}) ([0-9A-Fa-f]{16}) ([0-9A-Fa-f]{16}) ([0-9A-Fa-f]{2})")
                .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())
            .map(|caps| {
                let a = u64::from_str_radix(caps.get(1).unwrap().as_str(), 16).unwrap();
                let b = u64::from_str_radix(caps.get(2).unwrap().as_str(), 16).unwrap();
                let c = u64::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = Float::<u64>::construct(a);
                let fb = Float::<u64>::construct(b);
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = f(fa, fb);
                let actual_result = unsafe {
                    let u = f64u64 {
                        f: actual_result.into(),
                    };
                    u.u
                };

                let actual = (actual_result, actual_exception.0);
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| {
                e_result != a_result || e_exception != a_exception
            })
            .collect::<Vec<(u64, u64, (u64, u8), (u64, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>18}, {:>18}, {:>18}, {:>18}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:016x}, 0x{:016x}, 0x{:016x}, 0x{:016x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }

    fn f16_test_harness(
        function: &str,
        f: impl Fn(Float<u16>, Float<u16>) -> (Float<u16>, Exception),
    ) -> std::io::Result<()> {
        let output = Command::new("testfloat_gen").arg(function).output()?;
        assert_eq!(
            output.status.code(),
            Some(0),
            "status code is not 0(actual is {:?}). you need to install testfloat",
            output.status.code()
        );
        assert!(output.stderr.is_empty());
        let re = Regex::new(r"([0-9A-Fa-f]{4}) ([0-9A-Fa-f]{4}) ([0-9A-Fa-f]{4}) ([0-9A-Fa-f]{2})")
            .unwrap();

        let result = from_utf8(output.stdout.as_ref())
            .unwrap()
            .split('\n')
            .into_iter()
            .filter(|line| re.is_match(line))
            .map(|line| re.captures(line).unwrap())
            .map(|caps| {
                let a = u16::from_str_radix(caps.get(1).unwrap().as_str(), 16).unwrap();
                let b = u16::from_str_radix(caps.get(2).unwrap().as_str(), 16).unwrap();
                let c = u16::from_str_radix(caps.get(3).unwrap().as_str(), 16).unwrap();
                let exceptions = u8::from_str_radix(caps.get(4).unwrap().as_str(), 16).unwrap();

                (a, b, c, exceptions)
            })
            .map(|(a, b, c, expect_exception)| {
                let fa = Float::<u16>::construct(a);
                let fb = Float::<u16>::construct(b);
                let expect = (c, expect_exception);
                let (actual_result, actual_exception) = f(fa, fb);
                let actual = (actual_result.v, actual_exception.0);
                (a, b, expect, actual)
            })
            .filter(|(_, _, (e_result, e_exception), (a_result, a_exception))| {
                e_result != a_result || e_exception != a_exception
            })
            .collect::<Vec<(u16, u16, (u16, u8), (u16, u8))>>();

        if result.is_empty() {
            Ok(())
        } else {
            println!(
                "{:>6}, {:>6}, {:>6}, {:>6}, {:>16}, {:>16}",
                "a", "b", "c_expect", "c_actual", "exception_expect", "exception_actual"
            );
            result
                .iter()
                .for_each(|(a, b, (e_result, e_exception), (a_result, a_exception))| {
                    println!(
                        "0x{:04x}, 0x{:04x}, 0x{:04x}, 0x{:04x}, {:02x}, {:02x}",
                        a, b, e_result, a_result, e_exception, a_exception
                    )
                });
            panic!("test_{} failed({} failed)", function, result.len());
        }
    }
    */

    /*
    #[test]
    fn f32_add_0xfffffffe_0xfffffffe() {
        let a = Float::construct(0xffff_fffe);
        let b = Float::construct(0xffff_fffe);

        a + b;
    }

    #[test]
    fn f32_add_0xc00007ef_0x3dfff7bf() {
        let a = Float::construct(0xc00007ef);
        let b = Float::construct(0x3dfff7bf);

        a + b;
    }

    #[test]
    fn f32_add_0xc07fffee_0x4fff0010() {
        let a = Float::construct(0xc07fffee);
        let b = Float::construct(0x4fff0010);

        a + b;
    }

    #[test]
    fn f32_add_0x0027fffe_0x808006fe() {
        let a = Float::construct(0x0027fffe);
        let b = Float::construct(0x808006fe);

        a + b;
    }

    #[test]
    fn f32_mul_0x3f55e76b_0x80800000() {
        let a = Float::construct(0x3f55_e76b);
        let b = Float::construct(0x8080_0000);

        a * b;
    }

    #[test]
    fn f32_mul_0xc07fffee_0x4fff0010() {
        let a = Float::construct(0xc07f_ffee);
        let b = Float::construct(0x4fff_0010);

        a * b;
    }

    #[test]
    fn f32_mul_0xc1f4718a_0x80000102() {
        let a = Float::construct(0xc1f4_718a);
        let b = Float::construct(0x8000_0102);

        a * b;
    }

    #[test]
    fn f32_mul_0xbf7fffff_0x80800000() {
        let a = Float::construct(0xbf7f_ffff);
        let b = Float::construct(0x8080_0000);

        a * b;
    }

    #[test]
    fn f32_mul_0x0032c625_0x80e00004() {
        let a = Float::construct(0x0032_c625);
        let b = Float::construct(0x80e0_0004);

        a * b;
    }

    #[test]
    fn f32_div_0xc00007ef_0x3dfff7bf() {
        let a = Float::construct(0xc000_07ef);
        let b = Float::construct(0x3dff_f7bf);

        a / b;
    }

    #[test]
    fn f32_div_0xdea0000e_0x41ff8003() {
        let a = Float::construct(0xdea0_000e);
        let b = Float::construct(0x41ff_8003);

        a / b;
    }
    */
}
