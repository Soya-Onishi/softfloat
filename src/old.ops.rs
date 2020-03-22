extern crate either;

use std::ops::{Add, Sub, Mul, Div, Rem, Neg, BitAnd, BitOr, Not, Shr, Shl};
use std::cmp::{PartialOrd, PartialEq};
use crate::{
  Float, 
  RoundingMode, 
  Exception,
  constant::*
};
use either::Either;

  



const BIAS_F32: i32 = 127;
const SIG_WIDTH_F32: u32 = 23;
const SIG_MASK_F32: u32 = (1 << SIG_WIDTH_F32) - 1;
const HIDDEN_SIGNIFICAND: u32 = 0x0080_0000;
const ROUND_WIDTH: u32 = 3;
const ROUND_MASK: u32 = (1 << ROUND_WIDTH) - 1;