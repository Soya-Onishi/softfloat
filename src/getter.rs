use crate::Float;

pub trait FloatGetter<T> {
  fn sign(self) -> bool;
  fn exp(self) -> T;
  fn sig(self) -> T;
}

impl FloatGetter<u32> for Float<u32> {
  fn sign(self) -> bool { self.v & 0x8000_0000 != 0 }
  fn exp(self) -> u32 { (self.v >> 23) & 0xFF }
  fn sig(self) -> u32 { self.v & 0x3F_FFFF }
}