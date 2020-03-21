use crate::Float;

pub trait FloatConstant<T> {
  fn hidden_bit() -> T;
  fn round_width() -> T;
  fn round_mask() -> T;
  fn bias() -> T;
  fn signal_bit() -> T;
  fn significand_mask() -> T;
  fn width() -> T;
}

impl FloatConstant<u32> for Float<u32> {
  fn hidden_bit() -> u32 { 0x0080_0000 }
  fn round_width() -> u32 { 3 }
  fn round_mask() -> u32 { 1 << Float::<u32>::round_width() - 1 }
  fn bias() -> u32 { 127 }
  fn signal_bit() -> u32 { 0x0040_0000 }
  fn significand_mask() -> u32 { 0x007F_FFFF }
  fn width() -> u32 { 32 }
}