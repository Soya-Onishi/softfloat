use std::convert::TryFrom;

pub trait Extends {
  type Output;
  fn extend(v: Self) -> Self::Output;
}

impl Extends for u16 {
  type Output = u32;
  fn extend(v: u16) -> u32 {
    v as u32
  }
}

impl Extends for u32 {
  type Output = u64;

  fn extend(v: u32) -> u64 {
    v as u64
  }
}

impl Extends for u64 {
  type Output = u128;

  fn extend(v: u64) -> u128 {
    v as u128
  }
}

pub trait LeadingZeros {
  fn count_leading_zeros(self) -> Self;
}

impl LeadingZeros for u16 {
  fn count_leading_zeros(self) -> u16 {
    u16::try_from(self.leading_zeros()).unwrap()
  }
}

impl LeadingZeros for u32 {
  fn count_leading_zeros(self) -> u32 {
    self.leading_zeros()
  }
}

impl LeadingZeros for u64 {
  fn count_leading_zeros(self) -> u64 {
    self.leading_zeros().into()
  }
}

pub trait BitWidth {
  fn width() -> Self;
}

impl BitWidth for u16 {
  fn width() -> u16 {
    16
  }
}

impl BitWidth for u32 {
  fn width() -> u32 {
    32
  }
}

impl BitWidth for u64 {
  fn width() -> u64 {
    64
  }
}

impl BitWidth for u128 {
  fn width() -> u128 {
    128
  }
}
