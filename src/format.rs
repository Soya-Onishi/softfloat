use crate::Float;

pub trait FloatFormat {
  fn is_nan(self) -> bool; 
  fn is_signal_nan(self) -> bool;
  fn is_inf(self) -> bool;    
  fn is_zero(self) -> bool;
  fn is_subnormal(self) -> bool;
}

impl FloatFormat for Float<u32> {
  fn is_nan(self) -> bool {
    self.exp() == 0xFF && self.sig() != 0
  }

  fn is_signal_nan(self) -> bool {
      self.exp() == 0xFF && (self.sig() & 0x3FFFFF != 0) && (self.sig() & 0x40000 == 0)
  }

  fn is_inf(self) -> bool {
      self.exp() == 0xFF && self.sig() == 0
  }

  fn is_zero(self) -> bool {
      (self.exp() | self.sig()) == 0
  }

  fn is_subnormal(self) -> bool {
      self.exp() == 0 && self.sig() != 0
  }
}