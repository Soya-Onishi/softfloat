use crate::Float;

pub trait FloatConstructor<T> {
  fn constructor(sign: bool, exp: T, sig: T) -> Float<T>;  
  fn default_nan() -> Float<T>;      
  fn zero(sign: bool) -> Float<T>;
  fn infinite(sign: bool) -> Float<T>;
}

impl FloatConstructor<u32> for Float<u32> {
  fn constructor(sign: bool, exp: u32, sig: u32) -> Float<u32> {      
    Float::<u32> { v: ((sign as u32) << 31) | (exp << 23) | sig }
  }    

  fn default_nan() -> Float<u32> {
      Float::<u32>::constructor(true, 0xFF, 0x0040_0000)
  }

  fn zero(sign: bool) -> Float<u32> {
      Float::<u32>::constructor(sign, 0, 0)
  }

  fn infinite(sign: bool) -> Float<u32> {
      Float::<u32>::constructor(sign, 0xFF, 0)
  }
}

