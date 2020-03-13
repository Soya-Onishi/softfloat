use std::ops::BitOr;

#[derive(Clone, Copy)]
pub struct Exception(pub u8);
pub const EXCEPTION_NONE: u8 = 0;
pub const EXCEPTION_INEXACT: u8 = 1 << 0;
pub const EXCEPTION_UNDERFLOW: u8 = 1 << 1;
pub const EXCEPTION_OVERFLOW: u8 = 1 << 2;
pub const EXCEPTION_INFINITE: u8 = 1 << 3;
pub const EXCEPTION_INVALID: u8 = 1 << 4;

impl BitOr for Exception {
    type Output = Exception;

    fn bitor(self, other: Exception) -> Exception {
        Exception(self.0 | other.0)
    }
}
