use std::ops::BitOr;

#[derive(Clone, Copy)]
pub struct Exception(pub u8);

impl Exception {
    pub fn none() -> Exception {
        Exception(EXCEPTION_NONE)
    }

    pub fn inexact() -> Exception {
        Exception(EXCEPTION_INEXACT)
    }

    pub fn underflow() -> Exception {
        Exception(EXCEPTION_UNDERFLOW)
    }

    pub fn overflow() -> Exception {
        Exception(EXCEPTION_OVERFLOW)        
    }

    pub fn infinite() -> Exception {
        Exception(EXCEPTION_INFINITE)
    }

    pub fn invalid() -> Exception {
        Exception(EXCEPTION_INVALID)
    }
}

impl BitOr for Exception {
    type Output = Exception;

    fn bitor(self, other: Exception) -> Exception {
        Exception(self.0 | other.0)
    }
}

const EXCEPTION_NONE: u8 = 0;
const EXCEPTION_INEXACT: u8 = 1 << 0;
const EXCEPTION_UNDERFLOW: u8 = 1 << 1;
const EXCEPTION_OVERFLOW: u8 = 1 << 2;
const EXCEPTION_INFINITE: u8 = 1 << 3;
const EXCEPTION_INVALID: u8 = 1 << 4;
