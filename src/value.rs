use super::*;

//const UNINITIALIZED: u64 = 0x24; // 0010_0100
const NIL_VALUE: u64 = 0x04; // 0000_0100
const FALSE_VALUE: u64 = 0x14; // 0001_0100
const TRUE_VALUE: u64 = 0x1c; // 0001_1100
                              //const TAG_SYMBOL: u64 = 0x0c; // 0000_1100
                              //const BOOL_MASK1: u64 = 0b0011_0000;
                              //const BOOL_MASK2: u64 = 0xffff_ffff_ffff_ffcf;
const FLOAT_MASK1: u64 = !(0b0110u64 << 60);
const FLOAT_MASK2: u64 = 0b0100u64 << 60;

const ZERO: u64 = (0b1000 << 60) | 0b10;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Value(std::num::NonZeroU64);

#[derive(Debug, Clone, PartialEq)]
pub enum RV {
    Nil,
    Bool(bool),
    Integer(i32),
    Float(f64),
}

impl std::fmt::Display for RV {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RV::Integer(n) => write!(f, "{}", n),
            RV::Float(n) => write!(f, "{}", n),
            RV::Bool(b) => write!(f, "{:?}", b),
            RV::Nil => write!(f, "nil"),
        }
    }
}

/*impl RV {
    pub fn pack(&self) -> u64 {
        match self {
            Self::Integer(i) => *i as i64 as u64,
            Self::Float(f) => f64::to_bits(*f),
            Self::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            Self::Nil => 0,
        }
    }
}*/

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.unpack())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.unpack() {
            RV::Integer(n) => write!(f, "{}", n),
            RV::Float(n) => write!(f, "{}", n),
            RV::Bool(b) => write!(f, "{:?}", b),
            RV::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    #[inline(always)]
    pub fn from(id: u64) -> Self {
        Value(std::num::NonZeroU64::new(id).unwrap())
    }

    #[inline(always)]
    pub fn from_unchecked(id: u64) -> Self {
        unsafe { Value(std::num::NonZeroU64::new_unchecked(id)) }
    }

    #[inline(always)]
    pub fn get(&self) -> u64 {
        self.0.get()
    }

    #[inline(always)]
    pub fn get_u32(&self) -> (u32, u32) {
        let val = self.0.get();
        let v1 = (val >> 32) as u32;
        let v2 = val as u32;
        (v1, v2)
    }

    #[inline(always)]
    pub const fn nil() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(NIL_VALUE) })
    }

    /*#[inline(always)]
    const fn true_val() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(TRUE_VALUE) })
    }

    #[inline(always)]
    const fn false_val() -> Self {
        Value(unsafe { std::num::NonZeroU64::new_unchecked(FALSE_VALUE) })
    }*/

    #[inline(always)]
    pub fn bool(b: bool) -> Self {
        if b {
            Value::from(TRUE_VALUE)
        } else {
            Value::from(FALSE_VALUE)
        }
    }

    pub fn bool_fromu64(num: u64) -> Self {
        Value::bool(num as u8 != 0)
    }

    #[inline(always)]
    pub fn fixnum(num: i64) -> Self {
        Value::from((num << 1) as u64 | 0b1)
    }

    /*#[inline(always)]
    fn is_i63(num: i64) -> bool {
        let top = (num as u64) >> 62 ^ (num as u64) >> 63;
        top & 0b1 == 0
    }*/

    pub fn integer(num: i32) -> Self {
        Value::fixnum(num as i64)
    }

    pub fn integer_fromu64(num: u64) -> Self {
        Value::fixnum(num as i64)
    }

    pub fn float(num: f64) -> Self {
        if num == 0.0 {
            return Value::from(ZERO);
        }
        let unum = f64::to_bits(num);
        let exp = ((unum >> 60) & 0b111) + 1;
        if (exp & 0b0110) == 0b0100 {
            Value::from((unum & FLOAT_MASK1 | FLOAT_MASK2).rotate_left(3))
        } else {
            panic!()
        }
    }

    pub fn float_fromu64(num: u64) -> Self {
        Value::float(f64::from_bits(num))
    }

    pub fn unpack(&self) -> RV {
        if let Some(i) = self.as_fixnum() {
            RV::Integer(i)
        } else if let Some(f) = self.as_flonum() {
            RV::Float(f)
        } else if !self.is_packed_value() {
            panic!()
        } else {
            match self.0.get() {
                NIL_VALUE => RV::Nil,
                TRUE_VALUE => RV::Bool(true),
                FALSE_VALUE => RV::Bool(false),
                _ => unreachable!("Illegal packed value. {:x}", self.0),
            }
        }
    }

    pub fn pack(&self) -> u64 {
        if !self.is_packed_value() {
            panic!()
        } else if let Some(i) = self.as_fixnum() {
            i as i64 as u64
        } else if let Some(f) = self.as_flonum() {
            f64::to_bits(f)
        } else {
            match self.0.get() {
                NIL_VALUE => 0,
                TRUE_VALUE => 1,
                FALSE_VALUE => 0,
                _ => unreachable!("Illegal packed value. {:x}", self.0),
            }
        }
    }
}

impl Value {
    /*fn is_nil(&self) -> bool {
        self.0.get() == NIL_VALUE
    }*/

    pub fn to_bool(&self) -> bool {
        let v = self.0.get();
        (v | 0x10) != 0x14
    }

    fn is_packed_value(&self) -> bool {
        self.0.get() & 0b0111 != 0
    }

    pub fn as_fnum(&self) -> i64 {
        (self.0.get() as i64) >> 1
    }

    pub fn is_fnum(&self) -> bool {
        self.0.get() & 0b1 == 1
    }

    fn as_fixnum(&self) -> Option<i32> {
        if self.is_fnum() {
            Some(self.as_fnum() as i32)
        } else {
            None
        }
    }

    fn as_flonum(&self) -> Option<f64> {
        let u = self.0.get();
        if u & 0b11 == 2 {
            if u == ZERO {
                return Some(0.0);
            }
            let bit = 0b10 - ((u >> 63) & 0b1);
            let num = ((u & !(0b0011u64)) | bit).rotate_right(3);
            //eprintln!("after  unpack:{:064b}", num);
            Some(f64::from_bits(num))
        } else {
            None
        }
    }

    /*#[inline(always)]
    fn is_packed_num(&self) -> bool {
        self.0.get() & 0b11 != 0
    }*/

    pub fn ty(&self) -> Type {
        match self.unpack() {
            RV::Integer(_) => Type::Integer,
            RV::Float(_) => Type::Float,
            RV::Bool(_) => Type::Bool,
            RV::Nil => Type::Nil,
        }
    }

    /*pub fn pack(&self) -> u64 {
        self.0.get()
    }*/
}

impl Value {
    pub fn as_i(&self) -> i32 {
        match self.unpack() {
            RV::Integer(i) => i,
            _ => unreachable!("{:?}", self),
        }
    }

    pub fn as_f(&self) -> f64 {
        match self.unpack() {
            RV::Float(f) => f,
            _ => unreachable!(),
        }
    }
}
