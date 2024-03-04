use super::*;

#[derive(Clone, Debug, PartialEq, Hash)]
#[repr(transparent)]
pub struct ComplexInner(num::complex::Complex<Real>);

impl std::ops::Deref for ComplexInner {
    type Target = num::complex::Complex<Real>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ComplexInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::convert::From<i64> for ComplexInner {
    fn from(i: i64) -> Self {
        ComplexInner(Real::from(i).into())
    }
}

impl std::convert::From<BigInt> for ComplexInner {
    fn from(b: BigInt) -> Self {
        ComplexInner(Real::from(b).into())
    }
}

impl std::convert::From<f64> for ComplexInner {
    fn from(f: f64) -> Self {
        ComplexInner(Real::from(f).into())
    }
}

impl std::convert::From<Real> for ComplexInner {
    fn from(re: Real) -> Self {
        ComplexInner(num::complex::Complex::new(re, 0.into()))
    }
}

impl std::convert::From<num::complex::Complex<Real>> for ComplexInner {
    fn from(complex: num::complex::Complex<Real>) -> Self {
        ComplexInner(complex)
    }
}

impl ComplexInner {
    pub fn new(re: Real, im: Real) -> Self {
        Self(num::complex::Complex { re, im })
    }

    pub fn re(&self) -> Real {
        self.0.re
    }

    pub fn im(&self) -> Real {
        self.0.im
    }

    pub fn eql(&self, other: &Self) -> bool {
        self.0.re.eql(&other.0.re) && self.0.im.eql(&other.0.im)
    }

    pub fn dup(&self) -> Self {
        Self::new(self.0.re.dup(), self.0.im.dup())
    }

    pub fn to_complex_f64(&self) -> num::complex::Complex<f64> {
        num::complex::Complex::new(self.0.re.to_f64(), self.0.im.to_f64())
    }
}
