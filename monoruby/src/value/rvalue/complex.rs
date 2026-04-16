use super::*;

#[derive(Clone, Debug, PartialEq)]
#[repr(transparent)]
pub struct ComplexInner(num::complex::Complex<Real>);

impl std::fmt::Display for ComplexInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Complex(({})+({})i)", self.0.re, self.0.im)
    }
}

impl GC<RValue> for ComplexInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.0.re.mark(alloc);
        self.0.im.mark(alloc);
    }
}

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

impl RubyEql<Executor, Globals, MonorubyErr> for ComplexInner {
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        Ok(self.0.re.eql(&other.0.re, vm, globals)? && self.0.im.eql(&other.0.im, vm, globals)?)
    }
}

impl ComplexInner {
    pub fn new(re: Real, im: Real) -> Self {
        Self(num::complex::Complex { re, im })
    }

    pub fn neg(&self) -> Self {
        Self(-self.0)
    }

    pub fn re(&self) -> Real {
        self.0.re
    }

    pub fn im(&self) -> Real {
        self.0.im
    }

    pub fn dup(&self) -> Self {
        Self::new(self.0.re.dup(), self.0.im.dup())
    }

    pub fn to_complex_f64(&self) -> num::complex::Complex<f64> {
        num::complex::Complex::new(self.0.re.to_f64(), self.0.im.to_f64())
    }

    /// Returns the `inspect` representation: `(1+2i)`
    pub fn debug(&self, store: &Store) -> String {
        let (sign, im_str) = self.formatted_im_parts(store, |v| v.debug(store));
        let sep = if self.needs_star(store) { "*" } else { "" };
        format!("({}{}{}{}i)", self.re.get().debug(store), sign, im_str, sep)
    }

    /// Returns the `to_s` representation: `1+2i`
    pub fn to_s_str(&self, store: &Store) -> String {
        let (sign, im_str) = self.formatted_im_parts(store, |v| v.to_s(store));
        let sep = if self.needs_star(store) { "*" } else { "" };
        format!("{}{}{}{}i", self.re.get().to_s(store), sign, im_str, sep)
    }

    /// Extract the sign-prefix and the absolute textual form of the imaginary
    /// component for Complex rendering. NaN is always emitted with a `+`.
    fn formatted_im_parts(
        &self,
        _: &Store,
        fmt: impl Fn(Value) -> String,
    ) -> (&'static str, String) {
        let im_val = self.im.get();
        if let RV::Float(f) = im_val.unpack() {
            if f.is_nan() {
                return ("+", "NaN".to_string());
            }
            if f.is_infinite() {
                return (if f < 0.0 { "-" } else { "+" }, "Infinity".to_string());
            }
        }
        let text = fmt(im_val);
        if self.im.is_negative() {
            ("", text)
        } else {
            ("+", text)
        }
    }

    /// Does the imaginary part need a `*` between it and the trailing `i`?
    /// CRuby inserts `*` when the rendered imaginary part ends in a
    /// non-digit (Infinity, NaN, or a parenthesised form).
    fn needs_star(&self, _: &Store) -> bool {
        let im_val = self.im.get();
        if let RV::Float(f) = im_val.unpack() {
            if !f.is_finite() {
                return true;
            }
        }
        false
    }
}
