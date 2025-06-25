//! [`Equivalent`] is a trait for key comparison in maps.
//!
//! These may be used in the implementation of maps where the lookup type `Q`
//! may be different than the stored key type `K`.
//!
//! * `Q: Equivalent<K, E, G, R>` checks for equality, similar to the `HashMap<K, V>`
//!   constraint `K: Borrow<Q>, Q: Eq`.
//!
//! These traits are not used by the maps in the standard library, but they may
//! add more flexibility in third-party map implementations, especially in
//! situations where a strict `K: Borrow<Q>` relationship is not available.
//!
//! # Examples
//!
//! ```
//! use equivalent::*;
//!
//! pub struct Pair<A, B>(pub A, pub B);
//!
//! impl<'a, A: ?Sized, B: ?Sized, C, D> Equivalent<(C, D)> for Pair<&'a A, &'a B>
//! where
//!     A: Equivalent<C>,
//!     B: Equivalent<D>,
//! {
//!     fn equivalent(&self, key: &(C, D)) -> bool {
//!         self.0.equivalent(&key.0) && self.1.equivalent(&key.1)
//!     }
//! }
//!
//! fn main() {
//!     let key = (String::from("foo"), String::from("bar"));
//!     let q1 = Pair("foo", "bar");
//!     let q2 = Pair("boo", "bar");
//!     let q3 = Pair("foo", "baz");
//!
//!     assert!(q1.equivalent(&key));
//!     assert!(!q2.equivalent(&key));
//!     assert!(!q3.equivalent(&key));
//! }
//! ```

use std::borrow::Borrow;

pub trait RubyEql<E, G, R> {
    /// Compare self to `other` and return `true` if they are equal.
    fn eql(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R>;
}

impl<K: Eq, E, G, R> RubyEql<E, G, R> for K {
    #[inline]
    fn eql(&self, other: &Self, _: &mut E, _: &mut G) -> Result<bool, R> {
        Ok(self == other)
    }
}

/// Key equivalence trait.
///
/// This trait allows hash table lookup to be customized. It has one blanket
/// implementation that uses the regular solution with `Borrow` and `Eq`, just
/// like `HashMap` does, so that you can pass `&str` to lookup into a map with
/// `String` keys and so on.
///
/// # Contract
///
/// The implementor **must** hash like `K`, if it is hashable.
pub trait Equivalent<K: ?Sized, E, G, R> {
    /// Compare self to `key` and return `true` if they are equal.
    fn equivalent(&self, key: &K, e: &mut E, g: &mut G) -> Result<bool, R>;
}

impl<Q: ?Sized, K: ?Sized, E, G, R> Equivalent<K, E, G, R> for Q
where
    Q: Eq,
    K: Borrow<Q>,
{
    #[inline]
    fn equivalent(&self, key: &K, _: &mut E, _: &mut G) -> Result<bool, R> {
        Ok(PartialEq::eq(self, key.borrow()))
    }
}
