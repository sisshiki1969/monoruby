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

use std::borrow::Borrow;

/// Ruby-level Key equivalence trait.
///
/// This trait allows `Hash` lookup to be customized.
///
/// This operations is failible, and return Err if failed.
pub trait RubyEql<E, G, R> {
    /// Compare self to `other` and return `true` if they are equal.
    fn eql(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R>;
}

impl<T, E, G, R> RubyEql<E, G, R> for Vec<T>
where
    T: RubyEql<E, G, R>,
{
    fn eql(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R> {
        if self.len() != other.len() {
            return Ok(false);
        }
        for (a, b) in self.iter().zip(other.iter()) {
            if !a.eql(b, e, g)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

impl<T, E, G, R> RubyEql<E, G, R> for [T]
where
    T: RubyEql<E, G, R>,
{
    fn eql(&self, other: &Self, e: &mut E, g: &mut G) -> Result<bool, R> {
        if self.len() != other.len() {
            return Ok(false);
        }
        for (a, b) in self.iter().zip(other.iter()) {
            if !a.eql(b, e, g)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

///
/// A Hashable type.
///
pub trait RubyHash<E, G, R> {
    fn ruby_hash<H: std::hash::Hasher>(&self, state: &mut H, e: &mut E, g: &mut G)
        -> Result<(), R>;
}

impl<T, E, G, R> RubyHash<E, G, R> for Vec<T>
where
    T: RubyHash<E, G, R>,
{
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        e: &mut E,
        g: &mut G,
    ) -> Result<(), R> {
        for v in self {
            v.ruby_hash(state, e, g)?;
        }
        Ok(())
    }
}

macro_rules! impl_ruby {
    ($($t:ty),*) => {
        $(
            impl<E, G, R> RubyEql<E, G, R> for $t {
                fn eql(&self, other: &Self, _: &mut E, _: &mut G) -> Result<bool, R> {
                    Ok(self == other)
                }
            }

            impl<E, G, R> RubyEql<E, G, R> for &$t {
                fn eql(&self, other: &Self, _: &mut E, _: &mut G) -> Result<bool, R> {
                    Ok(self == other)
                }
            }

            impl<E, G, R> RubyEql<E, G, R> for &mut $t {
                fn eql(&self, other: &Self, _: &mut E, _: &mut G) -> Result<bool, R> {
                    Ok(self == other)
                }
            }

            impl<E, G, R> RubyHash<E, G, R> for $t {
                fn ruby_hash<H: std::hash::Hasher>(&self, state: &mut H, _: &mut E, _: &mut G) -> Result<(), R> {
                    std::hash::Hash::hash(self, state);
                    Ok(())
                }
            }

            impl<E, G, R> RubyHash<E, G, R> for &$t {
                fn ruby_hash<H: std::hash::Hasher>(&self, state: &mut H, _: &mut E, _: &mut G) -> Result<(), R> {
                    std::hash::Hash::hash(self, state);
                    Ok(())
                }
            }

            impl<E, G, R> RubyHash<E, G, R> for &mut $t {
                fn ruby_hash<H: std::hash::Hasher>(&self, state: &mut H, _: &mut E, _: &mut G) -> Result<(), R> {
                    std::hash::Hash::hash(self, state);
                    Ok(())
                }
            }
        )*
    };
}

impl_ruby!(
    (),
    bool,
    char,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    isize,
    usize,
    str,
    String
);

/// Key equivalence trait.
///
/// This trait allows hash table lookup to be customized. It has one blanket
/// implementation that uses the regular solution with `Borrow` and `Eq`, just
/// like `HashMap` does, so that you can pass `&str` to lookup into a map with
/// `String` keys and so on.
///
/// # Examples
///
/// ```
/// use ruby_traits::*;
///
/// pub struct Pair<A, B>(pub A, pub B);
/// struct E;
/// struct G;
///
/// impl<'a, A: ?Sized, B: ?Sized, C, D, E, G, R> Equivalent<(C, D), E, G, R> for Pair<&'a A, &'a B>
/// where
///     A: Equivalent<C, E, G, R>,
///     B: Equivalent<D, E, G, R>,
/// {
///     fn equivalent(&self, key: &(C, D), e: &mut E, g: &mut G) -> Result<bool, R> {
///         Ok(self.0.equivalent(&key.0, e, g)? && self.1.equivalent(&key.1, e, g)?)
///     }
/// }
///
///
/// fn main() {
///     let key = (String::from("foo"), String::from("bar"));
///     let q1 = Pair("foo", "bar");
///     let q2 = Pair("boo", "bar");
///     let q3 = Pair("foo", "baz");
///
///     assert!(<Pair<&str, &str> as ruby_traits::Equivalent<(String, String), (), (), ()>>::equivalent(&q1, &key, &mut (), &mut ()).unwrap());
///     assert!(!<Pair<&str, &str> as ruby_traits::Equivalent<(String, String), (), (), ()>>::equivalent(&q2, &key, &mut (), &mut ()).unwrap());
///     assert!(!<Pair<&str, &str> as ruby_traits::Equivalent<(String, String), (), (), ()>>::equivalent(&q3, &key, &mut (), &mut ()).unwrap());
/// }
/// ```
pub trait Equivalent<K: ?Sized, E, G, R> {
    /// Compare self to `key` and return `true` if they are equal.
    fn equivalent(&self, key: &K, e: &mut E, g: &mut G) -> Result<bool, R>;
}

impl<Q: ?Sized, K: ?Sized, E, G, R> Equivalent<K, E, G, R> for Q
where
    Q: RubyEql<E, G, R>,
    K: Borrow<Q>,
{
    #[inline]
    fn equivalent(&self, key: &K, e: &mut E, g: &mut G) -> Result<bool, R> {
        self.eql(key.borrow(), e, g)
    }
}
