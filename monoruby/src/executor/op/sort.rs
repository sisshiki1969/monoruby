use super::*;

impl Executor {
    ///
    /// Sort *vec* with `<=>`: natively when that runs no Ruby
    /// ([`sort_homogeneous`]), and otherwise by CRuby's merge order
    /// (`merge_sort`).
    ///
    pub(crate) fn sort(&mut self, globals: &mut Globals, vec: &mut [Value]) -> Result<()> {
        if sort_homogeneous(globals, vec) {
            return Ok(());
        }
        let is_greater = |a: Value, b: Value| {
            let ord = Executor::compare_values(self, globals, a, b)?;
            Result::Ok(ord == std::cmp::Ordering::Greater)
        };
        merge_sort(vec, is_greater)
    }

    ///
    /// Interpret a comparator block's return value as an ordering, following
    /// CRuby's `rb_cmpint`.
    ///
    /// An Integer gives its sign directly; `nil` means the two elements are
    /// not comparable; anything else is asked to compare *itself* against 0,
    /// so a `Float`, `Bignum` or `Rational` comparator works the way CRuby's
    /// does. `lhs`/`rhs` are the elements being compared and are only used to
    /// name the operands in the error.
    ///
    /// Every caller that runs a user comparator block must go through this:
    /// `Hash#sort` used to interpret only `Fixnum` and silently collapse
    /// everything else to "equal", which returned an unsorted result with no
    /// error for a `Float`/`Bignum` comparator and swallowed the
    /// `ArgumentError` for `nil` (#1076).
    ///
    pub(crate) fn cmpint(
        &mut self,
        globals: &mut Globals,
        res: Value,
        lhs: Value,
        rhs: Value,
    ) -> Result<std::cmp::Ordering> {
        if res.is_nil() {
            return Err(cmperr(&globals.store, lhs, rhs));
        }
        let i = match res.try_fixnum() {
            Some(i) => i,
            None => {
                // A Bignum's sign is read directly, like CRuby's rb_cmpint:
                // no method dispatch, so a redefined Integer#<=> (or #>)
                // cannot interfere with sorting by huge differences.
                if let RV::BigInt(b) = res.unpack() {
                    return Ok(match b.sign() {
                        num::bigint::Sign::Minus => std::cmp::Ordering::Less,
                        num::bigint::Sign::NoSign => std::cmp::Ordering::Equal,
                        num::bigint::Sign::Plus => std::cmp::Ordering::Greater,
                    });
                }
                // Anything else is asked for its sign the way rb_cmpint
                // asks — `res > 0`, then `res < 0` — and *not* through
                // `res <=> 0`. The difference shows whenever the program
                // redefined the `<=>` of the very class the comparator
                // returns: with `class Float; def <=>(o) = o - self; end`,
                // `[3.0, 1.0].sort` re-entered that method to read the sign
                // of its own result and got it backwards. A Float result
                // also has no exact Integer to coerce to — reading
                // `Float::MAX - 1.0` as an integer raised RangeError, where
                // CRuby just sees a positive number.
                //
                // A class that answers neither comparison — a String
                // comparator result, say — reaches the same ArgumentError
                // CRuby reaches, and by the same route: `String#>` comes
                // from Comparable, which reports
                // "comparison of String with 0 failed".
                let zero = Value::integer(0);
                let gt = self.invoke_method_inner(globals, IdentId::_GT, res, &[zero], None, None)?;
                if gt.as_bool() {
                    return Ok(std::cmp::Ordering::Greater);
                }
                let lt = self.invoke_method_inner(globals, IdentId::_LT, res, &[zero], None, None)?;
                return Ok(if lt.as_bool() {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                });
            }
        };
        Ok(i.cmp(&0))
    }
}


///
/// A class of element whose `<=>` answer is fixed, so an ordering over a
/// slice of them can be computed in Rust without running any Ruby.
///
/// Produced by [`homogeneous_ord`], which is what licenses the assumption:
/// every element is of the class, and the class's `<=>` is not redefined.
/// Callers then write the comparison for that one class directly — a
/// monomorphic loop that inlines, rather than a dispatch per comparison —
/// so the `unwrap`s they use are safe only for values that passed the
/// check.
///
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum HomogeneousOrd {
    Fixnum,
    String,
    Float,
}

///
/// Classify `vals` as all-of-one-class with a known `<=>` (`BASIC_OP_DEFS`
/// licenses the pairs, and a redefinition of the class's own `<=>` is
/// checked here), or `None` when no such shape applies.
///
/// These are the shapes `sort` / `min` / `max` actually meet — a slice of
/// numbers, or of strings — and the point is not just skipping the dispatch
/// (`compare_values_inner` already answers those pairs itself) but skipping
/// the *comparator plumbing*: a `Result`-returning closure through a generic
/// merge sort, or a `compare_values` call per element, versus a direct
/// comparison the compiler can inline.
///
/// A mixed slice, a `Float` slice holding a `NaN` (which has no ordering —
/// CRuby raises on the comparison rather than ordering around it, so it is
/// left to the general path to report), or a class with a redefined `<=>`
/// all give `None`.
///
/// An empty or single-element slice classifies as `Fixnum` vacuously; every
/// caller has already handled those lengths, and no comparison is made.
///
pub(crate) fn homogeneous_ord(globals: &Globals, vals: &[Value]) -> Option<HomogeneousOrd> {
    if vals.iter().all(|v| v.is_fixnum()) {
        return (!cmp_redefined(globals, INTEGER_CLASS)).then_some(HomogeneousOrd::Fixnum);
    }
    if vals.iter().all(|v| v.is_rstring_inner().is_some()) {
        return (!cmp_redefined(globals, STRING_CLASS)).then_some(HomogeneousOrd::String);
    }
    if vals
        .iter()
        .all(|v| v.try_float().is_some_and(|f| !f.is_nan()))
    {
        return (!cmp_redefined(globals, FLOAT_CLASS)).then_some(HomogeneousOrd::Float);
    }
    None
}

///
/// Whether `class` had its `<=>` redefined, which takes the ordering of its
/// instances away from the fast paths above. The global flag is checked
/// first: until *something* redefines a basic operation, no per-class
/// lookup is worth doing.
///
pub(crate) fn cmp_redefined(globals: &Globals, class: ClassId) -> bool {
    globals.store.basic_op_redefined()
        && globals.store.basic_op_redefined_for(class, IdentId::_CMP)
}

///
/// Sort *vec* in place without running any Ruby, when [`homogeneous_ord`]
/// classifies it. Returns whether it did.
///
/// Ruby's sort is not stable, so an unstable sort is a valid answer. Each
/// class gets its own `sort_unstable_by*` call rather than one call through
/// a shared comparator, so the key extraction is monomorphic and inlines.
///
fn sort_homogeneous(globals: &Globals, vec: &mut [Value]) -> bool {
    if vec.len() < 2 {
        return true;
    }
    match homogeneous_ord(globals, vec) {
        // SAFETY of the unwraps here and below: `homogeneous_ord` proved
        // every element is of the class.
        Some(HomogeneousOrd::Fixnum) => vec.sort_unstable_by_key(|v| v.try_fixnum().unwrap()),
        Some(HomogeneousOrd::String) => vec.sort_unstable_by(|a, b| {
            crate::builtins::string::string_byte_then_encoding_cmp(
                a.is_rstring_inner().unwrap(),
                b.is_rstring_inner().unwrap(),
            )
        }),
        Some(HomogeneousOrd::Float) => vec.sort_unstable_by(|a, b| {
            a.try_float()
                .unwrap()
                .partial_cmp(&b.try_float().unwrap())
                .expect("NaN was excluded above")
        }),
        None => return false,
    }
    true
}

///
/// [`sort_homogeneous`] for a *sort_by*-shaped sort: the values being
/// ordered are the keys, and what gets permuted is `indices` into them.
/// Returns whether it sorted.
///
pub(crate) fn sort_indices_by_homogeneous_keys(
    globals: &Globals,
    keys: &[Value],
    indices: &mut [usize],
) -> bool {
    if indices.len() < 2 {
        return true;
    }
    match homogeneous_ord(globals, keys) {
        Some(HomogeneousOrd::Fixnum) => {
            indices.sort_unstable_by_key(|&i| keys[i].try_fixnum().unwrap())
        }
        Some(HomogeneousOrd::String) => indices.sort_unstable_by(|&a, &b| {
            crate::builtins::string::string_byte_then_encoding_cmp(
                keys[a].is_rstring_inner().unwrap(),
                keys[b].is_rstring_inner().unwrap(),
            )
        }),
        Some(HomogeneousOrd::Float) => indices.sort_unstable_by(|&a, &b| {
            keys[a]
                .try_float()
                .unwrap()
                .partial_cmp(&keys[b].try_float().unwrap())
                .expect("NaN was excluded above")
        }),
        None => return false,
    }
    true
}

///
/// CRuby's `rb_cmperr`: the receiver is named by class, and the argument by
/// `inspect` when it is an immediate or a Float — so `0` reads as `0` rather
/// than `Integer` — and by class otherwise.
///
pub(crate) fn cmperr(store: &Store, x: Value, y: Value) -> MonorubyErr {
    let x_name = x.get_real_class_name(store);
    let y_name = if y.is_packed_value() || matches!(y.unpack(), RV::Float(_)) {
        y.inspect(store)
    } else {
        y.get_real_class_name(store)
    };
    MonorubyErr::argumenterr(format!("comparison of {x_name} with {y_name} failed"))
}

///
/// Sort a slice with `compare` in CRuby's merge order: of *Value*s
/// (a sort block), or of indices into a rooted key table (`sort_by`).
///
/// `compare(a, b)` is always asked with `a` the element currently to the
/// left of `b`, and the first error ends the sort.
///
pub(crate) fn sort_by<T, F>(vec: &mut [T], mut compare: F) -> Result<()>
where
    T: Copy,
    F: FnMut(T, T) -> Result<std::cmp::Ordering>,
{
    let f = |a: T, b: T| {
        let ord = compare(a, b)?;
        Result::Ok(ord == std::cmp::Ordering::Greater)
    };
    merge_sort(vec, f)
}

///
/// The sort itself: glibc's `msort_with_tmp`, which is what CRuby's
/// `ruby_qsort` is on Linux (`qsort_r`), so a comparator that runs Ruby —
/// a sort block, a user-defined `<=>` — is called with the same pairs, in
/// the same order and the same number of times as there (#2014). Top-down:
/// the first `n / 2` elements and the rest are sorted, then merged, the
/// left element taken while it is not greater than the right one.
///
/// `is_greater(a, b)` is only ever asked about a pair in its current order
/// — `a` to the left of `b` — and answers whether the two are out of
/// order. That is the question CRuby's comparator answers (`sort_2` calls
/// `a <=> b` with `a` the element qsort holds on the left), so the
/// receiver of each `<=>`, the arguments of a sort block and the operands
/// `rb_cmperr` names are the ones CRuby has: `[1, "a"].sort` is
/// "comparison of Integer with String failed" (#2007).
///
/// A merge writes into a scratch buffer and copies it back only once it
/// is complete, so every element stays in `v` while the comparator runs:
/// a collection it triggers finds them all through the slice's owner, and
/// an error leaves `v` a permutation of what it was. The scratch buffer
/// therefore needs no rooting.
///
fn merge_sort<T, F>(v: &mut [T], mut is_greater: F) -> Result<()>
where
    T: Copy,
    F: FnMut(T, T) -> Result<bool>,
{
    if v.len() < 2 {
        return Ok(());
    }
    let mut tmp = Vec::with_capacity(v.len());
    return msort(v, &mut tmp, &mut is_greater);

    fn msort<T, F>(b: &mut [T], tmp: &mut Vec<T>, is_greater: &mut F) -> Result<()>
    where
        T: Copy,
        F: FnMut(T, T) -> Result<bool>,
    {
        let n = b.len();
        if n < 2 {
            return Ok(());
        }
        let n1 = n / 2;
        msort(&mut b[..n1], tmp, is_greater)?;
        msort(&mut b[n1..], tmp, is_greater)?;
        tmp.clear();
        let (mut i, mut j) = (0, n1);
        while i < n1 && j < n {
            if is_greater(b[i], b[j])? {
                tmp.push(b[j]);
                j += 1;
            } else {
                tmp.push(b[i]);
                i += 1;
            }
        }
        // What is left of the right run is already in place.
        tmp.extend_from_slice(&b[i..n1]);
        b[..tmp.len()].copy_from_slice(tmp);
        Ok(())
    }
}
