use super::*;

/// Slices of up to this length get sorted using insertion sort, which
/// needs no merge buffer.
const MAX_INSERTION: usize = 20;

///
/// Number of scratch `Value` slots `merge_sort` needs to sort `len`
/// elements; 0 when insertion sort covers it.
///
/// The caller owns the scratch buffer because it must be **GC-visible**:
/// during a merge the buffer holds the only reference to the elements of
/// one run (their slots in the receiver have already been overwritten by
/// merge output), and the comparator runs arbitrary Ruby — and therefore
/// may trigger a GC — between the copy-in and the copy-out. A plain
/// Rust `Vec<Value>` is not scanned by the collector, so those elements
/// were swept and the sort resumed on dangling pointers. Callers back
/// the buffer with a `nil`-filled Array kept on the temp stack (every
/// slot then always holds a valid, markable `Value`).
///
pub(crate) fn merge_scratch_len(len: usize) -> usize {
    if len <= MAX_INSERTION { 0 } else { len / 2 }
}

impl Executor {
    ///
    /// Execute merge sort for Vec of *Value*s with `<=>`.
    ///
    /// `buf` must point to at least `merge_scratch_len(vec.len())`
    /// initialized, GC-rooted `Value`s (may be null when that is 0).
    ///
    pub(crate) fn sort(
        &mut self,
        globals: &mut Globals,
        vec: &mut [Value],
        buf: *mut Value,
    ) -> Result<()> {
        if sort_homogeneous(globals, vec) {
            return Ok(());
        }
        let is_less = |a: Value, b: Value| {
            let ord = Executor::compare_values(self, globals, a, b)?;
            Result::Ok(ord == std::cmp::Ordering::Less)
        };
        merge_sort(vec, buf, is_less)
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
/// Execute merge sort for Vec of *Value*s with `compare`.
///
/// See `Executor::sort` for the `buf` contract.
///
pub(crate) fn sort_by<F>(vec: &mut [Value], buf: *mut Value, mut compare: F) -> Result<()>
where
    F: FnMut(Value, Value) -> Result<std::cmp::Ordering>,
{
    let f = |a: Value, b: Value| {
        let ord = compare(a, b)?;
        Result::Ok(ord == std::cmp::Ordering::Less)
    };
    merge_sort(vec, buf, f)
}

fn merge_sort<F>(v: &mut [Value], buf: *mut Value, mut is_less: F) -> Result<()>
where
    F: FnMut(Value, Value) -> Result<bool>,
{
    // Very short runs are extended using insertion sort to span at least this many elements.
    const MIN_RUN: usize = 10;

    let len = v.len();

    // Short arrays get sorted in-place via insertion sort to avoid allocations.
    if len <= MAX_INSERTION {
        if len >= 2 {
            for i in (0..len - 1).rev() {
                insert_head(&mut v[i..], &mut is_less)?;
            }
        }
        return Ok(());
    }

    debug_assert!(!buf.is_null());
    let mut runs = vec![];
    let mut end = len;
    while end > 0 {
        // Find the next natural run, and reverse it if it's strictly descending.
        let mut start = end - 1;
        if start > 0 {
            start -= 1;
            unsafe {
                if is_less(*v.get_unchecked(start + 1), *v.get_unchecked(start))? {
                    while start > 0
                        && is_less(*v.get_unchecked(start), *v.get_unchecked(start - 1))?
                    {
                        start -= 1;
                    }
                    v[start..end].reverse();
                } else {
                    while start > 0
                        && !is_less(*v.get_unchecked(start), *v.get_unchecked(start - 1))?
                    {
                        start -= 1;
                    }
                }
            }
        }

        // Insert some more elements into the run if it's too short. Insertion sort is faster than
        // merge sort on short sequences, so this significantly improves performance.
        while start > 0 && end - start < MIN_RUN {
            start -= 1;
            insert_head(&mut v[start..end], &mut is_less)?;
        }

        // Push this run onto the stack.
        runs.push(Run {
            start,
            len: end - start,
        });
        end = start;

        // Merge some pairs of adjacent runs to satisfy the invariants.
        while let Some(r) = collapse(&runs) {
            let left = runs[r + 1];
            let right = runs[r];
            merge(
                &mut v[left.start..right.start + right.len],
                left.len,
                buf,
                &mut is_less,
            )?;
            runs[r] = Run {
                start: left.start,
                len: left.len + right.len,
            };
            runs.remove(r + 1);
        }
    }

    debug_assert!(runs.len() == 1 && runs[0].start == 0 && runs[0].len == len);

    return Ok(());

    #[inline]
    fn collapse(runs: &[Run]) -> Option<usize> {
        let n = runs.len();
        if n >= 2
            && (runs[n - 1].start == 0
                || runs[n - 2].len <= runs[n - 1].len
                || (n >= 3 && runs[n - 3].len <= runs[n - 2].len + runs[n - 1].len)
                || (n >= 4 && runs[n - 4].len <= runs[n - 3].len + runs[n - 2].len))
        {
            if n >= 3 && runs[n - 3].len < runs[n - 1].len {
                Some(n - 3)
            } else {
                Some(n - 2)
            }
        } else {
            None
        }
    }

    #[derive(Clone, Copy)]
    struct Run {
        start: usize,
        len: usize,
    }
}

fn insert_head<F>(v: &mut [Value], is_less: &mut F) -> Result<()>
where
    F: FnMut(Value, Value) -> Result<bool>,
{
    if v.len() >= 2 && is_less(v[1], v[0])? {
        unsafe {
            let mut tmp = std::mem::ManuallyDrop::new(std::ptr::read(&v[0]));

            // initially held exactly once.
            let mut hole = InsertionHole {
                src: &mut *tmp,
                dest: &mut v[1],
            };
            std::ptr::copy_nonoverlapping(&v[1], &mut v[0], 1);

            for i in 2..v.len() {
                if !is_less(v[i], *tmp)? {
                    break;
                }
                std::ptr::copy_nonoverlapping(&v[i], &mut v[i - 1], 1);
                hole.dest = &mut v[i];
            }
            // `hole` gets dropped and thus copies `tmp` into the remaining hole in `v`.
        }
    }
    return Ok(());

    // When dropped, copies from `src` into `dest`.
    struct InsertionHole<T> {
        src: *mut T,
        dest: *mut T,
    }

    impl<T> Drop for InsertionHole<T> {
        fn drop(&mut self) {
            unsafe {
                std::ptr::copy_nonoverlapping(self.src, self.dest, 1);
            }
        }
    }
}

fn merge<F>(v: &mut [Value], mid: usize, buf: *mut Value, is_less: &mut F) -> Result<()>
where
    F: FnMut(Value, Value) -> Result<bool>,
{
    let len = v.len();
    let v = v.as_mut_ptr();
    let (v_mid, v_end) = unsafe { (v.add(mid), v.add(len)) };

    let mut hole;

    if mid <= len - mid {
        // The left run is shorter.
        unsafe {
            std::ptr::copy_nonoverlapping(v, buf, mid);
            hole = MergeHole {
                start: buf,
                end: buf.add(mid),
                dest: v,
            };
        }

        // Initially, these pointers point to the beginnings of their arrays.
        let left = &mut hole.start;
        let mut right = v_mid;
        let out = &mut hole.dest;

        while *left < hole.end && right < v_end {
            // Consume the lesser side.
            // If equal, prefer the left run to maintain stability.
            unsafe {
                let to_copy = if is_less(*right, **left)? {
                    get_and_increment(&mut right)
                } else {
                    get_and_increment(left)
                };
                std::ptr::copy_nonoverlapping(to_copy, get_and_increment(out), 1);
            }
        }
    } else {
        // The right run is shorter.
        unsafe {
            std::ptr::copy_nonoverlapping(v_mid, buf, len - mid);
            hole = MergeHole {
                start: buf,
                end: buf.add(len - mid),
                dest: v_mid,
            };
        }

        // Initially, these pointers point past the ends of their arrays.
        let left = &mut hole.dest;
        let right = &mut hole.end;
        let mut out = v_end;

        while v < *left && buf < *right {
            // Consume the greater side.
            // If equal, prefer the right run to maintain stability.
            unsafe {
                let to_copy = if is_less(*right.offset(-1), *left.offset(-1))? {
                    decrement_and_get(left)
                } else {
                    decrement_and_get(right)
                };
                std::ptr::copy_nonoverlapping(to_copy, decrement_and_get(&mut out), 1);
            }
        }
    };
    return Ok(());
    // Finally, `hole` gets dropped. If the shorter run was not fully consumed, whatever remains of
    // it will now be copied into the hole in `v`.

    unsafe fn get_and_increment<T>(ptr: &mut *mut T) -> *mut T {
        let old = *ptr;
        unsafe { *ptr = ptr.offset(1) };
        old
    }

    unsafe fn decrement_and_get<T>(ptr: &mut *mut T) -> *mut T {
        unsafe { *ptr = ptr.offset(-1) };
        *ptr
    }

    // When dropped, copies the range `start..end` into `dest..`.
    struct MergeHole<T> {
        start: *mut T,
        end: *mut T,
        dest: *mut T,
    }

    impl<T> Drop for MergeHole<T> {
        fn drop(&mut self) {
            // `T` is not a zero-sized type, so it's okay to divide by its size.
            let len = (self.end as usize - self.start as usize) / std::mem::size_of::<T>();
            unsafe {
                std::ptr::copy_nonoverlapping(self.start, self.dest, len);
            }
        }
    }
}
