use super::*;
use std::collections::HashSet;
use std::hash::Hash;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ops::Deref;
use std::ptr::NonNull;

//
// ## Small-hash representation
//
// A Hash keeps its representation bits in the RValue header's per-type
// metadata byte (`Metadata::ty_flags`), which frees the whole 48-byte
// payload: small hashes with packed-immediate keys store up to
// [`INLINE_CAP`] key-value pairs directly in the cell (`HashBody::inline`)
// — no heap allocation at all — while everything else lives behind a
// boxed [`BoxedHash`].
//
// Flags byte layout (HASH objects):
//
// | bits | meaning                                              |
// |------|------------------------------------------------------|
// | 0-2  | representation: 0..=3 = inline with that many pairs, |
// |      | 7 = boxed (`HashBody::boxed` is live)                |
// | 3    | ruby2_keywords flag                                  |
// | 4-5  | inline iteration depth (saturating; boxed hashes     |
// |      | count in `BoxedHash::iter_lev` instead)              |
// | 6-7  | reserved (zero)                                      |
//
// A zeroed byte — the `Header::new` default — is a valid empty inline
// hash, and dup/clone (`Header::newborn`) preserve the byte, so the
// representation travels with the header while the payload is copied
// alongside.
//

const REP_MASK: u8 = 0b0000_0111;
const REP_BOXED: u8 = 7;
const R2K_BIT: u8 = 0b0000_1000;
const ITER_MASK: u8 = 0b0011_0000;
const ITER_SHIFT: u8 = 4;
const ITER_MAX: u8 = 3;

// A tombstoned (deleted-during-iteration) entry in a boxed hash's entry
// vector carries the key `None`: the boxed maps are keyed by
// `Option<Value>` / `Option<IdentKey>`, whose `None` occupies `Value`'s
// niche (`Value` wraps `NonZeroU64`, so `Option<Value>` is still 8 bytes,
// `None` is all-zero bits, and `Some(v)` is exactly `v`'s bits — all
// language-guaranteed). Generated code therefore tests the raw key word
// against zero, and the type system forces every Rust-side reader to
// decide what a dead entry means — the sentinel cannot leak by omission.
// Dead entries are unreachable through lookups (the index table drops
// them in `rubymap::tombstone_remove`), every iterator filters them, the
// `__live_at` intrinsic reports them false, and the next mutating
// operation outside an iteration compacts them away
// ([`HashRefMut::compact_if_dirty`]).
const _: () = assert!(std::mem::size_of::<Option<Value>>() == 8);
const _: () = assert!(std::mem::size_of::<Option<IdentKey>>() == 8);
/// The *inline* hash is identity-keyed (`compare_by_identity`).
/// Identity probing is a pure id scan — no hashing, no Ruby code, and
/// an id can never go stale — so an identity-keyed inline hash may hold
/// *any* keys, heap ones included. The boxed form does not use this
/// bit; there the `HashContent` variant (Map vs IdentMap) is the truth.
const IDENT_BIT: u8 = 0b0100_0000;

/// Max pairs held inline: the full 48-byte payload.
pub(crate) const INLINE_CAP: usize = 3;

//
// ## Layout constants for JIT-generated code
//
// `Hash#size` / `#__key_at` / `#__value_at` are emitted as machine code
// that walks this representation directly, so the offsets it bakes in
// must come from the compiler rather than from a reading of the struct
// definitions. Nothing below is hand-computed: the pair is a `repr(Rust)`
// tuple whose field order is unspecified, and the entry buckets reorder
// their fields according to the key's niche (see `rubymap::EntriesLayout`).
//

/// Representation bits of the `ty_flags` byte, and the value meaning "boxed".
pub const HASH_REP_MASK: u8 = REP_MASK;
pub const HASH_REP_BOXED: u8 = REP_BOXED;

/// Max pairs the inline representation holds — the gate for the JIT's
/// inline-allocated Hash literal fast path.
pub const HASH_INLINE_CAP: usize = INLINE_CAP;

/// The inline pair array, addressed from the start of the `RValue`.
pub const HASH_INLINE_PAIRS_OFFSET: usize = RVALUE_OFFSET_KIND;
pub const HASH_INLINE_PAIR_STRIDE: usize = std::mem::size_of::<(Value, Value)>();
pub const HASH_INLINE_KEY_OFFSET: usize = std::mem::offset_of!((Value, Value), 0);
pub const HASH_INLINE_VALUE_OFFSET: usize = std::mem::offset_of!((Value, Value), 1);

/// The boxed content, addressed from the start of the `RValue`. The
/// discriminant sits at `HASH_CONTENT_OFFSET` and the `Box<RubyMap<..>>`
/// at `HASH_CONTENT_MAP_OFFSET`, per `HashContent`'s `repr(C, usize)`.
pub const HASH_CONTENT_OFFSET: usize = RVALUE_OFFSET_KIND + std::mem::offset_of!(BoxedHash, content);
pub const HASH_CONTENT_MAP_OFFSET: usize = HASH_CONTENT_OFFSET + std::mem::size_of::<usize>();

/// `HashContent`'s discriminant: the boxed hash is identity-keyed iff the word
/// at [`HASH_CONTENT_OFFSET`] equals this.
pub const HASH_CONTENT_TAG_IDENT: usize = 1;

/// The inline representation's identity-mode bit within the `ty_flags` byte.
/// (The boxed form uses the `HashContent` discriminant instead.)
pub const HASH_INLINE_IDENT_BIT: u8 = IDENT_BIT;

/// The boxed default slot, addressed from the start of the `RValue`. It holds
/// an `Option<Box<HashDefault>>`, so the word is null exactly when no default
/// is set; otherwise it points at a discriminant followed by the payload.
pub const HASH_DEFAULT_OFFSET: usize = RVALUE_OFFSET_KIND + std::mem::offset_of!(BoxedHash, default);
pub const HASH_DEFAULT_PAYLOAD_OFFSET: usize = std::mem::size_of::<usize>();

/// The boxed tombstone count (a `Cell<u32>`), addressed from the start of the
/// `RValue`. `Hash#size` in machine code subtracts this from the raw entry
/// length; it is zero except while a traversal that deleted keys is live (or
/// until the next mutation compacts).
pub const HASH_DEAD_OFFSET: usize = RVALUE_OFFSET_KIND + std::mem::offset_of!(BoxedHash, dead);

/// The entry-storage layout shared by both boxed forms.
///
/// Returns `None` when the two instantiations disagree, or when
/// `rubymap`'s probe cannot identify the vector's fields — in either case
/// the caller must keep its out-of-line path, so an unrecognised layout
/// costs speed rather than correctness. Identity-keyed maps are keyed by
/// `IdentKey`, a transparent wrapper, so in practice the two agree and
/// generated code need not branch on the discriminant.
pub fn hash_entries_layout() -> Option<rubymap::EntriesLayout> {
    type S = std::collections::hash_map::RandomState;
    let by_value = rubymap::entries_layout::<Option<Value>, Value, (), (), (), S>()?;
    let by_ident = rubymap::entries_layout::<Option<IdentKey>, Value, (), (), (), S>()?;
    (by_value == by_ident).then_some(by_value)
}

/// Is the boxed representation live for this flags byte?
pub(super) fn flags_is_boxed(flags: u8) -> bool {
    flags & REP_MASK == REP_BOXED
}

/// The flags a dup/clone of a hash must carry: the representation and
/// identity-mode bits travel with the copied body, while the
/// ruby2_keywords flag and any live iteration count belong to the
/// source object only.
pub(super) fn sanitize_dup_flags(flags: u8) -> u8 {
    flags & (REP_MASK | IDENT_BIT)
}

/// The boxed map's digest of a packed (immediate) key, computed without
/// the vm: the same builder and the same digest stream as
/// `RubyMap::hash(&Some(k))` (an `Option` key digests as its payload, and
/// a packed payload digests via [`Value::ruby_hash_packed`]), so the
/// prehashed probe and the general probe always agree on buckets.
fn packed_digest<S: std::hash::BuildHasher>(hash_builder: &S, k: Value) -> usize {
    use std::hash::Hasher;
    debug_assert!(k.is_packed_value());
    let mut h = hash_builder.build_hasher();
    k.ruby_hash_packed(&mut h);
    h.finish() as usize
}

/// The boxed map's digest of a String key, computed without the vm: the
/// same builder and the same digest stream as `RubyMap::hash(&Some(k))`
/// for an `ObjTy::STRING` payload — `Value::ruby_hash`'s STRING arm
/// digests the byte content via `RStringInner::hash`, unconditionally (a
/// redefined `String#hash` is never consulted for string-key bucketing)
/// — so the prehashed probe and the general probe always agree on
/// buckets.
fn string_digest<S: std::hash::BuildHasher>(hash_builder: &S, s: &RStringInner) -> usize {
    use std::hash::{Hash, Hasher};
    let mut h = hash_builder.build_hasher();
    s.hash(&mut h);
    h.finish() as usize
}

/// The `eql?` verdict of a stored boxed-map key against a String probe
/// `k` whose content is `s`, computed without the vm — `Value::eql`'s
/// reachable arms for a String lhs: identity, then STRING×STRING byte
/// equality; nothing else is `eql?` to a String. (The general probe
/// could only reach its `eql?` *dispatch* arm against a non-String heap
/// key on a full 64-bit digest collision, and the builtin
/// `String#eql?` it resolves to returns false for any non-String
/// argument — a redefined `String#eql?` is no more observed here than a
/// redefined `String#hash` is at insert time.)
fn string_key_eq(stored: &Option<Value>, k: Value, s: &RStringInner) -> bool {
    match stored {
        Some(sk) => sk.id() == k.id() || sk.is_rstring_inner().is_some_and(|si| si == s),
        None => false,
    }
}

/// Drop the live content of `body` according to `flags` (the RValue
/// sweep path for HASH cells).
///
/// # Safety
/// `flags` must describe `body`, and the boxed content must not be used
/// again afterwards.
pub(super) unsafe fn drop_hash_body(flags: u8, body: &mut HashBody) {
    if flags_is_boxed(flags) {
        unsafe { ManuallyDrop::drop(&mut body.boxed) }
    }
}

///
/// The 48-byte Hash payload, discriminated by the header flags byte.
///
#[repr(C)]
pub union HashBody {
    /// Live while the flags byte says inline; only the first `len` pairs
    /// are meaningful, the rest are nil-nil placeholders (kept
    /// initialized so reading the whole array is always defined).
    inline: [(Value, Value); INLINE_CAP],
    boxed: ManuallyDrop<BoxedHash>,
}

const _: () = assert!(std::mem::size_of::<HashBody>() == 48);

fn empty_pairs() -> [(Value, Value); INLINE_CAP] {
    [(Value::nil(), Value::nil()); INLINE_CAP]
}

///
/// The boxed form: a real map, the default value/proc, and an exact
/// iteration counter.
///
pub(crate) struct BoxedHash {
    content: HashContent,
    /// The default value / default proc. `None` is the common
    /// nil-default case.
    default: Option<Box<HashDefault>>,
    /// Active iteration count on this hash. Incremented while a
    /// block-based traversal (each, each_pair, etc.) is in progress and
    /// decremented when it finishes. Mutating operations consult this
    /// via `check_iter` and raise `RuntimeError` when non-zero.
    /// Corresponds to CRuby's `RHASH_ITER_LEV`. A `Cell` so a traversal
    /// holding a shared borrow can still record its presence.
    iter_lev: std::cell::Cell<u32>,
    /// Tombstoned entries currently sitting in the entry vector: a delete
    /// while `iter_lev > 0` cannot compact (positions must stay stable
    /// under the live traversal), so the entry stays in place with its key
    /// overwritten by `None` and this count goes up. The
    /// live size is the entry count minus this; the next mutating
    /// operation outside an iteration compacts and resets it. A `Cell`
    /// because `Hash#delete` reaches here through the same shared-borrow
    /// discipline as `iter_lev`.
    dead: std::cell::Cell<u32>,
}

impl BoxedHash {
    fn new(content: HashContent) -> Self {
        BoxedHash {
            content,
            default: None,
            iter_lev: std::cell::Cell::new(0),
            dead: std::cell::Cell::new(0),
        }
    }
}

#[derive(Debug, Clone)]
/// `repr(C, usize)` so the discriminant sits at offset 0 and the boxed
/// map pointer at [`HASH_CONTENT_MAP_OFFSET`]. A `repr(Rust)` enum has no
/// guaranteed layout and `offset_of!` cannot reach into a variant, so the
/// JIT could not otherwise follow this pointer — see
/// [`hash_entries_layout`].
#[repr(C, usize)]
enum HashContent {
    Map(Box<RubyMap<Option<Value>, Value>>),
    IdentMap(Box<RubyMap<Option<IdentKey>, Value>>),
}

#[derive(Debug, Clone)]
/// `repr(C, usize)` for the same reason as [`HashContent`]: the JIT reads the
/// discriminant at offset 0 and the payload at [`HASH_DEFAULT_PAYLOAD_OFFSET`],
/// and `offset_of!` cannot reach into a `repr(Rust)` enum variant. Both
/// variants carry exactly one `Value`-shaped word (`Proc` is a newtype over
/// `Value`), so the payload is read the same way for either tag.
#[repr(C, usize)]
enum HashDefault {
    Value(Value),
    Proc(Proc),
}

/// Discriminant values of [`HashDefault`], as generated code sees them.
pub const HASH_DEFAULT_TAG_VALUE: usize = 0;
pub const HASH_DEFAULT_TAG_PROC: usize = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HashId(usize);

///
/// An owned hash — the flags byte and the payload side by side. This is
/// the form built on the Rust stack (keyword-hash assembly, builtins
/// composing a result hash) and handed to [`Value::hash_from_inner`],
/// which moves the payload into a fresh RValue and the flags into its
/// header.
///
pub struct HashmapInner {
    flags: u8,
    body: HashBody,
}

impl std::default::Default for HashmapInner {
    fn default() -> Self {
        HashmapInner {
            flags: 0,
            body: HashBody {
                inline: empty_pairs(),
            },
        }
    }
}

impl Drop for HashmapInner {
    fn drop(&mut self) {
        if flags_is_boxed(self.flags) {
            // SAFETY: the flags byte says the boxed field is live.
            unsafe { ManuallyDrop::drop(&mut self.body.boxed) }
        }
    }
}

impl Clone for HashmapInner {
    /// Clones the content but resets the iteration count and the
    /// ruby2_keywords flag — a fresh copy is not being iterated, and
    /// `Hash#dup` drops the r2k flag (as CRuby).
    fn clone(&self) -> Self {
        self.as_ref().clone_inner()
    }
}

impl std::fmt::Debug for HashmapInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.as_ref().iter()).finish()
    }
}

impl HashmapInner {
    fn from_parts(flags: u8, body: HashBody) -> Self {
        HashmapInner { flags, body }
    }

    /// Decompose without running `Drop` (ownership of the boxed content
    /// moves to the caller along with the flags).
    pub(super) fn into_parts(self) -> (u8, HashBody) {
        let this = ManuallyDrop::new(self);
        // SAFETY: `this` is never dropped; the body is moved out exactly once.
        let body = unsafe { std::ptr::read(&this.body) };
        (this.flags, body)
    }

    pub(crate) fn as_ref(&self) -> HashRef<'_> {
        HashRef {
            flags: NonNull::from(&self.flags),
            body: NonNull::from(&self.body),
            _marker: PhantomData,
        }
    }

    pub(crate) fn as_mut(&mut self) -> HashRefMut<'_> {
        HashRefMut {
            flags: NonNull::from(&mut self.flags),
            body: NonNull::from(&mut self.body),
            _marker: PhantomData,
        }
    }

    pub fn new(map: RubyMap<Value, Value>) -> Self {
        if map.len() <= INLINE_CAP && map.iter().all(|(k, _)| is_inline_key(*k)) {
            let mut pairs = empty_pairs();
            let mut len = 0u8;
            for (k, v) in map.iter() {
                pairs[len as usize] = (*k, *v);
                len += 1;
            }
            Self::from_parts(len, HashBody { inline: pairs })
        } else {
            Self::from_parts(
                REP_BOXED,
                HashBody {
                    // `map_keys(Some)` re-wraps the keys into the boxed
                    // content's `Option` representation without re-hashing
                    // anything (entry order, stored hashes and the index
                    // table move verbatim).
                    boxed: ManuallyDrop::new(BoxedHash::new(HashContent::Map(Box::new(
                        map.map_keys(Some),
                    )))),
                },
            )
        }
    }

    /// An empty hash pre-sized for `n` entries. `n > INLINE_CAP` builds
    /// the boxed map with its capacity (and, past AR_MAX, its index
    /// table) up front, so bulk fills — `Hash#to_h` / `#transform_keys`
    /// / `#transform_values` building a result of a known size — skip
    /// the whole inline→boxed→indexed growth ladder. `HashmapInner::new`
    /// cannot be used for this: it converts any small (or empty) map to
    /// the inline form, dropping the reserved capacity.
    pub fn with_capacity(n: usize) -> Self {
        if n <= INLINE_CAP {
            Self::default()
        } else {
            Self::from_parts(
                REP_BOXED,
                HashBody {
                    boxed: ManuallyDrop::new(BoxedHash::new(HashContent::Map(Box::new(
                        RubyMap::with_capacity(n),
                    )))),
                },
            )
        }
    }

    /// The template of a constant Hash literal (`{"name" => 1, sym: 2}`
    /// with only immediate or String keys and immutable values), built
    /// at bytecode-emission time — so without a vm. Every key's digest
    /// and `eql?` are vm-free (`packed_digest` / `string_digest`, exactly
    /// what `insert` uses for them), so no Ruby code can run. Later
    /// pairs overwrite earlier ones, as `insert` does. The literal is
    /// materialized per evaluation by `RValue::deep_copy`.
    pub(crate) fn from_literal_pairs(pairs: &[(Value, Value)]) -> Self {
        debug_assert!(
            pairs
                .iter()
                .all(|(k, _)| k.is_packed_value() || k.is_rstring_inner().is_some())
        );
        if pairs.len() <= INLINE_CAP && pairs.iter().all(|(k, _)| is_inline_key(*k)) {
            let mut h = Self::default();
            let mut m = h.as_mut();
            for &(k, v) in pairs {
                if let Some(i) = m.as_ref().inline_pos_noobs(k) {
                    // SAFETY: rep is inline; i < len.
                    unsafe { m.body_mut().inline[i].1 = v };
                } else {
                    let len = m.inline_len();
                    // SAFETY: rep is inline; the slot exists (len < CAP).
                    unsafe { m.body_mut().inline[len] = (k, v) };
                    m.set_rep(len as u8 + 1);
                }
            }
            return h;
        }
        let mut map = RubyMap::with_capacity(pairs.len());
        for &(k, v) in pairs {
            if k.is_packed_value() {
                let hash = packed_digest(map.hasher(), k);
                map.insert_prehashed(hash, Some(k), v);
            } else {
                let s = k.as_rstring_inner();
                let hash = string_digest(map.hasher(), s);
                map.insert_prehashed_with(hash, Some(k), v, |ek| string_key_eq(ek, k, s));
            }
        }
        Self::from_parts(
            REP_BOXED,
            HashBody {
                boxed: ManuallyDrop::new(BoxedHash::new(HashContent::Map(Box::new(map)))),
            },
        )
    }

    fn new_boxed_with_default(map: RubyMap<Value, Value>, default: Option<HashDefault>) -> Self {
        Self::from_parts(
            REP_BOXED,
            HashBody {
                boxed: ManuallyDrop::new(BoxedHash {
                    content: HashContent::Map(Box::new(map.map_keys(Some))),
                    default: default.map(Box::new),
                    iter_lev: std::cell::Cell::new(0),
                    dead: std::cell::Cell::new(0),
                }),
            },
        )
    }

    /// A hash with a default value. A non-nil default needs the boxed
    /// form (the inline payload has no default slot), so this
    /// constructs it directly — no promotion, hence no re-hashing.
    pub fn new_with_default(map: RubyMap<Value, Value>, default: Value) -> Self {
        if default.is_nil() {
            Self::new(map)
        } else {
            Self::new_boxed_with_default(map, Some(HashDefault::Value(default)))
        }
    }

    pub fn new_with_default_proc(map: RubyMap<Value, Value>, default_proc: Proc) -> Self {
        Self::new_boxed_with_default(map, Some(HashDefault::Proc(default_proc)))
    }

    // Forwarders for the owned/builder form.

    pub fn insert(
        &mut self,
        k: Value,
        v: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        self.as_mut().insert(k, v, vm, globals)
    }

    pub fn remove(
        &mut self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Value>> {
        self.as_mut().remove(k, vm, globals)
    }

    pub fn get(&self, k: Value, vm: &mut Executor, globals: &mut Globals) -> Result<Option<Value>> {
        self.as_ref().get(k, vm, globals)
    }

    pub(crate) fn set_ruby2_keywords_flag(&mut self) {
        self.as_mut().set_ruby2_keywords_flag()
    }

    pub fn defalut_value(&self) -> Option<Value> {
        self.as_ref().defalut_value()
    }

    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.as_ref().len()
    }

    pub fn is_empty(&self) -> bool {
        self.as_ref().is_empty()
    }

    #[cfg(test)]
    pub(crate) fn keys(&self) -> Vec<Value> {
        self.as_ref().keys()
    }

    #[cfg(test)]
    pub(crate) fn values(&self) -> Vec<Value> {
        self.as_ref().values()
    }

    pub(crate) fn iter(&self) -> Iter<'_> {
        self.as_ref().iter()
    }

    pub fn compare_by_identity(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        self.as_mut().compare_by_identity(vm, globals)
    }

    pub fn set_compare_by_identity_empty(&mut self, ident: bool) -> Result<()> {
        self.as_mut().set_compare_by_identity_empty(ident)
    }

    pub fn is_compare_by_identity(&self) -> bool {
        self.as_ref().is_compare_by_identity()
    }

    pub fn clear(&mut self) -> Result<()> {
        self.as_mut().clear()
    }

    pub fn shift(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<(Value, Value)>> {
        self.as_mut().shift(vm, globals)
    }

    pub fn contains_key(&self, k: Value, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        self.as_ref().contains_key(k, vm, globals)
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for HashmapInner {
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        self.as_ref().eql(&other.as_ref(), vm, globals)
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for HashmapInner {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<()> {
        self.as_ref().ruby_hash(state, e, g)
    }
}

/// Can `k` be stored inline? Only packed immediates: their identity *is*
/// their content, so equality is bit comparison and the digest
/// recomputed at probe time can never disagree with an insert-time one
/// (heap keys — even strings — can be mutated, and a boxed map's
/// insert-time digest going stale is exactly the behavior `Hash#rehash`
/// exists for; that protocol stays with the boxed representation).
fn is_inline_key(k: Value) -> bool {
    k.is_packed_value()
}

/// A tag-resolved view of the content, so read paths can match on the
/// representation without touching the union directly.
enum ContentRef<'a> {
    Inline(&'a [(Value, Value)]),
    Map(&'a RubyMap<Option<Value>, Value>),
    Ident(&'a RubyMap<Option<IdentKey>, Value>),
}

///
/// A borrowed hash: the header flags byte and the payload, paired. The
/// flags pointer is kept raw (not `&u8`) because the iteration guard
/// mutates the inline iteration bits through a shared borrow, in the
/// same interior-mutability style the boxed form's `Cell` counter uses.
///
#[derive(Clone, Copy)]
pub struct HashRef<'a> {
    flags: NonNull<u8>,
    body: NonNull<HashBody>,
    _marker: PhantomData<&'a RValue>,
}

///
/// An exclusively borrowed hash; grants the mutating operations.
///
pub struct HashRefMut<'a> {
    flags: NonNull<u8>,
    body: NonNull<HashBody>,
    _marker: PhantomData<&'a mut RValue>,
}

impl<'a> HashRef<'a> {
    /// Borrow the hash stored in `rv`.
    ///
    /// # Safety
    /// `rv` must be a live HASH object.
    pub(super) unsafe fn from_rvalue(rv: &'a RValue) -> Self {
        unsafe {
            HashRef {
                flags: rv.ty_flags_ptr(),
                body: NonNull::from(&*rv.kind.hash),
                _marker: PhantomData,
            }
        }
    }

    fn flags(&self) -> u8 {
        // SAFETY: the pointee outlives 'a.
        unsafe { *self.flags.as_ref() }
    }

    fn body(&self) -> &'a HashBody {
        // SAFETY: the pointee outlives 'a.
        unsafe { self.body.as_ref() }
    }

    fn is_inline(&self) -> bool {
        !flags_is_boxed(self.flags())
    }

    fn inline_len(&self) -> usize {
        debug_assert!(self.is_inline());
        (self.flags() & REP_MASK) as usize
    }

    fn inline_pairs(&self) -> &'a [(Value, Value)] {
        let len = self.inline_len();
        // SAFETY: rep is inline; len ≤ INLINE_CAP and the array is
        // always fully initialized.
        unsafe { &self.body().inline[..len] }
    }

    fn boxed(&self) -> &'a BoxedHash {
        debug_assert!(!self.is_inline());
        // SAFETY: rep is boxed.
        unsafe { &self.body().boxed }
    }

    fn content(&self) -> ContentRef<'a> {
        if self.is_inline() {
            ContentRef::Inline(self.inline_pairs())
        } else {
            match &self.boxed().content {
                HashContent::Map(m) => ContentRef::Map(m),
                HashContent::IdentMap(m) => ContentRef::Ident(m),
            }
        }
    }

    fn default_ref(&self) -> Option<&'a HashDefault> {
        if self.is_inline() {
            None
        } else {
            self.boxed().default.as_deref()
        }
    }

    fn id(&self) -> HashId {
        HashId(self.body.as_ptr() as usize)
    }

    /// Is this an identity-keyed *inline* hash?
    fn is_ident_inline(&self) -> bool {
        self.is_inline() && self.flags() & IDENT_BIT != 0
    }

    /// Position of `k` among the inline pairs when no `#hash` dispatch
    /// can be observable: an identity-keyed hash compares ids for any
    /// key, and in an eql?-keyed hash only a packed probe can match
    /// (packed values are eql? iff their bits are equal — `Value::eql`'s
    /// immediate arm — so the id scan is exactly a map's digest-probe +
    /// eql? for them). An eql?-keyed heap probe returns `None`; whether
    /// its `#hash` must be observed is the caller's business.
    fn inline_pos_noobs(&self, k: Value) -> Option<usize> {
        if !self.is_ident_inline() && !k.is_packed_value() {
            return None;
        }
        self.inline_pairs()
            .iter()
            .position(|(ek, _)| ek.id() == k.id())
    }

    /// Position of `k` among the inline pairs, observing the same
    /// `#hash` protocol a boxed-map lookup would: an eql?-keyed heap
    /// probe is hashed exactly once (dispatching a user-defined `#hash`
    /// and propagating its errors) even though it can never be eql? to a
    /// packed key. Identity-keyed lookups never hash (matching the
    /// IdentKey map, which digests ids natively).
    fn inline_pos(
        &self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<usize>> {
        if self.is_ident_inline() || k.is_packed_value() {
            Ok(self.inline_pos_noobs(k))
        } else {
            k.calculate_hash(vm, globals)?;
            Ok(None)
        }
    }

    /// Tombstoned entries currently in the boxed entry vector (zero for the
    /// inline representation, which never tombstones — a delete during
    /// iteration promotes to boxed first).
    fn dead_count(&self) -> usize {
        if self.is_inline() {
            0
        } else {
            self.boxed().dead.get() as usize
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self.content() {
            ContentRef::Inline(pairs) => pairs.len(),
            ContentRef::Map(m) => m.len() - self.dead_count(),
            ContentRef::Ident(m) => m.len() - self.dead_count(),
        }
    }

    /// The raw entry-vector length, tombstones included — the exclusive
    /// upper bound for a position-indexed walk (`__entry_count`). Equals
    /// [`Self::len`] whenever no tombstones are outstanding.
    pub(crate) fn entry_count(&self) -> usize {
        match self.content() {
            ContentRef::Inline(pairs) => pairs.len(),
            ContentRef::Map(m) => m.len(),
            ContentRef::Ident(m) => m.len(),
        }
    }

    /// Is the `index`-th entry live — in range and not a tombstone? The
    /// `__live_at` intrinsic; a position-indexed walk asks this before
    /// touching `entry_at`.
    pub(crate) fn live_at(&self, index: usize) -> bool {
        match self.content() {
            ContentRef::Inline(pairs) => index < pairs.len(),
            ContentRef::Map(m) => m.get_index(index).is_some_and(|(k, _)| k.is_some()),
            ContentRef::Ident(m) => m.get_index(index).is_some_and(|(k, _)| k.is_some()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The `index`-th entry in insertion order, or `None` when out of range
    /// **or tombstoned** — the sentinel key must never surface as a Ruby
    /// value, so a dead entry answers like an out-of-range one.
    /// O(1) for every representation: the inline pairs and `RubyMap`'s entry
    /// vector are both position-addressable. Used by the `__key_at` /
    /// `__value_at` intrinsics so Ruby-level iteration can be a `while` loop
    /// over indices instead of an `each` block.
    pub(crate) fn entry_at(&self, index: usize) -> Option<(Value, Value)> {
        match self.content() {
            ContentRef::Inline(pairs) => pairs.get(index).copied(),
            ContentRef::Map(m) => m
                .get_index(index)
                .and_then(|(k, v)| k.map(|k| (k, *v))),
            ContentRef::Ident(m) => m
                .get_index(index)
                .and_then(|(k, v)| k.map(|k| (k.0, *v))),
        }
    }

    pub fn is_compare_by_identity(&self) -> bool {
        if self.is_inline() {
            self.flags() & IDENT_BIT != 0
        } else {
            matches!(self.content(), ContentRef::Ident(_))
        }
    }

    pub(crate) fn ruby2_keywords_flag(&self) -> bool {
        self.flags() & R2K_BIT != 0
    }

    pub fn defalut_value(&self) -> Option<Value> {
        match self.default_ref() {
            None => Some(Value::nil()),
            Some(HashDefault::Value(v)) => Some(*v),
            Some(HashDefault::Proc(_)) => None,
        }
    }

    pub fn defalut_proc(&self) -> Option<Proc> {
        if let Some(HashDefault::Proc(p)) = self.default_ref() {
            Some(*p)
        } else {
            None
        }
    }

    /// The hash's default *value* (`Hash.new(x)`), if one is set and it
    /// is a plain value rather than a default proc. Returns `None` for
    /// the nil default or a default proc.
    pub fn default_value(&self) -> Option<Value> {
        match self.default_ref() {
            Some(HashDefault::Value(v)) if !v.is_nil() => Some(*v),
            _ => None,
        }
    }

    pub fn get(&self, k: Value, vm: &mut Executor, globals: &mut Globals) -> Result<Option<Value>> {
        Ok(match self.content() {
            ContentRef::Inline(pairs) => self.inline_pos(k, vm, globals)?.map(|i| pairs[i].1),
            ContentRef::Map(m) => {
                // See `HashRefMut::insert`: a packed key probes vm-free,
                // and so does a String key (vm-free digest and byte
                // equality — `string_digest` / `string_key_eq`).
                if k.is_packed_value() {
                    let hash = packed_digest(m.hasher(), k);
                    m.get_prehashed(hash, &Some(k), vm, globals)?.copied()
                } else if let Some(s) = k.is_rstring_inner() {
                    let hash = string_digest(m.hasher(), s);
                    m.get_prehashed_with(hash, |ek| string_key_eq(ek, k, s), vm, globals)?
                        .copied()
                } else {
                    m.get(&k, vm, globals)?.copied()
                }
            }
            ContentRef::Ident(m) => m.get(&IdentKey(k), vm, globals)?.copied(),
        })
    }

    pub(crate) fn contains_key(
        &self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<bool> {
        match self.content() {
            ContentRef::Inline(_) => Ok(self.inline_pos(k, vm, globals)?.is_some()),
            ContentRef::Map(m) => {
                // See `HashRefMut::insert`: a packed key probes vm-free,
                // and so does a String key (vm-free digest and byte
                // equality — `string_digest` / `string_key_eq`).
                if k.is_packed_value() {
                    let hash = packed_digest(m.hasher(), k);
                    Ok(m.get_prehashed(hash, &Some(k), vm, globals)?.is_some())
                } else if let Some(s) = k.is_rstring_inner() {
                    let hash = string_digest(m.hasher(), s);
                    Ok(m
                        .get_prehashed_with(hash, |ek| string_key_eq(ek, k, s), vm, globals)?
                        .is_some())
                } else {
                    m.contains_key(&k, vm, globals)
                }
            }
            ContentRef::Ident(m) => m.contains_key(&IdentKey(k), vm, globals),
        }
    }

    pub(crate) fn iter(&self) -> Iter<'a> {
        match self.content() {
            ContentRef::Inline(pairs) => Iter::Inline(pairs.iter()),
            ContentRef::Map(m) => Iter::Map(m.iter()),
            ContentRef::Ident(m) => Iter::IdentMap(m.iter()),
        }
    }

    pub(crate) fn keys(&self) -> Vec<Value> {
        self.iter().map(|(k, _)| k).collect()
    }

    pub(crate) fn values(&self) -> Vec<Value> {
        self.iter().map(|(_, v)| v).collect()
    }

    /// Is a traversal currently in progress on this hash?
    fn iter_active(&self) -> bool {
        if self.is_inline() {
            self.flags() & ITER_MASK != 0
        } else {
            self.boxed().iter_lev.get() > 0
        }
    }

    /// Raise `RuntimeError` if a traversal is currently in progress on
    /// this hash. Called from mutating entry points that change the set
    /// of keys (delete, clear, shift, compare_by_identity). Updating the
    /// value of an already-present key does *not* go through this check —
    /// matching CRuby, where `h.each { h[existing] = v }` is allowed but
    /// `h.each { h[new] = v }` or `h.each { h.delete(k) }` raises.
    pub fn check_iter(&self) -> Result<()> {
        if self.iter_active() {
            Err(MonorubyErr::runtimeerr(
                "can't modify hash during iteration",
            ))
        } else {
            Ok(())
        }
    }

    /// Start a traversal: returns an RAII guard that decrements the
    /// iteration count when dropped. Takes `&self` (not `&mut`) so the
    /// hash can be iterated concurrently with the guard being alive.
    ///
    /// Inline hashes track the depth in two header-flag bits; if a
    /// pathological nesting exceeds that range the extra guards become
    /// no-ops — the hash simply stays marked as iterating until the
    /// tracked (outer) guards unwind, which keeps the "no new keys
    /// while iterating" rule sound for the usual LIFO guard order.
    pub fn iter_guard(&self) -> IterGuard<'a> {
        let real = self.iter_incr();
        IterGuard { h: *self, real }
    }

    ///
    /// Take one iteration reference, returning whether it was actually
    /// recorded. The inline representation holds the depth in two flag
    /// bits, so a deeply nested traversal saturates and is admitted as a
    /// no-op — the caller must pass that answer back to [`Self::iter_decr`]
    /// rather than assume a decrement is owed.
    ///
    pub fn iter_incr(&self) -> bool {
        if self.is_inline() {
            let flags = self.flags();
            let depth = (flags & ITER_MASK) >> ITER_SHIFT;
            if depth < ITER_MAX {
                let new = (flags & !ITER_MASK) | ((depth + 1) << ITER_SHIFT);
                // SAFETY: interior mutation of the iteration bits through
                // the shared borrow — the inline analogue of the boxed
                // form's `Cell` counter. No other bits change.
                unsafe { *self.flags.as_ptr() = new };
                true
            } else {
                false
            }
        } else {
            let lev = &self.boxed().iter_lev;
            lev.set(lev.get() + 1);
            true
        }
    }

    ///
    /// Release an iteration reference taken by [`Self::iter_incr`]. `real`
    /// must be that call's return value.
    ///
    pub fn iter_decr(&self, real: bool) {
        if !real {
            return;
        }
        if self.is_inline() {
            let flags = self.flags();
            let depth = (flags & ITER_MASK) >> ITER_SHIFT;
            debug_assert!(depth > 0);
            let new = (flags & !ITER_MASK) | ((depth - 1) << ITER_SHIFT);
            // SAFETY: see `iter_incr`.
            unsafe { *self.flags.as_ptr() = new };
        } else {
            let lev = &self.boxed().iter_lev;
            lev.set(lev.get() - 1);
        }
    }

    /// A detached copy of this hash: content cloned, iteration count and
    /// ruby2_keywords flag reset (`Hash#dup` semantics for the flag).
    pub(crate) fn clone_inner(&self) -> HashmapInner {
        HashmapInner::from_parts(sanitize_dup_flags(self.flags()), self.clone_body())
    }

    /// Clone just the payload (for `dup`/`clone`, which copy the header
    /// — and with it the representation bits — separately).
    pub(super) fn clone_body(&self) -> HashBody {
        if self.is_inline() {
            HashBody {
                // SAFETY: rep is inline; the pair array is Copy.
                inline: unsafe { self.body().inline },
            }
        } else {
            let b = self.boxed();
            let mut content = b.content.clone();
            // A copy is not being iterated, so it owes no position
            // stability: drop any tombstones now rather than carrying the
            // dead count over.
            if b.dead.get() > 0 {
                match &mut content {
                    HashContent::Map(m) => m.compact_tombstones(|k| k.is_none()),
                    HashContent::IdentMap(m) => m.compact_tombstones(|k| k.is_none()),
                }
            }
            HashBody {
                boxed: ManuallyDrop::new(BoxedHash {
                    content,
                    default: b.default.clone(),
                    iter_lev: std::cell::Cell::new(0),
                    dead: std::cell::Cell::new(0),
                }),
            }
        }
    }

    ///
    /// Generational GC: does this hash reference any young (non-old)
    /// heap object — among its keys, values, or default value/proc?
    /// Used for the remember-on-promote decision. See `doc/gc.md`.
    ///
    pub(crate) fn young_child_exists(&self, alloc: &alloc::Allocator<RValue>) -> bool {
        fn is_young(v: Value, alloc: &alloc::Allocator<RValue>) -> bool {
            v.try_rvalue().is_some_and(|rv| !alloc.is_old(rv))
        }
        match self.default_ref() {
            Some(HashDefault::Proc(p)) => {
                if is_young((*p).into(), alloc) {
                    return true;
                }
            }
            Some(HashDefault::Value(v)) => {
                if is_young(*v, alloc) {
                    return true;
                }
            }
            None => {}
        }
        self.iter()
            .any(|(k, v)| is_young(k, alloc) || is_young(v, alloc))
    }

    pub fn debug(&self, store: &Store) -> String {
        match self.len() {
            0 => "{}".to_string(),
            i => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in self.iter().take(3) {
                    let k_inspect = if let Some(h) = k.try_hash_ty()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        k.debug(store)
                    };
                    let v_inspect = if let Some(h) = v.try_hash_ty()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        v.debug(store)
                    };
                    result = if first {
                        format!("{k_inspect}=>{v_inspect}")
                    } else {
                        format!("{result}, {k_inspect}=>{v_inspect}")
                    };
                    first = false;
                }
                if i > 3 {
                    format! {"{{{} .. }}", result}
                } else {
                    format! {"{{{}}}", result}
                }
            }
        }
    }

    pub fn to_s(&self, store: &Store, self_id: u64) -> String {
        let mut set = HashSet::new();
        set.insert(self_id);
        self.inspect_inner(store, &mut set)
    }

    pub fn inspect_inner(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        match self.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in self.iter() {
                    let k_inspect = k.inspect_inner(store, set);
                    let v_inspect = v.inspect_inner(store, set);
                    let s = if let Some(k) = k.try_symbol() {
                        format!("{}: {v_inspect}", crate::value::symbol_hash_label(k, false))
                    } else {
                        format!("{k_inspect} => {v_inspect}")
                    };
                    result = if first { s } else { format!("{result}, {s}") };
                    first = false;
                }
                format! {"{{{}}}", result}
            }
        }
    }
}

impl std::fmt::Debug for HashRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for HashRef<'_> {
    // This type of equality is used for comparison for keys of Hash.
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        // A hash and an identity-compared hash never compare equal.
        if self.is_compare_by_identity() != other.is_compare_by_identity() {
            return Ok(false);
        }
        // Same mode: representation (inline vs boxed) must not matter,
        // so compare pairwise through `get` — the same probe protocol
        // RubyMap::eql uses (which for identity hashes is native id
        // digesting/comparison on both sides).
        if self.len() != other.len() {
            return Ok(false);
        }
        for (k, v) in self.iter() {
            match other.get(k, vm, globals)? {
                None => return Ok(false),
                Some(ov) => {
                    if !v.eql(&ov, vm, globals)? {
                        return Ok(false);
                    }
                }
            }
        }
        Ok(true)
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for HashRef<'_> {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<()> {
        match self.content() {
            ContentRef::Inline(pairs) => {
                if self.is_ident_inline() {
                    // digest keys by id, matching `IdentKey::ruby_hash`
                    // so inline and boxed identity hashes digest alike
                    for (key, val) in pairs {
                        key.id().hash(state);
                        val.ruby_hash(state, e, g)?;
                    }
                } else {
                    for (key, val) in pairs {
                        key.ruby_hash(state, e, g)?;
                        val.ruby_hash(state, e, g)?;
                    }
                }
            }
            ContentRef::Map(h) => {
                for (key, val) in h.iter() {
                    key.ruby_hash(state, e, g)?;
                    val.ruby_hash(state, e, g)?;
                }
            }
            ContentRef::Ident(h) => {
                for (key, val) in h.iter() {
                    key.ruby_hash(state, e, g)?;
                    val.ruby_hash(state, e, g)?;
                }
            }
        }
        Ok(())
    }
}

impl alloc::GC<RValue> for HashRef<'_> {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        match self.default_ref() {
            Some(HashDefault::Proc(p)) => p.mark(alloc),
            Some(HashDefault::Value(v)) => v.mark(alloc),
            None => {}
        }
        for (k, v) in self.iter() {
            k.mark(alloc);
            v.mark(alloc);
        }
    }
}

impl<'a> HashRefMut<'a> {
    /// Borrow the hash stored in `rv` exclusively.
    ///
    /// # Safety
    /// `rv` must be a live HASH object.
    pub(super) unsafe fn from_rvalue(rv: &'a mut RValue) -> Self {
        unsafe {
            HashRefMut {
                flags: rv.ty_flags_ptr(),
                body: NonNull::from(&mut *rv.kind.hash),
                _marker: PhantomData,
            }
        }
    }

    pub(crate) fn as_ref(&self) -> HashRef<'_> {
        HashRef {
            flags: self.flags,
            body: self.body,
            _marker: PhantomData,
        }
    }

    fn flags(&self) -> u8 {
        unsafe { *self.flags.as_ref() }
    }

    fn set_flags(&mut self, flags: u8) {
        unsafe { *self.flags.as_mut() = flags }
    }

    fn set_rep(&mut self, rep: u8) {
        let f = self.flags();
        self.set_flags((f & !REP_MASK) | rep);
    }

    fn body_mut(&mut self) -> &mut HashBody {
        unsafe { self.body.as_mut() }
    }

    fn is_inline(&self) -> bool {
        !flags_is_boxed(self.flags())
    }

    fn inline_len(&self) -> usize {
        self.as_ref().inline_len()
    }

    fn boxed_mut(&mut self) -> &mut BoxedHash {
        debug_assert!(!self.is_inline());
        // SAFETY: rep is boxed.
        unsafe { &mut self.body_mut().boxed }
    }

    /// Replace the body wholesale, dropping the previous boxed content
    /// if any, and set the representation bits to match.
    fn install(&mut self, rep: u8, body: HashBody) {
        if !self.is_inline() {
            // SAFETY: rep says the boxed field is live; it is not
            // touched again before the overwrite below.
            unsafe { ManuallyDrop::drop(&mut self.body_mut().boxed) }
        }
        *self.body_mut() = body;
        self.set_rep(rep);
    }

    /// Move an inline hash into its boxed form (the 4th pair, a heap
    /// key, a default, or `compare_by_identity` arrived). Re-inserting
    /// packed keys hashes them natively — no Ruby code runs. The live
    /// iteration depth (header bits) migrates into the exact counter.
    fn promote(&mut self, ident: bool, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        debug_assert!(self.is_inline());
        let flags = self.flags();
        let iter_depth = ((flags & ITER_MASK) >> ITER_SHIFT) as u32;
        let content = if ident {
            let mut map = RubyMap::default();
            for (k, v) in self.as_ref().inline_pairs() {
                map.insert(Some(IdentKey(*k)), *v, vm, globals)?;
            }
            HashContent::IdentMap(Box::new(map))
        } else {
            let mut map = RubyMap::default();
            for (k, v) in self.as_ref().inline_pairs() {
                map.insert(Some(*k), *v, vm, globals)?;
            }
            HashContent::Map(Box::new(map))
        };
        let boxed = BoxedHash {
            content,
            default: None,
            iter_lev: std::cell::Cell::new(iter_depth),
            dead: std::cell::Cell::new(0),
        };
        *self.body_mut() = HashBody {
            boxed: ManuallyDrop::new(boxed),
        };
        // rep := boxed, iteration bits cleared (now tracked in iter_lev)
        self.set_flags((flags & R2K_BIT) | REP_BOXED);
        Ok(())
    }

    pub fn insert(
        &mut self,
        k: Value,
        v: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        // Stale tombstones must go before an insert: growing the index
        // table re-inserts every raw bucket, which would resurrect them.
        // (During iteration the new-key check below raises first, and an
        // existing-key update touches no bucket positions, so tombstones
        // can only be stale — no traversal live — when this compacts.)
        if !self.as_ref().iter_active() {
            self.compact_if_dirty();
        }
        if self.as_ref().iter_active() && !self.as_ref().contains_key(k, vm, globals)? {
            return Err(MonorubyErr::runtimeerr(
                "can't add a new key into hash during iteration",
            ));
        }
        if self.is_inline() {
            let len = self.inline_len();
            let ident = self.as_ref().is_ident_inline();
            if let Some(i) = self.as_ref().inline_pos_noobs(k) {
                // SAFETY: rep is inline; i < len.
                unsafe { self.body_mut().inline[i].1 = v };
            } else if (ident || is_inline_key(k)) && len < INLINE_CAP {
                // An identity-keyed inline hash accepts any key (id
                // probing never goes stale); an eql?-keyed one only
                // packed immediates.
                // SAFETY: rep is inline; the slot exists (len < CAP).
                unsafe { self.body_mut().inline[len] = (k, v) };
                self.set_rep(len as u8 + 1);
            } else {
                // 4th pair — or, in eql? mode, a heap key: move to the
                // boxed map. An eql?-keyed heap key's user-defined
                // #hash is observed (once) by the map insert, matching
                // the boxed-representation protocol.
                self.promote(ident, vm, globals)?;
                match &mut self.boxed_mut().content {
                    HashContent::Map(m) => m.insert(Some(k), v, vm, globals)?,
                    HashContent::IdentMap(m) => m.insert(Some(IdentKey(k)), v, vm, globals)?,
                };
            }
            return Ok(());
        }
        match &mut self.boxed_mut().content {
            HashContent::Map(m) => {
                if k.is_packed_value() {
                    // A packed key's `eql?` is exactly bit equality (an
                    // immediate is never `eql?` to a heap value, and
                    // immediate-vs-immediate compares ids), and its digest
                    // is vm-free — so the whole probe is vm-free.
                    let hash = packed_digest(m.hasher(), k);
                    m.insert_prehashed(hash, Some(k), v);
                } else if let Some(s) = k.is_rstring_inner() {
                    // A String key's digest and `eql?` are likewise
                    // vm-free: byte content only, dispatching neither
                    // `String#hash` nor `String#eql?` — exactly what the
                    // general probe does for a String key
                    // (`string_digest` / `string_key_eq`).
                    let hash = string_digest(m.hasher(), s);
                    m.insert_prehashed_with(hash, Some(k), v, |ek| string_key_eq(ek, k, s));
                } else {
                    m.insert(Some(k), v, vm, globals)?;
                }
            }
            HashContent::IdentMap(m) => {
                m.insert(Some(IdentKey(k)), v, vm, globals)?;
            }
        }
        Ok(())
    }

    /// Compact any tombstones left by a finished (or broken-out-of)
    /// traversal. Must run before any operation that walks or rebuilds the
    /// raw buckets as if all were live — insertion (whose index-table
    /// growth re-inserts every bucket), `compare_by_identity`'s rebuild —
    /// and is called from every such `&mut` entry point. Never called with
    /// a traversal live: the callers either raise on iteration first or
    /// take the tombstoning branch instead.
    fn compact_if_dirty(&mut self) {
        if self.is_inline() {
            return;
        }
        let b = self.boxed_mut();
        if b.dead.get() == 0 {
            return;
        }
        debug_assert_eq!(b.iter_lev.get(), 0);
        match &mut b.content {
            HashContent::Map(m) => m.compact_tombstones(|k| k.is_none()),
            HashContent::IdentMap(m) => m.compact_tombstones(|k| k.is_none()),
        }
        b.dead.set(0);
    }

    pub fn remove(
        &mut self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Value>> {
        // Removing a key during iteration is explicitly allowed — CRuby
        // behaves the same way (see ruby/spec core/hash/delete_spec.rb
        // "allows removing a key while iterating") — but the traversal is
        // walking the entries by position, so the entry cannot be compacted
        // out from under it. Instead it is tombstoned in place: dropped
        // from the index table, key overwritten with the sentinel, so the
        // walk sees a dead slot (skipped via `__live_at`) and every
        // position it has yet to visit stays put. CRuby semantics follow:
        // a deleted not-yet-visited entry is not yielded, and deleting a
        // visited (or the current) one skips nothing. The inline form
        // cannot hold a dead slot, so it promotes to boxed first (its
        // iteration depth migrates with it).
        if self.as_ref().iter_active() {
            if self.is_inline() {
                let ident = self.as_ref().is_compare_by_identity();
                self.promote(ident, vm, globals)?;
            }
            let removed = match &mut self.boxed_mut().content {
                HashContent::Map(m) => m
                    .tombstone_remove(&k, None, Value::nil(), vm, globals)?
                    .map(|(_, _, v)| v),
                HashContent::IdentMap(m) => m
                    .tombstone_remove(&IdentKey(k), None, Value::nil(), vm, globals)?
                    .map(|(_, _, v)| v),
            };
            if removed.is_some() {
                let dead = &self.boxed_mut().dead;
                dead.set(dead.get() + 1);
            }
            return Ok(removed);
        }
        if self.is_inline() {
            return Ok(match self.as_ref().inline_pos(k, vm, globals)? {
                Some(i) => Some(self.inline_remove_at(i)),
                None => None,
            });
        }
        self.compact_if_dirty();
        match &mut self.boxed_mut().content {
            HashContent::Map(m) => m.shift_remove(&k, vm, globals),
            HashContent::IdentMap(m) => m.shift_remove(&IdentKey(k), vm, globals),
        }
    }

    /// Overwrite the value of the `index`-th entry in place (the
    /// `__set_value_at` intrinsic behind `Hash#transform_values(!)`:
    /// the key set is untouched, so no digest or probe is needed).
    /// Out-of-range and tombstoned positions are ignored.
    pub(crate) fn set_value_at(&mut self, index: usize, v: Value) {
        if self.is_inline() {
            if index < self.inline_len() {
                // SAFETY: rep is inline; index < len.
                unsafe { self.body_mut().inline[index].1 = v };
            }
            return;
        }
        match &mut self.boxed_mut().content {
            HashContent::Map(m) => {
                if let Some((k, slot)) = m.get_index_mut(index)
                    && k.is_some()
                {
                    *slot = v;
                }
            }
            HashContent::IdentMap(m) => {
                if let Some((k, slot)) = m.get_index_mut(index)
                    && k.is_some()
                {
                    *slot = v;
                }
            }
        }
    }

    /// Remove the inline pair at `i`, closing the gap (insertion order
    /// is preserved, like the boxed map's `shift_remove`).
    fn inline_remove_at(&mut self, i: usize) -> Value {
        let len = self.inline_len();
        debug_assert!(i < len);
        // SAFETY: rep is inline; indices stay within the always-initialized array.
        unsafe {
            let v = self.body_mut().inline[i].1;
            for j in i..len - 1 {
                self.body_mut().inline[j] = self.body_mut().inline[j + 1];
            }
            self.body_mut().inline[len - 1] = (Value::nil(), Value::nil());
            self.set_rep(len as u8 - 1);
            v
        }
    }

    pub fn clear(&mut self) -> Result<()> {
        self.as_ref().check_iter()?;
        if self.is_inline() {
            // Keep the mode bit: a cleared identity hash stays
            // identity-compared.
            self.install(
                0,
                HashBody {
                    inline: empty_pairs(),
                },
            );
        } else if self.boxed_mut().default.is_some() {
            // A default keeps the hash boxed; just empty the map (the
            // default itself survives, as in CRuby: `clear` does not
            // touch it). Any stale tombstones vanish with the entries, so
            // the dead count resets alongside.
            match &mut self.boxed_mut().content {
                HashContent::Map(m) => m.clear(),
                HashContent::IdentMap(m) => m.clear(),
            }
            self.boxed_mut().dead.set(0);
        } else {
            // Give the boxed storage back — an emptied, default-less
            // hash is small again by definition. An identity-compared
            // hash keeps its mode via the inline mode bit.
            let ident = matches!(self.boxed_mut().content, HashContent::IdentMap(_));
            self.install(
                0,
                HashBody {
                    inline: empty_pairs(),
                },
            );
            if ident {
                let f = self.flags();
                self.set_flags(f | IDENT_BIT);
            }
        }
        Ok(())
    }

    pub fn shift(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<(Value, Value)>> {
        // Shifting an entry during iteration is allowed, like `delete` —
        // CRuby permits `h.each { h.shift }`. Only *adding* a key is
        // rejected mid-iteration.
        if self.as_ref().len() == 0 {
            return Ok(None);
        }
        if self.as_ref().iter_active() {
            // Same discipline as `remove` during iteration: tombstone the
            // first *live* entry so positions stay stable under the
            // traversal.
            if self.is_inline() {
                let ident = self.as_ref().is_compare_by_identity();
                self.promote(ident, vm, globals)?;
            }
            let first_live = (0..self.as_ref().entry_count())
                .find(|&i| self.as_ref().live_at(i))
                .expect("len > 0 implies a live entry");
            let removed = match &mut self.boxed_mut().content {
                HashContent::Map(m) => m
                    .tombstone_index(first_live, None, Value::nil(), vm, globals)?
                    .map(|(k, v)| (k.expect("first_live is a live entry"), v)),
                HashContent::IdentMap(m) => m
                    .tombstone_index(first_live, None, Value::nil(), vm, globals)?
                    .map(|(k, v)| (k.expect("first_live is a live entry").0, v)),
            };
            debug_assert!(removed.is_some());
            if removed.is_some() {
                let dead = &self.boxed_mut().dead;
                dead.set(dead.get() + 1);
            }
            return Ok(removed);
        }
        if self.is_inline() {
            // SAFETY: rep is inline; len > 0 was checked above.
            let k = unsafe { self.body_mut().inline[0].0 };
            let v = self.inline_remove_at(0);
            return Ok(Some((k, v)));
        }
        self.compact_if_dirty();
        // Just compacted, so entry 0 is live (`Some`) — `len > 0` was
        // checked above.
        match &mut self.boxed_mut().content {
            HashContent::Map(m) => m
                .shift_remove_index(0, vm, globals)
                .map(|opt| opt.map(|(k, v)| (k.expect("compacted"), v))),
            HashContent::IdentMap(m) => m
                .shift_remove_index(0, vm, globals)
                .map(|opt| opt.map(|(k, v)| (k.expect("compacted").0, v))),
        }
    }

    /// Set the default value. A non-nil default needs the boxed form
    /// (the inline payload has no default slot), so setting one on an
    /// inline hash promotes it — that re-hashes the packed keys
    /// natively, no Ruby code runs.
    pub fn set_defalut_value(
        &mut self,
        default: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        // A nil default is indistinguishable from no default.
        if default.is_nil() {
            if !self.is_inline() {
                self.boxed_mut().default = None;
            }
            return Ok(());
        }
        if self.is_inline() {
            let ident = self.as_ref().is_ident_inline();
            self.promote(ident, vm, globals)?;
        }
        self.boxed_mut().default = Some(Box::new(HashDefault::Value(default)));
        Ok(())
    }

    pub fn set_defalut_proc(
        &mut self,
        default_proc: Proc,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        if self.is_inline() {
            let ident = self.as_ref().is_ident_inline();
            self.promote(ident, vm, globals)?;
        }
        self.boxed_mut().default = Some(Box::new(HashDefault::Proc(default_proc)));
        Ok(())
    }

    pub(crate) fn set_ruby2_keywords_flag(&mut self) {
        let f = self.flags();
        self.set_flags(f | R2K_BIT);
    }

    pub(crate) fn unset_ruby2_keywords_flag(&mut self) {
        let f = self.flags();
        self.set_flags(f & !R2K_BIT);
    }

    pub fn compare_by_identity(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        self.as_ref().check_iter()?;
        // The rebuild below walks the raw buckets; stale tombstones must
        // not come along.
        self.compact_if_dirty();
        if self.is_inline() {
            // Just flip the mode bit: the existing keys are packed
            // immediates, for which eql? and identity coincide, so the
            // pairs stay valid verbatim — and the hash stays inline.
            let f = self.flags();
            self.set_flags(f | IDENT_BIT);
            return Ok(());
        }
        if let HashContent::Map(m) = &self.boxed_mut().content {
            let mut new_map = RubyMap::default();
            for (k, v) in m.iter() {
                // Compacted above (`compact_if_dirty`), so every key is live.
                let Some(k) = k else { continue };
                new_map.insert(Some(IdentKey(*k)), *v, vm, globals)?;
            }
            self.boxed_mut().content = HashContent::IdentMap(Box::new(new_map));
        }
        Ok(())
    }

    ///
    /// Set the key-comparison mode of an **empty** hash, in either
    /// direction.
    ///
    /// This is the primitive behind CRuby's `rb_hash_replace`, which
    /// adopts the source hash's comparison mode wholesale — including
    /// turning identity comparison *off*, which `compare_by_identity` (a
    /// one-way door) cannot do. Turning it off on a populated map would
    /// have to merge keys that are `eql?` but not identical, silently
    /// dropping entries; requiring an empty map keeps the primitive
    /// lossless. Callers that rebuild a container (`Set#replace` /
    /// `#map!` / `#flatten!`) clear it first anyway.
    ///
    pub fn set_compare_by_identity_empty(&mut self, ident: bool) -> Result<()> {
        self.as_ref().check_iter()?;
        assert_eq!(
            0,
            self.as_ref().len(),
            "the map must be empty to change its mode"
        );
        if ident {
            if self.is_inline() {
                // stays inline: the empty pair array serves both modes
                let f = self.flags();
                self.set_flags(f | IDENT_BIT);
            } else if !self.as_ref().is_compare_by_identity() {
                self.boxed_mut().content = HashContent::IdentMap(Box::new(RubyMap::default()));
            }
        } else if self.is_inline() {
            let f = self.flags();
            self.set_flags(f & !IDENT_BIT);
        } else if self.as_ref().is_compare_by_identity() {
            if self.boxed_mut().default.is_some() {
                self.boxed_mut().content = HashContent::Map(Box::new(RubyMap::default()));
            } else {
                self.install(
                    0,
                    HashBody {
                        inline: empty_pairs(),
                    },
                );
            }
        }
        Ok(())
    }
}

/// RAII guard returned from [`HashRef::iter_guard`].
///
/// While alive it represents a single layer of active iteration;
/// dropping it (normal return or unwinding via `Result::Err`) decrements
/// the count — wherever it currently lives: a hash promoted mid-guard
/// (e.g. `h.each { h.default = x }`) migrated its depth into the boxed
/// counter, and the drop follows the representation.
pub struct IterGuard<'a> {
    h: HashRef<'a>,
    /// False for a guard that saturated the inline depth bits and was
    /// admitted as a no-op (see `iter_guard`).
    real: bool,
}

impl Drop for IterGuard<'_> {
    fn drop(&mut self) {
        self.h.iter_decr(self.real);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct IdentKey(pub Value);

impl Deref for IdentKey {
    type Target = Value;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for IdentKey {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        _: &mut Executor,
        _: &mut Globals,
    ) -> Result<()> {
        (self.0.id()).hash(state);
        Ok(())
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for IdentKey {
    // Object#eql?()
    // This type of equality is used for comparison for keys of Hash.
    fn eql(&self, other: &Self, _: &mut Executor, _: &mut Globals) -> Result<bool> {
        Ok(self.0.id() == other.0.id())
    }
}

/// Query a live (`Some`) key with a bare `Value` — a dead entry is never
/// equal to anything, so lookups can pass the plain query straight through
/// to the `Option`-keyed maps. (The blanket Borrow-based `Equivalent` does
/// not apply: `Option<Value>` does not `Borrow<Value>`.)
impl rubymap::Equivalent<Option<Value>, Executor, Globals, MonorubyErr> for Value {
    fn equivalent(
        &self,
        key: &Option<Value>,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<bool> {
        match key {
            Some(k) => self.eql(k, e, g),
            None => Ok(false),
        }
    }
}

impl rubymap::Equivalent<Option<IdentKey>, Executor, Globals, MonorubyErr> for IdentKey {
    fn equivalent(
        &self,
        key: &Option<IdentKey>,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<bool> {
        match key {
            Some(k) => self.eql(k, e, g),
            None => Ok(false),
        }
    }
}

pub enum Iter<'a> {
    Inline(std::slice::Iter<'a, (Value, Value)>),
    Map(rubymap::map::Iter<'a, Option<Value>, Value>),
    IdentMap(rubymap::map::Iter<'a, Option<IdentKey>, Value>),
}

impl Iterator for Iter<'_> {
    type Item = (Value, Value);
    fn next(&mut self) -> Option<Self::Item> {
        // Tombstoned entries are dead (`None` keys): skip them so no caller
        // — user-facing iteration, GC marking, `keys`/`values`, `inspect` —
        // can observe them. The `Option` key type makes forgetting this
        // filter a compile error rather than a leaked sentinel.
        loop {
            match self {
                Iter::Inline(pairs) => return pairs.next().copied(),
                Iter::Map(map) => match map.next() {
                    Some((Some(k), v)) => return Some((*k, *v)),
                    Some((None, _)) => continue,
                    None => return None,
                },
                Iter::IdentMap(map) => match map.next() {
                    Some((Some(k), v)) => return Some((k.0, *v)),
                    Some((None, _)) => continue,
                    None => return None,
                },
            }
        }
    }
}

///
/// The Hash object handle: a `Value` known to be `ObjTy::HASH`.
///
/// Mutating methods run the generational write barrier; read methods
/// forward to [`HashRef`].
///
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Hashmap(Value);

impl std::convert::From<Hashmap> for Value {
    fn from(h: Hashmap) -> Value {
        h.0
    }
}

impl alloc::GC<RValue> for Hashmap {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.0.mark(alloc)
    }
}

impl Hashmap {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::HASH));
        Self(val)
    }

    pub fn new_unchecked(val: Value) -> Self {
        Self(val)
    }

    pub fn as_ptr(self) -> *mut RValue {
        self.0.id() as _
    }

    pub fn as_val(self) -> Value {
        self.0
    }

    /// Borrow the content.
    pub(crate) fn inner(&self) -> HashRef<'_> {
        self.0.as_hashmap_inner()
    }

    pub fn index(&self, vm: &mut Executor, globals: &mut Globals, key: Value) -> Result<Value> {
        if let Some(v) = self.get(key, vm, globals)? {
            Ok(v)
        } else if let Some(proc) = self.inner().defalut_proc() {
            vm.invoke_proc(globals, &proc, &[self.0, key])
        } else {
            Ok(self.inner().defalut_value().unwrap_or_default())
        }
    }

    pub fn get(&self, k: Value, vm: &mut Executor, globals: &mut Globals) -> Result<Option<Value>> {
        self.inner().get(k, vm, globals)
    }

    pub fn contains_key(&self, k: Value, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        self.inner().contains_key(k, vm, globals)
    }

    pub fn len(&self) -> usize {
        self.inner().len()
    }

    /// See [`HashRef::entry_at`].
    pub(crate) fn entry_at(&self, index: usize) -> Option<(Value, Value)> {
        self.inner().entry_at(index)
    }

    /// See [`HashRef::entry_count`].
    pub(crate) fn entry_count(&self) -> usize {
        self.inner().entry_count()
    }

    /// See [`HashRef::live_at`].
    pub(crate) fn live_at(&self, index: usize) -> bool {
        self.inner().live_at(index)
    }

    pub fn is_empty(&self) -> bool {
        self.inner().is_empty()
    }

    pub fn iter(&self) -> Iter<'_> {
        self.inner().iter()
    }

    pub fn keys(&self) -> Vec<Value> {
        self.inner().keys()
    }

    pub fn values(&self) -> Vec<Value> {
        self.inner().values()
    }

    pub fn is_compare_by_identity(&self) -> bool {
        self.inner().is_compare_by_identity()
    }

    pub fn defalut_value(&self) -> Option<Value> {
        self.inner().defalut_value()
    }

    pub fn defalut_proc(&self) -> Option<Proc> {
        self.inner().defalut_proc()
    }

    pub fn default_value(&self) -> Option<Value> {
        self.inner().default_value()
    }

    pub fn check_iter(&self) -> Result<()> {
        self.inner().check_iter()
    }

    pub fn iter_guard(&self) -> IterGuard<'_> {
        self.inner().iter_guard()
    }

    /// See [`HashRef::iter_incr`].
    pub fn iter_incr(&self) -> bool {
        self.inner().iter_incr()
    }

    /// See [`HashRef::iter_decr`].
    pub fn iter_decr(&self, real: bool) {
        self.inner().iter_decr(real)
    }

    pub fn debug(&self, store: &Store) -> String {
        self.inner().debug(store)
    }

    pub fn to_s(&self, store: &Store, self_id: u64) -> String {
        self.inner().to_s(store, self_id)
    }

    pub fn inspect_inner(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        self.inner().inspect_inner(store, set)
    }

    //#[cfg(test)]
    //pub(crate) fn ruby2_keywords_flag(&self) -> bool {
    //    self.inner().ruby2_keywords_flag()
    //}

    fn id(&self) -> HashId {
        self.inner().id()
    }

    // Write-barrier-protected stores.

    pub fn insert(
        &mut self,
        k: Value,
        v: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        self.0.as_hashmap_inner_mut().insert(k, v, vm, globals)?;
        self.0.write_barrier_bulk();
        Ok(())
    }

    /// Positional value overwrite with the generational write barrier.
    /// A no-op for out-of-range or tombstoned `index`.
    pub(crate) fn set_value_at(&mut self, index: usize, v: Value) {
        self.0.as_hashmap_inner_mut().set_value_at(index, v);
        self.0.write_barrier(v);
    }

    pub fn remove(
        &mut self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Value>> {
        self.0.as_hashmap_inner_mut().remove(k, vm, globals)
    }

    pub fn clear(&mut self) -> Result<()> {
        self.0.as_hashmap_inner_mut().clear()
    }

    pub fn shift(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<(Value, Value)>> {
        self.0.as_hashmap_inner_mut().shift(vm, globals)
    }

    pub fn compare_by_identity(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<()> {
        self.0
            .as_hashmap_inner_mut()
            .compare_by_identity(vm, globals)?;
        self.0.write_barrier_bulk();
        Ok(())
    }

    pub fn set_compare_by_identity_empty(&mut self, ident: bool) -> Result<()> {
        self.0
            .as_hashmap_inner_mut()
            .set_compare_by_identity_empty(ident)
    }

    pub fn set_defalut_value(
        &mut self,
        default: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        self.0
            .as_hashmap_inner_mut()
            .set_defalut_value(default, vm, globals)?;
        self.0.write_barrier(default);
        Ok(())
    }

    pub fn set_defalut_proc(
        &mut self,
        default_proc: Proc,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        self.0
            .as_hashmap_inner_mut()
            .set_defalut_proc(default_proc, vm, globals)?;
        self.0.write_barrier_bulk();
        Ok(())
    }

    /// Replace the whole content (`Hash#replace`). The source's
    /// representation, identity-mode, and r2k bits replace the
    /// receiver's (`Hash#replace` transfers the compare_by_identity
    /// flag in both directions). Bulk barrier: the new content may
    /// reference young objects.
    pub fn replace_inner(&mut self, inner: HashmapInner) {
        let (flags, body) = inner.into_parts();
        let mut m = self.0.as_hashmap_inner_mut();
        m.install(flags & REP_MASK, body);
        let f = m.flags();
        m.set_flags((f & !(IDENT_BIT | R2K_BIT)) | (flags & (IDENT_BIT | R2K_BIT)));
        self.0.write_barrier_bulk();
    }

    /// Set / clear the ruby2_keywords flag (a plain bit — no GC edge,
    /// so no write barrier is needed).
    pub(crate) fn set_ruby2_keywords_flag(&mut self) {
        self.0.as_hashmap_inner_mut().set_ruby2_keywords_flag();
    }

    pub(crate) fn unset_ruby2_keywords_flag(&mut self) {
        self.0.as_hashmap_inner_mut().unset_ruby2_keywords_flag();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rep_of(h: &HashmapInner) -> u8 {
        h.flags & REP_MASK
    }

    /// The query-side `Equivalent` impls over the `Option`-keyed boxed
    /// maps: a bare query equals a live (`Some`) key iff it eqls the inner
    /// key, and never equals a dead (`None`) one. The `None` arms are
    /// unreachable through lookups in practice — the index table drops
    /// dead entries — so they are pinned here directly.
    #[test]
    fn option_key_equivalence() {
        use rubymap::Equivalent;
        let mut globals = Globals::new_test();
        let mut executor = Executor::default();
        let (e, g) = (&mut executor, &mut globals);
        let five = Value::integer(5);
        assert!(five.equivalent(&Some(Value::integer(5)), e, g).unwrap());
        assert!(!five.equivalent(&Some(Value::integer(7)), e, g).unwrap());
        assert!(!five.equivalent(&None, e, g).unwrap());
        let ik = IdentKey(five);
        assert!(ik.equivalent(&Some(IdentKey(five)), e, g).unwrap());
        assert!(!ik
            .equivalent(&Some(IdentKey(Value::integer(7))), e, g)
            .unwrap());
        assert!(!ik.equivalent(&None, e, g).unwrap());
    }

    #[test]
    fn hash0() {
        let mut globals = Globals::new_test();
        let mut executor = Executor::default();
        let mut map = HashmapInner::default();
        map.insert(
            Value::integer(5),
            Value::float(12.0),
            &mut executor,
            &mut globals,
        )
        .unwrap();
        map.insert(
            Value::integer(5),
            Value::float(5.7),
            &mut executor,
            &mut globals,
        )
        .unwrap();
        map.insert(
            Value::integer(7),
            Value::float(42.5),
            &mut executor,
            &mut globals,
        )
        .unwrap();
        assert_eq!(
            Some(Value::float(5.7)),
            map.get(Value::integer(5), &mut executor, &mut globals)
                .unwrap()
        );
        assert_eq!(vec![Value::integer(5), Value::integer(7)], map.keys());
        assert_eq!(vec![Value::float(5.7), Value::float(42.5)], map.values());
        assert_eq!(2, map.len())
    }

    /// Exercise the inline representation end to end: stay inline for
    /// three packed-key pairs, promote on the 4th pair and on a heap
    /// key, and keep every operation's result identical across the
    /// transition.
    #[test]
    fn hash_inline() {
        let mut globals = Globals::new_test();
        let e = &mut Executor::default();
        let g = &mut globals;

        let mut h = HashmapInner::default();
        assert!(h.is_empty());
        assert_eq!(0, rep_of(&h));

        // fill the three inline slots
        h.insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        h.insert(Value::integer(2), Value::integer(20), e, g)
            .unwrap();
        h.insert(Value::integer(3), Value::integer(30), e, g)
            .unwrap();
        assert_eq!(3, rep_of(&h));
        assert_eq!(3, h.len());
        // in-place update does not promote
        h.insert(Value::integer(2), Value::integer(21), e, g)
            .unwrap();
        assert_eq!(3, rep_of(&h));
        assert_eq!(
            Some(Value::integer(21)),
            h.get(Value::integer(2), e, g).unwrap()
        );
        assert_eq!(None, h.get(Value::integer(9), e, g).unwrap());
        // a heap probe misses without promoting
        assert_eq!(None, h.get(Value::string_from_str("k"), e, g).unwrap());
        assert!(h.contains_key(Value::integer(1), e, g).unwrap());
        assert!(!h.contains_key(Value::string_from_str("k"), e, g).unwrap());

        // remove keeps order and reopens an inline slot
        assert_eq!(
            Some(Value::integer(10)),
            h.remove(Value::integer(1), e, g).unwrap()
        );
        assert_eq!(2, h.len());
        assert_eq!(None, h.remove(Value::integer(1), e, g).unwrap());
        h.insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        assert_eq!(
            vec![Value::integer(2), Value::integer(3), Value::integer(1)],
            h.keys()
        );

        // 4th pair promotes to the boxed map with entries preserved
        h.insert(Value::integer(4), Value::integer(40), e, g)
            .unwrap();
        assert_eq!(REP_BOXED, rep_of(&h));
        assert_eq!(4, h.len());
        assert_eq!(
            vec![
                Value::integer(2),
                Value::integer(3),
                Value::integer(1),
                Value::integer(4)
            ],
            h.keys()
        );
        assert_eq!(
            Some(Value::integer(21)),
            h.get(Value::integer(2), e, g).unwrap()
        );

        // a heap (String) key promotes even when a slot is free
        let mut h2 = HashmapInner::default();
        h2.insert(Value::string_from_str("a"), Value::integer(1), e, g)
            .unwrap();
        assert_eq!(REP_BOXED, rep_of(&h2));
        assert_eq!(
            Some(Value::integer(1)),
            h2.get(Value::string_from_str("a"), e, g).unwrap()
        );

        // inline and boxed hashes with the same entries are eql and hash
        // equal
        let mut inline = HashmapInner::default();
        inline
            .insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        let mut boxed = HashmapInner::default();
        boxed
            .insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        for i in 8..11 {
            boxed
                .insert(Value::integer(i), Value::integer(i), e, g)
                .unwrap();
        }
        assert_eq!(REP_BOXED, rep_of(&boxed));
        for i in 8..11 {
            boxed.remove(Value::integer(i), e, g).unwrap();
        }
        assert!(inline.eql(&boxed, e, g).unwrap());
        assert!(boxed.eql(&inline, e, g).unwrap());
        let digest = |m: &HashmapInner, e: &mut Executor, g: &mut Globals| {
            let mut s = crate::value::seeded_hasher();
            m.ruby_hash(&mut s, e, g).unwrap();
            std::hash::Hasher::finish(&s)
        };
        assert_eq!(digest(&inline, e, g), digest(&boxed, e, g));

        // shift walks the inline pairs in insertion order
        let mut s = HashmapInner::default();
        s.insert(Value::symbol_from_str("x"), Value::integer(1), e, g)
            .unwrap();
        s.insert(Value::symbol_from_str("y"), Value::integer(2), e, g)
            .unwrap();
        assert_eq!(
            Some((Value::symbol_from_str("x"), Value::integer(1))),
            s.shift(e, g).unwrap()
        );
        assert_eq!(
            Some((Value::symbol_from_str("y"), Value::integer(2))),
            s.shift(e, g).unwrap()
        );
        assert_eq!(None, s.shift(e, g).unwrap());

        // clear returns a boxed, default-less hash to the inline form
        let mut c = HashmapInner::default();
        for i in 0..5 {
            c.insert(Value::integer(i), Value::integer(i), e, g)
                .unwrap();
        }
        assert_eq!(REP_BOXED, rep_of(&c));
        c.clear().unwrap();
        assert_eq!(0, rep_of(&c));
        assert!(c.is_empty());
        c.insert(Value::integer(1), Value::integer(1), e, g)
            .unwrap();
        assert_eq!(1, rep_of(&c));

        // compare_by_identity promotes an inline hash to an ident map
        let mut ident = HashmapInner::default();
        ident
            .insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        ident.compare_by_identity(e, g).unwrap();
        assert!(ident.is_compare_by_identity());
        assert_eq!(
            Some(Value::integer(10)),
            ident.get(Value::integer(1), e, g).unwrap()
        );
        // ... and clear keeps the identity mode
        ident.clear().unwrap();
        assert!(ident.is_compare_by_identity());
        // switching an empty hash's mode goes both ways
        ident.set_compare_by_identity_empty(false).unwrap();
        assert!(!ident.is_compare_by_identity());
        assert_eq!(0, rep_of(&ident));
        ident.set_compare_by_identity_empty(true).unwrap();
        assert!(ident.is_compare_by_identity());

        // clone of an inline hash stays inline and is independent
        let mut orig = HashmapInner::default();
        orig.insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        let mut copy = orig.clone();
        assert_eq!(1, rep_of(&copy));
        copy.insert(Value::integer(2), Value::integer(20), e, g)
            .unwrap();
        assert_eq!(1, orig.len());
        assert_eq!(2, copy.len());

        // HashmapInner::new converts a small packed-key map to inline
        let mut small = RubyMap::default();
        small
            .insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        let conv = HashmapInner::new(small);
        assert_eq!(1, rep_of(&conv));
        assert_eq!(1, conv.len());
        let mut with_heap_key = RubyMap::default();
        with_heap_key
            .insert(Value::string_from_str("a"), Value::integer(1), e, g)
            .unwrap();
        let conv = HashmapInner::new(with_heap_key);
        assert_eq!(REP_BOXED, rep_of(&conv));
        assert_eq!(1, conv.len());
    }

    /// The identity-keyed arms, the default plumbing (a default forces
    /// the boxed form), and the iteration guard across representations.
    #[test]
    fn hash_ident_and_defaults() {
        let mut globals = Globals::new_test();
        let e = &mut Executor::default();
        let g = &mut globals;

        // identity-keyed: full method surface
        let mut h = HashmapInner::default();
        h.insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        h.compare_by_identity(e, g).unwrap();
        h.insert(Value::integer(2), Value::integer(20), e, g)
            .unwrap();
        assert!(h.contains_key(Value::integer(1), e, g).unwrap());
        assert!(!h.contains_key(Value::integer(3), e, g).unwrap());
        assert_eq!(2, h.len());
        assert_eq!(vec![Value::integer(1), Value::integer(2)], h.keys());
        assert_eq!(vec![Value::integer(10), Value::integer(20)], h.values());
        assert_eq!(
            Some((Value::integer(1), Value::integer(10))),
            h.shift(e, g).unwrap()
        );
        assert_eq!(
            Some(Value::integer(20)),
            h.remove(Value::integer(2), e, g).unwrap()
        );
        assert_eq!(None, h.remove(Value::integer(2), e, g).unwrap());

        // ident-vs-ident eql, ident-vs-eql-keyed is always false, and
        // the ident ruby_hash arm digests by id
        let mut i1 = HashmapInner::default();
        i1.compare_by_identity(e, g).unwrap();
        i1.insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        let i2 = i1.clone();
        assert!(i2.is_compare_by_identity());
        assert!(i1.eql(&i2, e, g).unwrap());
        let mut plain = HashmapInner::default();
        plain
            .insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        assert!(!i1.eql(&plain, e, g).unwrap());
        assert!(!plain.eql(&i1, e, g).unwrap());
        let mut s = crate::value::seeded_hasher();
        i1.ruby_hash(&mut s, e, g).unwrap();

        // eql: length mismatch and value mismatch
        let mut short = HashmapInner::default();
        short
            .insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        let mut long = short.clone();
        long.insert(Value::integer(2), Value::integer(20), e, g)
            .unwrap();
        assert!(!short.eql(&long, e, g).unwrap());
        let mut diff = HashmapInner::default();
        diff.insert(Value::integer(1), Value::integer(99), e, g)
            .unwrap();
        assert!(!short.eql(&diff, e, g).unwrap());

        // an emptied boxed map can switch to identity mode
        let mut m = RubyMap::default();
        m.insert(Value::string_from_str("k"), Value::integer(1), e, g)
            .unwrap();
        let mut boxed = HashmapInner::new(m);
        boxed.remove(Value::string_from_str("k"), e, g).unwrap();
        boxed.set_compare_by_identity_empty(true).unwrap();
        assert!(boxed.is_compare_by_identity());
        // Map-arm shift
        let mut m2 = RubyMap::default();
        m2.insert(Value::string_from_str("k"), Value::integer(1), e, g)
            .unwrap();
        let mut boxed2 = HashmapInner::new(m2);
        let (sk, sv) = boxed2.shift(e, g).unwrap().unwrap();
        assert_eq!(Value::integer(1), sv);
        assert!(sk.eql(&Value::string_from_str("k"), e, g).unwrap());

        // defaults: nil normalizes to no box; a real default forces the
        // boxed representation and round-trips
        let mut d = HashmapInner::default();
        assert_eq!(Some(Value::nil()), d.defalut_value());
        assert_eq!(None, d.as_ref().default_value());
        d.insert(Value::integer(1), Value::integer(2), e, g)
            .unwrap();
        assert_eq!(1, rep_of(&d));
        d.as_mut().set_defalut_value(Value::nil(), e, g).unwrap();
        assert_eq!(1, rep_of(&d)); // nil default keeps it inline
        d.as_mut()
            .set_defalut_value(Value::integer(42), e, g)
            .unwrap();
        assert_eq!(REP_BOXED, rep_of(&d)); // a real default boxes
        assert_eq!(Some(Value::integer(42)), d.defalut_value());
        assert_eq!(Some(Value::integer(42)), d.as_ref().default_value());
        assert_eq!(
            Some(Value::integer(2)),
            d.get(Value::integer(1), e, g).unwrap()
        );
        d.as_mut().set_defalut_value(Value::nil(), e, g).unwrap();
        assert_eq!(Some(Value::nil()), d.defalut_value());
        assert!(d.as_ref().defalut_proc().is_none());
        let dm = HashmapInner::new_with_default(RubyMap::default(), Value::integer(7));
        assert_eq!(REP_BOXED, rep_of(&dm));
        assert_eq!(Some(Value::integer(7)), dm.defalut_value());

        // r2k flag travels in the flags byte and is dropped by clone
        let mut r = HashmapInner::default();
        r.set_ruby2_keywords_flag();
        assert!(r.as_ref().ruby2_keywords_flag());
        assert!(!r.clone().as_ref().ruby2_keywords_flag());
        r.as_mut().unset_ruby2_keywords_flag();
        assert!(!r.as_ref().ruby2_keywords_flag());

        // iteration guard: inline bits count up and down, and inserting
        // a new key mid-iteration fails while updating an existing one
        // succeeds
        let mut it = HashmapInner::default();
        it.insert(Value::integer(1), Value::integer(1), e, g)
            .unwrap();
        {
            let r = it.as_ref();
            let _g1 = r.iter_guard();
            let _g2 = r.iter_guard();
            assert!(r.check_iter().is_err());
        }
        assert!(it.as_ref().check_iter().is_ok());
        {
            let r = it.as_ref();
            let _g1 = r.iter_guard();
            // 3 real guards + 1 saturated no-op guard
            let _g2 = r.iter_guard();
            let _g3 = r.iter_guard();
            let _g4 = r.iter_guard();
        }
        assert!(it.as_ref().check_iter().is_ok());
        // (promotion while a guard is live — `h.each { h.default = x }` —
        // needs two aliases of the same cell and is covered by the
        // Ruby-level test `hash_default_set_during_iteration`.)
        it.as_mut()
            .set_defalut_value(Value::integer(5), e, g)
            .unwrap();
        assert_eq!(REP_BOXED, rep_of(&it));
        assert_eq!(
            Some(Value::integer(1)),
            it.get(Value::integer(1), e, g).unwrap()
        );
        assert!(it.as_ref().check_iter().is_ok());

        // Debug walks the pairs
        let mut dbg = HashmapInner::default();
        dbg.insert(Value::integer(1), Value::integer(2), e, g)
            .unwrap();
        assert!(!format!("{dbg:?}").is_empty());
    }

    /// Identity-keyed hashes use the inline representation too: the mode
    /// bit lives in the flags byte, id probing accepts heap keys, and
    /// the boxed transitions preserve the mode in both directions.
    #[test]
    fn hash_ident_inline() {
        let mut globals = Globals::new_test();
        let e = &mut Executor::default();
        let g = &mut globals;

        // compare_by_identity on a populated inline hash stays inline
        let mut h = HashmapInner::default();
        h.insert(Value::integer(1), Value::integer(10), e, g)
            .unwrap();
        h.compare_by_identity(e, g).unwrap();
        assert!(h.is_compare_by_identity());
        assert!(rep_of(&h) != REP_BOXED);

        // heap keys go inline under identity mode; same-content distinct
        // strings are distinct keys, and re-probing with the same object
        // hits
        let s1 = Value::string_from_str("key");
        let s2 = Value::string_from_str("key");
        h.insert(s1, Value::integer(100), e, g).unwrap();
        assert!(rep_of(&h) != REP_BOXED);
        assert_eq!(2, h.len());
        assert_eq!(Some(Value::integer(100)), h.get(s1, e, g).unwrap());
        assert_eq!(None, h.get(s2, e, g).unwrap());
        assert!(h.contains_key(s1, e, g).unwrap());
        assert!(!h.contains_key(s2, e, g).unwrap());
        assert_eq!(Some(Value::integer(100)), h.remove(s1, e, g).unwrap());
        h.insert(s1, Value::integer(100), e, g).unwrap();

        // the 4th pair promotes to the boxed IdentMap with identical
        // behavior
        h.insert(s2, Value::integer(200), e, g).unwrap();
        h.insert(Value::integer(9), Value::integer(90), e, g)
            .unwrap();
        assert_eq!(REP_BOXED, rep_of(&h));
        assert!(h.is_compare_by_identity());
        assert_eq!(4, h.len());
        assert_eq!(Some(Value::integer(100)), h.get(s1, e, g).unwrap());
        assert_eq!(Some(Value::integer(200)), h.get(s2, e, g).unwrap());

        // clear demotes a default-less boxed ident hash back to inline,
        // keeping the mode
        h.clear().unwrap();
        assert!(rep_of(&h) != REP_BOXED);
        assert!(h.is_compare_by_identity());
        h.insert(s1, Value::integer(1), e, g).unwrap();
        assert!(rep_of(&h) != REP_BOXED);

        // inline-ident and boxed-ident with the same entries are eql and
        // digest alike; ident never equals eql?-keyed
        let mut inline_i = HashmapInner::default();
        inline_i.compare_by_identity(e, g).unwrap();
        inline_i.insert(s1, Value::integer(1), e, g).unwrap();
        let mut boxed_i = HashmapInner::default();
        boxed_i.compare_by_identity(e, g).unwrap();
        boxed_i.insert(s1, Value::integer(1), e, g).unwrap();
        for i in 0..3 {
            boxed_i
                .insert(Value::integer(i), Value::integer(i), e, g)
                .unwrap();
        }
        assert_eq!(REP_BOXED, rep_of(&boxed_i));
        for i in 0..3 {
            boxed_i.remove(Value::integer(i), e, g).unwrap();
        }
        assert!(inline_i.eql(&boxed_i, e, g).unwrap());
        assert!(boxed_i.eql(&inline_i, e, g).unwrap());
        let digest = |m: &HashmapInner, e: &mut Executor, g: &mut Globals| {
            let mut s = crate::value::seeded_hasher();
            m.ruby_hash(&mut s, e, g).unwrap();
            std::hash::Hasher::finish(&s)
        };
        assert_eq!(digest(&inline_i, e, g), digest(&boxed_i, e, g));

        // clone keeps the identity mode (and the inline representation)
        let copy = inline_i.clone();
        assert!(copy.is_compare_by_identity());
        assert!(rep_of(&copy) != REP_BOXED);
        assert_eq!(Some(Value::integer(1)), copy.get(s1, e, g).unwrap());

        // a default still forces boxing, into an IdentMap
        let mut di = HashmapInner::default();
        di.compare_by_identity(e, g).unwrap();
        di.insert(s1, Value::integer(1), e, g).unwrap();
        di.as_mut()
            .set_defalut_value(Value::integer(7), e, g)
            .unwrap();
        assert_eq!(REP_BOXED, rep_of(&di));
        assert!(di.is_compare_by_identity());
        assert_eq!(Some(Value::integer(1)), di.get(s1, e, g).unwrap());
        assert_eq!(Some(Value::integer(7)), di.defalut_value());

        // shift preserves insertion order across the mode
        let mut sh = HashmapInner::default();
        sh.compare_by_identity(e, g).unwrap();
        sh.insert(s1, Value::integer(1), e, g).unwrap();
        sh.insert(s2, Value::integer(2), e, g).unwrap();
        let (k, v) = sh.shift(e, g).unwrap().unwrap();
        assert_eq!((s1.id(), Value::integer(1)), (k.id(), v));
        let (k, v) = sh.shift(e, g).unwrap().unwrap();
        assert_eq!((s2.id(), Value::integer(2)), (k.id(), v));
    }

    ///
    /// Read entry `i` of `v` exactly the way JIT-generated code does:
    /// through the layout constants alone, never through the Rust API.
    ///
    unsafe fn raw_entry_at(v: Value, i: usize) -> Option<(Value, Value)> {
        unsafe {
            let p = v.rvalue() as *const RValue as *const u8;
            let rep = p.add(RVALUE_OFFSET_TY + 1).read() & HASH_REP_MASK;
            if rep != HASH_REP_BOXED {
                // Inline: the representation bits double as the length.
                if i >= rep as usize {
                    return None;
                }
                let pair = p.add(HASH_INLINE_PAIRS_OFFSET + i * HASH_INLINE_PAIR_STRIDE);
                return Some((
                    pair.add(HASH_INLINE_KEY_OFFSET).cast::<Value>().read(),
                    pair.add(HASH_INLINE_VALUE_OFFSET).cast::<Value>().read(),
                ));
            }
            let lay = hash_entries_layout().unwrap();
            let map = p.add(HASH_CONTENT_MAP_OFFSET).cast::<*const u8>().read();
            if i >= map.add(lay.len_offset).cast::<usize>().read() {
                return None;
            }
            let entry = map
                .add(lay.ptr_offset)
                .cast::<*const u8>()
                .read()
                .add(i * lay.bucket_size);
            Some((
                entry.add(lay.key_offset).cast::<Value>().read(),
                entry.add(lay.value_offset).cast::<Value>().read(),
            ))
        }
    }

    /// The offsets the machine-code intrinsics bake in must agree with
    /// `entry_at` for both representations, across the inline→boxed
    /// promotion. Every offset involved is chosen by the compiler — the
    /// tuple's field order, the bucket's field order under `Value`'s
    /// niche, the `Vec` word order — so a layout change has to fail here
    /// rather than turn into wrong loads inside generated code.
    #[test]
    fn jit_layout_matches_entry_at() {
        let mut globals = Globals::new_test();
        let e = &mut Executor::default();
        let g = &mut globals;
        assert!(
            hash_entries_layout().is_some(),
            "the boxed forms must share one entry layout for the JIT to skip the tag check"
        );
        for n in [0usize, 1, 2, 3, 4, 10, 64] {
            let mut inner = HashmapInner::default();
            for i in 0..n {
                inner
                    .insert(Value::integer(i as i64), Value::integer(i as i64 * 7 + 1), e, g)
                    .unwrap();
            }
            let v = Value::hash_from_inner(inner);
            let h = v.as_hash();
            assert_eq!(h.len(), n, "n={n}");
            // Past the end too: the intrinsics answer nil rather than trap.
            for i in 0..n + 2 {
                assert_eq!(unsafe { raw_entry_at(v, i) }, h.entry_at(i), "n={n} i={i}");
            }
        }
    }

    /// The identity-keyed forms are keyed by `IdentKey`, so this pins the
    /// assumption that they share the `Value`-keyed layout.
    #[test]
    fn jit_layout_matches_entry_at_compare_by_identity() {
        let mut globals = Globals::new_test();
        let e = &mut Executor::default();
        let g = &mut globals;
        for n in [1usize, 3, 4, 10] {
            let mut inner = HashmapInner::default();
            inner.set_compare_by_identity_empty(true).unwrap();
            for i in 0..n {
                inner
                    .insert(Value::integer(i as i64), Value::integer(i as i64 + 100), e, g)
                    .unwrap();
            }
            let v = Value::hash_from_inner(inner);
            let h = v.as_hash();
            assert_eq!(h.len(), n, "n={n}");
            for i in 0..n + 2 {
                assert_eq!(unsafe { raw_entry_at(v, i) }, h.entry_at(i), "n={n} i={i}");
            }
        }
    }

    /// Read `compare_by_identity?` the way generated code does.
    unsafe fn raw_compare_by_identity(v: Value) -> bool {
        unsafe {
            let p = v.rvalue() as *const RValue as *const u8;
            let flags = p.add(RVALUE_OFFSET_TY + 1).read();
            if flags & HASH_REP_MASK != HASH_REP_BOXED {
                flags & HASH_INLINE_IDENT_BIT != 0
            } else {
                p.add(HASH_CONTENT_OFFSET).cast::<usize>().read() == HASH_CONTENT_TAG_IDENT
            }
        }
    }

    /// Read `default` (`want_proc == false`) or `default_proc` the way
    /// generated code does. An inline hash never carries a default, and a
    /// mismatched discriminant answers nil — exactly what the builtins do
    /// via `unwrap_or_default`.
    unsafe fn raw_default(v: Value, want_proc: bool) -> Value {
        unsafe {
            let p = v.rvalue() as *const RValue as *const u8;
            if p.add(RVALUE_OFFSET_TY + 1).read() & HASH_REP_MASK != HASH_REP_BOXED {
                return Value::nil();
            }
            let d = p.add(HASH_DEFAULT_OFFSET).cast::<*const u8>().read();
            if d.is_null() {
                return Value::nil();
            }
            let want = if want_proc {
                HASH_DEFAULT_TAG_PROC
            } else {
                HASH_DEFAULT_TAG_VALUE
            };
            if d.cast::<usize>().read() != want {
                return Value::nil();
            }
            d.add(HASH_DEFAULT_PAYLOAD_OFFSET).cast::<Value>().read()
        }
    }

    /// The identity flag has two homes — a `ty_flags` bit while inline, the
    /// `HashContent` discriminant once boxed — so check both sides of the
    /// promotion against the safe API.
    #[test]
    fn jit_layout_matches_compare_by_identity() {
        let mut globals = Globals::new_test();
        let e = &mut Executor::default();
        let g = &mut globals;
        for ident in [false, true] {
            for n in [0usize, 1, 3, 4, 10] {
                let mut inner = HashmapInner::default();
                inner.set_compare_by_identity_empty(ident).unwrap();
                for i in 0..n {
                    inner
                        .insert(Value::integer(i as i64), Value::integer(i as i64), e, g)
                        .unwrap();
                }
                let v = Value::hash_from_inner(inner);
                assert_eq!(
                    unsafe { raw_compare_by_identity(v) },
                    v.as_hash().is_compare_by_identity(),
                    "ident={ident} n={n}"
                );
            }
        }
    }

    /// `Hash#default` / `#default_proc` read the same `Option<Box<..>>` slot
    /// and differ only in which discriminant they accept, so both are pinned
    /// here for the no-default and value-default cases. The default-proc case
    /// needs a real `Proc` and is covered from Ruby
    /// (`builtins::hash::tests::hash_default_accessors_jit`).
    #[test]
    fn jit_layout_matches_default() {
        let mut globals = Globals::new_test();
        let g = &mut globals;
        let _ = &g;

        // No default: nil from both, in either representation.
        for n in [0usize, 4] {
            let mut inner = HashmapInner::default();
            let e = &mut Executor::default();
            for i in 0..n {
                inner
                    .insert(Value::integer(i as i64), Value::integer(i as i64), e, g)
                    .unwrap();
            }
            let v = Value::hash_from_inner(inner);
            assert!(unsafe { raw_default(v, false) }.is_nil(), "n={n}");
            assert!(unsafe { raw_default(v, true) }.is_nil(), "n={n}");
        }

        // Value default: `default` returns it, `default_proc` stays nil.
        let d = Value::integer(42);
        let inner = HashmapInner::new_with_default(RubyMap::new(), d);
        let v = Value::hash_from_inner(inner);
        assert_eq!(unsafe { raw_default(v, false) }, d);
        assert_eq!(
            unsafe { raw_default(v, false) },
            v.as_hash().default_value().unwrap()
        );
        assert!(unsafe { raw_default(v, true) }.is_nil());
    }
}
