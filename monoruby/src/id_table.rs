use crate::ast::CmpKind;
use crate::bytecodegen::{BinOpK, UnOpK};

use super::*;
use std::num::NonZeroU32;
use std::sync::{LazyLock, RwLock};

static ID: LazyLock<RwLock<IdentifierTable>> =
    LazyLock::new(|| RwLock::new(IdentifierTable::new()));

///
/// Identifier name: either a valid UTF-8 string or raw bytes (binary/ASCII-8BIT).
///
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IdentName {
    Utf8(String),
    Bytes(Vec<u8>),
}

impl IdentName {
    /// Returns the name as &str if UTF-8, or a lossy representation for binary.
    pub fn to_string_lossy(&self) -> String {
        match self {
            IdentName::Utf8(s) => s.clone(),
            IdentName::Bytes(b) => String::from_utf8_lossy(b).into_owned(),
        }
    }

    /// Returns the underlying bytes.
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            IdentName::Utf8(s) => s.as_bytes(),
            IdentName::Bytes(b) => b.as_slice(),
        }
    }

    /// Returns &str if UTF-8, None if binary.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            IdentName::Utf8(s) => Some(s.as_str()),
            IdentName::Bytes(_) => None,
        }
    }

    /// Returns true if this is a valid UTF-8 name.
    pub fn is_utf8(&self) -> bool {
        matches!(self, IdentName::Utf8(_))
    }

    /// Returns true if the name consists of only ASCII characters.
    pub fn is_ascii(&self) -> bool {
        self.as_bytes().is_ascii()
    }
}

impl std::fmt::Debug for IdentName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentName::Utf8(s) => write!(f, "{}", s),
            IdentName::Bytes(b) => write!(f, "{}", String::from_utf8_lossy(b)),
        }
    }
}

impl std::fmt::Display for IdentName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentName::Utf8(s) => write!(f, "{}", s),
            IdentName::Bytes(b) => write!(f, "{}", String::from_utf8_lossy(b)),
        }
    }
}

///
/// Wrapper of ID for strings.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IdentId(NonZeroU32);

// `special_gvar_name` indexes SPECIAL_GVARS by id offset; pin the pairing.
const _: () = {
    let mut i = 0;
    while i < IdentId::SPECIAL_GVARS.len() {
        assert!(
            IdentId::SPECIAL_GVARS[i].1.0.get()
                == IdentId::GVAR_CHILD_STATUS.0.get() + i as u32
        );
        i += 1;
    }
};

impl std::default::Default for IdentId {
    fn default() -> Self {
        Self(NonZeroU32::new(1u32).unwrap())
    }
}

impl std::fmt::Debug for IdentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

impl std::fmt::Display for IdentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

impl From<IdentId> for usize {
    #[inline(always)]
    fn from(id: IdentId) -> usize {
        id.0.get() as usize
    }
}

impl From<IdentId> for u32 {
    #[inline(always)]
    fn from(id: IdentId) -> u32 {
        id.0.get()
    }
}

impl From<u32> for IdentId {
    #[inline(always)]
    fn from(id: u32) -> Self {
        let id = NonZeroU32::new(id).unwrap();
        IdentId(id)
    }
}

impl From<CmpKind> for IdentId {
    fn from(kind: CmpKind) -> Self {
        match kind {
            CmpKind::Eq => IdentId::_EQ,
            CmpKind::Ne => IdentId::_NEQ,
            CmpKind::Lt => IdentId::_LT,
            CmpKind::Le => IdentId::_LE,
            CmpKind::Gt => IdentId::_GT,
            CmpKind::Ge => IdentId::_GE,
            CmpKind::TEq => IdentId::_TEQ,
        }
    }
}

impl From<BinOpK> for IdentId {
    fn from(kind: BinOpK) -> Self {
        match kind {
            BinOpK::Add => IdentId::_ADD,
            BinOpK::Sub => IdentId::_SUB,
            BinOpK::Mul => IdentId::_MUL,
            BinOpK::Div => IdentId::_DIV,
            BinOpK::BitOr => IdentId::_BOR,
            BinOpK::BitAnd => IdentId::_BAND,
            BinOpK::BitXor => IdentId::_BXOR,
            BinOpK::Rem => IdentId::_REM,
            BinOpK::Exp => IdentId::_POW,
            BinOpK::Shl => IdentId::_SHL,
            BinOpK::Shr => IdentId::_SHR,
        }
    }
}

impl From<UnOpK> for IdentId {
    fn from(kind: UnOpK) -> Self {
        match kind {
            UnOpK::Neg => IdentId::_UMINUS,
            UnOpK::Pos => IdentId::_UPLUS,
            UnOpK::BitNot => IdentId::_BNOT,
            UnOpK::Not => IdentId::_NOT,
        }
    }
}

macro_rules! id {
    ($constant:expr) => {
        IdentId(std::num::NonZeroU32::new($constant).unwrap())
    };
}

impl IdentId {
    pub const INITIALIZE: IdentId = id!(1);
    pub const OBJECT: IdentId = id!(2);
    pub const NEW: IdentId = id!(3);
    pub const NAME: IdentId = id!(4);
    pub const _ADD: IdentId = id!(5);
    pub const _SUB: IdentId = id!(6);
    pub const _MUL: IdentId = id!(7);
    pub const _DIV: IdentId = id!(8);
    pub const _REM: IdentId = id!(9);
    pub const _POW: IdentId = id!(10);
    pub const _SHL: IdentId = id!(11);
    pub const _SHR: IdentId = id!(12);
    pub const _BOR: IdentId = id!(13);
    pub const _BAND: IdentId = id!(14);
    pub const _BXOR: IdentId = id!(15);
    pub const _BNOT: IdentId = id!(16);
    pub const _UMINUS: IdentId = id!(17);
    pub const _UPLUS: IdentId = id!(18);
    pub const _EQ: IdentId = id!(19);
    pub const _NEQ: IdentId = id!(20);
    pub const _GT: IdentId = id!(21);
    pub const _GE: IdentId = id!(22);
    pub const _LT: IdentId = id!(23);
    pub const _LE: IdentId = id!(24);
    pub const _CMP: IdentId = id!(25);
    pub const _TEQ: IdentId = id!(26);
    pub const _MATCH: IdentId = id!(27);
    pub const _INDEX: IdentId = id!(28);
    pub const _INDEX_ASSIGN: IdentId = id!(29);
    pub const _DOT3: IdentId = id!(30);

    pub const TO_S: IdentId = id!(31);
    pub const TO_PROC: IdentId = id!(32);
    pub const METHOD_MISSING: IdentId = id!(33);
    pub const EACH: IdentId = id!(34);
    pub const MAP: IdentId = id!(35);
    pub const _ALIAS_METHOD: IdentId = id!(36);
    pub const _ENUM_FUNC: IdentId = id!(37);
    pub const _NAME: IdentId = id!(38);
    pub const _MAIN: IdentId = id!(39);

    pub const FLOAT_SECOND: IdentId = id!(40);
    pub const SECOND: IdentId = id!(41);
    pub const FLOAT_MILLISECOND: IdentId = id!(42);
    pub const MILLISECOND: IdentId = id!(43);
    pub const FLOAT_MICROSECOND: IdentId = id!(44);
    pub const MICROSECOND: IdentId = id!(45);
    pub const NANOSECOND: IdentId = id!(46);

    pub const ENCODING: IdentId = id!(47);
    pub const UTF_8: IdentId = id!(48);
    pub const ASCII_8BIT: IdentId = id!(49);
    pub const _ENCODING: IdentId = id!(50);
    pub const TO_ARY: IdentId = id!(51);
    pub const TO_A: IdentId = id!(52);
    pub const ALLOCATE: IdentId = id!(53);
    pub const METHOD_ADDED: IdentId = id!(54);
    pub const HASH: IdentId = id!(55);
    pub const EQL_: IdentId = id!(56);
    pub const TO_STR: IdentId = id!(57);
    pub const TO_PATH: IdentId = id!(58);
    pub const SINGLETON_METHOD_ADDED: IdentId = id!(59);
    pub const METHOD_REMOVED: IdentId = id!(60);
    pub const METHOD_UNDEFINED: IdentId = id!(61);
    pub const CONST_ADDED: IdentId = id!(62);
    pub const INHERITED: IdentId = id!(63);
    pub const APPEND_FEATURES: IdentId = id!(64);
    pub const EXTEND_OBJECT: IdentId = id!(65);
    pub const EXTENDED: IdentId = id!(66);
    pub const INCLUDED: IdentId = id!(67);
    pub const PREPENDED: IdentId = id!(68);
    pub const _UNMATCH: IdentId = id!(69);
    pub const _NOT: IdentId = id!(70);
    pub const INSPECT: IdentId = id!(71);
    pub const SINGLETON_METHOD_REMOVED: IdentId = id!(72);
    pub const SINGLETON_METHOD_UNDEFINED: IdentId = id!(73);
    pub const TO_INT: IdentId = id!(74);
    pub const TO_F: IdentId = id!(75);
    pub const INITIALIZE_COPY: IdentId = id!(76);
    pub const INITIALIZE_CLONE: IdentId = id!(77);
    pub const INITIALIZE_DUP: IdentId = id!(78);
    pub const RESPOND_TO_MISSING_: IdentId = id!(79);
    /// The hidden ivar `Integer#chr` hangs a mock encoding object on
    /// (`/encoding_override`). `String#encoding` probes it on every call,
    /// so it is interned up front rather than by name each time.
    pub const _ENCODING_OVERRIDE: IdentId = id!(96);

    // The special global variables whose assignment `write_special_check`
    // (globals/gvar.rs) validates or coerces. Deliberately a *consecutive*
    // block: every global-variable write asks "is this one special?", and
    // with the block contiguous that is a single range check
    // ([`IdentId::is_special_gvar`]) instead of a name lookup — no String
    // is ever materialized for the ordinary-global fast path.
    pub const GVAR_CHILD_STATUS: IdentId = id!(80); // $?
    pub const GVAR_ARGF: IdentId = id!(81); // $<
    pub const GVAR_FILENAME: IdentId = id!(82); // $FILENAME
    pub const GVAR_IRS: IdentId = id!(83); // $/
    pub const GVAR_IRS_ALIAS: IdentId = id!(84); // $-0
    pub const GVAR_ORS: IdentId = id!(85); // $\
    pub const GVAR_OFS: IdentId = id!(86); // $,
    pub const GVAR_FS: IdentId = id!(87); // $;
    pub const GVAR_LINENO: IdentId = id!(88); // $.
    pub const GVAR_STDOUT: IdentId = id!(89); // $stdout
    pub const GVAR_STDERR: IdentId = id!(90); // $stderr
    pub const GVAR_VERBOSE: IdentId = id!(91); // $VERBOSE
    pub const GVAR_VERBOSE_V: IdentId = id!(92); // $-v
    pub const GVAR_VERBOSE_W: IdentId = id!(93); // $-w
    pub const GVAR_PROGRAM_NAME0: IdentId = id!(94); // $0
    pub const GVAR_PROGRAM_NAME: IdentId = id!(95); // $PROGRAM_NAME

    /// Name ↔ id pairing for the block above — the single source of truth:
    /// `IdentifierTable::new` interns from it, and [`Self::special_gvar_name`]
    /// reads a name back without materializing a String. Order must follow
    /// the ids (checked by the const assertion below).
    pub(crate) const SPECIAL_GVARS: [(&'static str, IdentId); 16] = [
        ("$?", Self::GVAR_CHILD_STATUS),
        ("$<", Self::GVAR_ARGF),
        ("$FILENAME", Self::GVAR_FILENAME),
        ("$/", Self::GVAR_IRS),
        ("$-0", Self::GVAR_IRS_ALIAS),
        ("$\\", Self::GVAR_ORS),
        ("$,", Self::GVAR_OFS),
        ("$;", Self::GVAR_FS),
        ("$.", Self::GVAR_LINENO),
        ("$stdout", Self::GVAR_STDOUT),
        ("$stderr", Self::GVAR_STDERR),
        ("$VERBOSE", Self::GVAR_VERBOSE),
        ("$-v", Self::GVAR_VERBOSE_V),
        ("$-w", Self::GVAR_VERBOSE_W),
        ("$0", Self::GVAR_PROGRAM_NAME0),
        ("$PROGRAM_NAME", Self::GVAR_PROGRAM_NAME),
    ];

    /// Whether this id names one of the special globals above.
    pub(crate) fn is_special_gvar(self) -> bool {
        (Self::GVAR_CHILD_STATUS.0.get()..=Self::GVAR_PROGRAM_NAME.0.get())
            .contains(&self.0.get())
    }

    /// The name of a special global, as a static str — the ids are
    /// consecutive, so this is an array index off the block's base.
    /// Callers must have checked [`Self::is_special_gvar`].
    pub(crate) fn special_gvar_name(self) -> &'static str {
        debug_assert!(self.is_special_gvar());
        Self::SPECIAL_GVARS[(self.0.get() - Self::GVAR_CHILD_STATUS.0.get()) as usize].0
    }
}

impl IdentId {
    ///
    /// Get an inner id as u32.
    ///
    pub(crate) fn get(&self) -> u32 {
        self.0.get()
    }

    ///
    /// Get an inner id as usize.
    ///
    fn to_usize(self) -> usize {
        self.0.get() as usize
    }
}

impl IdentId {
    ///
    /// Get *IdentId* from &str (UTF-8).
    ///
    pub fn get_id(name: &str) -> IdentId {
        // Almost every call re-interns a name that already exists (method
        // names, ivar keys, `$/`), so try the shared read lock first and
        // only escalate to the exclusive one on a genuine miss. Taking
        // the write lock unconditionally showed up as ~27% of `IO#gets`.
        if let Some(id) = ID.read().unwrap().rev_table.get(name) {
            return *id;
        }
        ID.write().unwrap().get_id(name)
    }

    ///
    /// Get *IdentId* from String (UTF-8).
    ///
    pub fn get_id_from_string(name: String) -> IdentId {
        ID.write().unwrap().get_id_from_string(name)
    }

    ///
    /// Get *IdentId* from raw bytes (binary/ASCII-8BIT).
    ///
    /// `enc` discriminates the symbol: same bytes interned under a
    /// different (ASCII-incompatible / non-UTF-8) encoding produce a
    /// distinct `IdentId`, matching CRuby's per-(bytes, encoding)
    /// symbol identity. The encoding is recorded so
    /// `Symbol#{to_s,name,encoding,inspect}` round-trip it.
    pub fn get_id_from_bytes(bytes: Vec<u8>, enc: crate::value::Encoding) -> IdentId {
        ID.write().unwrap().get_id_from_bytes(bytes, enc)
    }

    ///
    /// Get name as String from *self* (lossy for binary names).
    ///
    pub fn get_name(&self) -> String {
        ID.read().unwrap().get_ident_name(*self).to_string_lossy()
    }

    ///
    /// Get the IdentName from *self*.
    ///
    pub fn get_ident_name_clone(&self) -> IdentName {
        ID.read().unwrap().get_ident_name(*self).clone()
    }

    ///
    /// The recorded source encoding for this symbol, if any.
    ///
    pub(crate) fn symbol_encoding(self) -> Option<crate::value::Encoding> {
        ID.read().unwrap().enc_map.get(&self).copied()
    }

    ///
    /// Compare *self* to *other* by raw bytes.
    ///
    pub fn compare(&self, other: &Self) -> std::cmp::Ordering {
        if self == other {
            return std::cmp::Ordering::Equal;
        }
        let id = ID.read().unwrap();
        let lhs = id.get_ident_name(*self);
        let rhs = id.get_ident_name(*other);
        lhs.as_bytes().cmp(rhs.as_bytes())
    }

    ///
    /// Get instance variable name from *id*.
    ///
    /// ex) "var" -> "@var"
    ///
    pub(crate) fn add_ivar_prefix(id: IdentId) -> IdentId {
        ID.write().unwrap().add_ivar_prefix(id)
    }

    ///
    /// Get assign method name from *id*.
    ///
    /// ex) "var" -> "var="
    ///
    pub(crate) fn add_assign_postfix(id: IdentId) -> IdentId {
        ID.write().unwrap().add_assign_postfix(id)
    }

    ///
    /// Get all interned symbol IDs.
    ///
    pub(crate) fn all_symbols() -> Vec<IdentId> {
        let id = ID.read().unwrap();
        (1..=id.table.len() as u32)
            .map(IdentId::from)
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct IdentifierTable {
    /// Reverse lookup for UTF-8 names.
    rev_table: HashMap<String, IdentId>,
    /// Reverse lookup for binary names, keyed by `(bytes, encoding)`
    /// so the same bytes under different (ASCII-incompatible /
    /// non-UTF-8) encodings are distinct symbols.
    rev_table_bytes: HashMap<(Vec<u8>, crate::value::Encoding), IdentId>,
    /// Forward table: IdentId -> IdentName.
    table: Vec<IdentName>,
    /// The source encoding each byte-interned symbol carries, set
    /// atomically by `get_id_from_bytes` at intern time. Collision-
    /// free now that the byte table is keyed by `(bytes, encoding)`.
    /// String/identifier interning never touches it, so method /
    /// ivar / constant dispatch is unaffected.
    enc_map: HashMap<IdentId, crate::value::Encoding>,
}

impl IdentifierTable {
    pub(crate) fn new() -> Self {
        let mut table = IdentifierTable {
            rev_table: HashMap::default(),
            rev_table_bytes: HashMap::default(),
            table: vec![IdentName::Utf8(String::new()); 100],
            enc_map: HashMap::default(),
        };
        table.set_id("initialize", IdentId::INITIALIZE);
        table.set_id("Object", IdentId::OBJECT);
        table.set_id("new", IdentId::NEW);
        table.set_id("name", IdentId::NAME);
        table.set_id("+", IdentId::_ADD);
        table.set_id("-", IdentId::_SUB);
        table.set_id("*", IdentId::_MUL);
        table.set_id("/", IdentId::_DIV);
        table.set_id("%", IdentId::_REM);
        table.set_id("**", IdentId::_POW);
        table.set_id("<<", IdentId::_SHL);
        table.set_id(">>", IdentId::_SHR);
        table.set_id("|", IdentId::_BOR);
        table.set_id("&", IdentId::_BAND);
        table.set_id("^", IdentId::_BXOR);
        table.set_id("~", IdentId::_BNOT);
        table.set_id("-@", IdentId::_UMINUS);
        table.set_id("+@", IdentId::_UPLUS);
        table.set_id("==", IdentId::_EQ);
        table.set_id("!=", IdentId::_NEQ);
        table.set_id(">", IdentId::_GT);
        table.set_id(">=", IdentId::_GE);
        table.set_id("<", IdentId::_LT);
        table.set_id("<=", IdentId::_LE);
        table.set_id("<=>", IdentId::_CMP);
        table.set_id("===", IdentId::_TEQ);
        table.set_id("=~", IdentId::_MATCH);
        table.set_id("[]", IdentId::_INDEX);
        table.set_id("[]=", IdentId::_INDEX_ASSIGN);
        table.set_id("...", IdentId::_DOT3);

        table.set_id("to_s", IdentId::TO_S);
        table.set_id("to_proc", IdentId::TO_PROC);
        table.set_id("method_missing", IdentId::METHOD_MISSING);
        table.set_id("each", IdentId::EACH);
        table.set_id("map", IdentId::MAP);
        table.set_id("/alias_method", IdentId::_ALIAS_METHOD);
        table.set_id("/enum", IdentId::_ENUM_FUNC);
        table.set_id("/name", IdentId::_NAME);
        table.set_id("/main", IdentId::_MAIN);
        table.set_id("float_second", IdentId::FLOAT_SECOND);
        table.set_id("second", IdentId::SECOND);
        table.set_id("float_millisecond", IdentId::FLOAT_MILLISECOND);
        table.set_id("millisecond", IdentId::MILLISECOND);
        table.set_id("float_microsecond", IdentId::FLOAT_MICROSECOND);
        table.set_id("microsecond", IdentId::MICROSECOND);
        table.set_id("nanosecond", IdentId::NANOSECOND);

        table.set_id("Encoding", IdentId::ENCODING);
        table.set_id("UTF_8", IdentId::UTF_8);
        table.set_id("ASCII_8BIT", IdentId::ASCII_8BIT);
        table.set_id("/encoding", IdentId::_ENCODING);
        table.set_id("to_ary", IdentId::TO_ARY);
        table.set_id("to_a", IdentId::TO_A);
        table.set_id("allocate", IdentId::ALLOCATE);
        table.set_id("method_added", IdentId::METHOD_ADDED);
        table.set_id("hash", IdentId::HASH);
        table.set_id("eql?", IdentId::EQL_);
        table.set_id("to_str", IdentId::TO_STR);
        table.set_id("to_path", IdentId::TO_PATH);
        table.set_id("singleton_method_added", IdentId::SINGLETON_METHOD_ADDED);
        table.set_id("method_removed", IdentId::METHOD_REMOVED);
        table.set_id("method_undefined", IdentId::METHOD_UNDEFINED);
        table.set_id("const_added", IdentId::CONST_ADDED);
        table.set_id("inherited", IdentId::INHERITED);
        table.set_id("append_features", IdentId::APPEND_FEATURES);
        table.set_id("extend_object", IdentId::EXTEND_OBJECT);
        table.set_id("extended", IdentId::EXTENDED);
        table.set_id("included", IdentId::INCLUDED);
        table.set_id("prepended", IdentId::PREPENDED);
        table.set_id("!~", IdentId::_UNMATCH);
        table.set_id("!", IdentId::_NOT);
        table.set_id("inspect", IdentId::INSPECT);
        table.set_id("singleton_method_removed", IdentId::SINGLETON_METHOD_REMOVED);
        table.set_id("singleton_method_undefined", IdentId::SINGLETON_METHOD_UNDEFINED);
        table.set_id("to_int", IdentId::TO_INT);
        table.set_id("to_f", IdentId::TO_F);
        table.set_id("initialize_copy", IdentId::INITIALIZE_COPY);
        table.set_id("initialize_clone", IdentId::INITIALIZE_CLONE);
        table.set_id("initialize_dup", IdentId::INITIALIZE_DUP);
        table.set_id("respond_to_missing?", IdentId::RESPOND_TO_MISSING_);
        for (name, id) in IdentId::SPECIAL_GVARS {
            table.set_id(name, id);
        }
        table.set_id("/encoding_override", IdentId::_ENCODING_OVERRIDE);
        table
    }

    fn set_id(&mut self, name: &str, id: IdentId) {
        self.rev_table.insert(name.to_string(), id);
        self.table[id.to_usize() - 1] = IdentName::Utf8(name.to_string());
    }

    fn get_id(&mut self, name: &str) -> IdentId {
        match self.rev_table.get(name) {
            Some(id) => *id,
            None => {
                let id = IdentId::from(self.table.len() as u32 + 1);
                self.rev_table.insert(name.to_string(), id);
                self.table.push(IdentName::Utf8(name.to_string()));
                id
            }
        }
    }

    fn get_id_from_string(&mut self, name: String) -> IdentId {
        match self.rev_table.get(&name) {
            Some(id) => *id,
            None => {
                let id = IdentId::from(self.table.len() as u32 + 1);
                self.rev_table.insert(name.clone(), id);
                self.table.push(IdentName::Utf8(name));
                id
            }
        }
    }

    fn get_id_from_bytes(&mut self, bytes: Vec<u8>, enc: crate::value::Encoding) -> IdentId {
        let key = (bytes, enc);
        match self.rev_table_bytes.get(&key) {
            Some(id) => *id,
            None => {
                let id = IdentId::from(self.table.len() as u32 + 1);
                let (bytes, enc) = key;
                self.rev_table_bytes.insert((bytes.clone(), enc), id);
                self.table.push(IdentName::Bytes(bytes));
                self.enc_map.insert(id, enc);
                id
            }
        }
    }

    ///
    /// Get the IdentName from *id*.
    ///
    fn get_ident_name(&self, id: IdentId) -> &IdentName {
        &self.table[id.to_usize() - 1]
    }

    ///
    /// Get instance variable name from *id*.
    ///
    /// ex) "var" -> "@var"
    ///
    fn add_ivar_prefix(&mut self, id: IdentId) -> IdentId {
        let ivar_name = format!("@{}", self.table[id.to_usize() - 1]);
        self.get_id_from_string(ivar_name)
    }

    ///
    /// Get assign method name from *id*.
    ///
    /// ex) "var" -> "var="
    ///
    fn add_assign_postfix(&mut self, id: IdentId) -> IdentId {
        let method_name = format!("{}=", self.table[id.to_usize() - 1]);
        self.get_id_from_string(method_name)
    }
}
