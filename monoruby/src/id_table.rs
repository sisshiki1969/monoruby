use super::*;
use std::num::NonZeroU32;
use std::sync::{LazyLock, RwLock};

static ID: LazyLock<RwLock<IdentifierTable>> =
    LazyLock::new(|| RwLock::new(IdentifierTable::new()));

///
/// Wrapper of ID for strings.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct IdentId(NonZeroU32);

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
    pub const _POW: IdentId = id!(8);
    pub const _SHL: IdentId = id!(9);
    pub const _REM: IdentId = id!(10);
    pub const _EQ: IdentId = id!(11);
    pub const _NEQ: IdentId = id!(12);
    pub const _GT: IdentId = id!(13);
    pub const _GE: IdentId = id!(14);
    pub const _DIV: IdentId = id!(15);
    pub const _LT: IdentId = id!(16);
    pub const _LE: IdentId = id!(17);
    pub const _CMP: IdentId = id!(18);
    pub const _TEQ: IdentId = id!(19);
    pub const _ENUM_FUNC: IdentId = id!(20);
    pub const _INDEX: IdentId = id!(21);
    pub const _INDEX_ASSIGN: IdentId = id!(22);
    pub const TO_S: IdentId = id!(23);
    pub const _SHR: IdentId = id!(24);
    pub const _ALIAS_METHOD: IdentId = id!(25);
    pub const _METHOD_MISSING: IdentId = id!(26);
    pub const EACH: IdentId = id!(27);
    pub const MAP: IdentId = id!(28);
    pub const _NAME: IdentId = id!(29);
    pub const _DOT3: IdentId = id!(30);
    pub const _MAIN: IdentId = id!(31);
    pub const _BOR: IdentId = id!(32);
    pub const _BAND: IdentId = id!(33);
    pub const _BXOR: IdentId = id!(34);
    pub const _UMINUS: IdentId = id!(35);
    pub const FLOAT_SECOND: IdentId = id!(36);
    pub const SECOND: IdentId = id!(37);
    pub const FLOAT_MILLISECOND: IdentId = id!(38);
    pub const MILLISECOND: IdentId = id!(39);
    pub const FLOAT_MICROSECOND: IdentId = id!(40);
    pub const MICROSECOND: IdentId = id!(41);
    pub const NANOSECOND: IdentId = id!(42);
    pub const _MATCH: IdentId = id!(43);
    pub const TO_PROC: IdentId = id!(44);
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
    /// Get *IdentId* from &str.
    ///
    pub fn get_id(name: &str) -> IdentId {
        ID.write().unwrap().get_id(name)
    }

    ///
    /// Get *IdentId* from String.
    ///
    pub fn get_id_from_string(name: String) -> IdentId {
        ID.write().unwrap().get_id_from_string(name)
    }

    ///
    /// Get name as String from *self*.
    ///
    pub fn get_name(&self) -> String {
        ID.read().unwrap().get_name(*self).to_string()
    }

    ///
    /// Compare *self* to *other*.
    ///
    pub fn compare(&self, other: &Self) -> std::cmp::Ordering {
        let id = ID.read().unwrap();
        let lhs = id.get_name(*self);
        let rhs = id.get_name(*other);
        lhs.cmp(rhs)
    }

    ///
    /// Append the name of *self* to *s*.
    ///
    #[cfg(feature = "dump-bc")]
    pub(crate) fn append_to(self, s: &mut String) {
        ID.read().unwrap().append_to(self, s);
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
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct IdentifierTable {
    rev_table: HashMap<String, IdentId>,
    table: Vec<String>,
}

impl IdentifierTable {
    pub(crate) fn new() -> Self {
        let mut table = IdentifierTable {
            rev_table: HashMap::default(),
            table: vec![String::new(); 50],
        };
        table.set_id("initialize", IdentId::INITIALIZE);
        table.set_id("Object", IdentId::OBJECT);
        table.set_id("new", IdentId::NEW);
        table.set_id("name", IdentId::NAME);
        table.set_id("+", IdentId::_ADD);
        table.set_id("-", IdentId::_SUB);
        table.set_id("*", IdentId::_MUL);
        table.set_id("**", IdentId::_POW);
        table.set_id("<<", IdentId::_SHL);
        table.set_id("%", IdentId::_REM);
        table.set_id("==", IdentId::_EQ);
        table.set_id("!=", IdentId::_NEQ);
        table.set_id(">", IdentId::_GT);
        table.set_id(">=", IdentId::_GE);
        table.set_id("/", IdentId::_DIV);
        table.set_id("<", IdentId::_LT);
        table.set_id("<=", IdentId::_LE);
        table.set_id("<=>", IdentId::_CMP);
        table.set_id("===", IdentId::_TEQ);
        table.set_id("/enum", IdentId::_ENUM_FUNC);
        table.set_id("[]", IdentId::_INDEX);
        table.set_id("[]=", IdentId::_INDEX_ASSIGN);
        table.set_id("to_s", IdentId::TO_S);
        table.set_id(">>", IdentId::_SHR);
        table.set_id("/alias_method", IdentId::_ALIAS_METHOD);
        table.set_id("method_missing", IdentId::_METHOD_MISSING);
        table.set_id("each", IdentId::EACH);
        table.set_id("map", IdentId::MAP);
        table.set_id("/name", IdentId::_NAME);
        table.set_id("...", IdentId::_DOT3);
        table.set_id("/main", IdentId::_MAIN);
        table.set_id("|", IdentId::_BOR);
        table.set_id("&", IdentId::_BAND);
        table.set_id("^", IdentId::_BXOR);
        table.set_id("-@", IdentId::_UMINUS);
        table.set_id("float_second", IdentId::FLOAT_SECOND);
        table.set_id("second", IdentId::SECOND);
        table.set_id("float_millisecond", IdentId::FLOAT_MILLISECOND);
        table.set_id("millisecond", IdentId::MILLISECOND);
        table.set_id("float_microsecond", IdentId::FLOAT_MICROSECOND);
        table.set_id("microsecond", IdentId::MICROSECOND);
        table.set_id("nanosecond", IdentId::NANOSECOND);
        table.set_id("=~", IdentId::_MATCH);
        table.set_id("to_proc", IdentId::TO_PROC);
        table
    }

    fn set_id(&mut self, name: &str, id: IdentId) {
        self.rev_table.insert(name.to_string(), id);
        self.table[id.to_usize() - 1] = name.to_string();
    }

    fn get_id(&mut self, name: &str) -> IdentId {
        match self.rev_table.get(name) {
            Some(id) => *id,
            None => {
                let id = IdentId::from(self.table.len() as u32 + 1);
                self.rev_table.insert(name.to_string(), id);
                self.table.push(name.to_string());
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
                self.table.push(name);
                id
            }
        }
    }

    ///
    /// Get the name as &str from *self*.
    ///
    fn get_name(&self, id: IdentId) -> &str {
        &self.table[id.to_usize() - 1]
    }

    ///
    /// Append the name of *self* to *s*.
    ///
    #[cfg(feature = "dump-bc")]
    fn append_to(&self, id: IdentId, s: &mut String) {
        s.push_str(self.table[id.to_usize() - 1].as_str());
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
