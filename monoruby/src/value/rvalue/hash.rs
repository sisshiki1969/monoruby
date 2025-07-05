use rubymap::RubyEql;

use super::*;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

#[monoruby_object]
pub struct Hashmap(Value);

impl Hashmap {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::HASH));
        Self(val)
    }

    pub fn index(&self, vm: &mut Executor, globals: &mut Globals, key: Value) -> Result<Value> {
        if let Some(v) = self.get(key, vm, globals)? {
            Ok(v)
        } else {
            match self.default {
                HashDefault::Proc(proc) => vm.invoke_proc(globals, &proc, &[self.0, key]),
                HashDefault::Value(v) => Ok(v.dup()),
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HashId(usize);

#[derive(Debug, Clone)]
enum HashDefault {
    Value(Value),
    Proc(Proc),
}

impl std::default::Default for HashDefault {
    fn default() -> Self {
        HashDefault::Value(Value::nil())
    }
}

#[derive(Debug, Clone, Default)]
pub struct HashmapInner {
    default: HashDefault,
    content: HashContent,
}

impl RubyEql<Executor, Globals, MonorubyErr> for HashmapInner {
    // This type of equality is used for comparison for keys of Hash.
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        self.content.eql(&other.content, vm, globals)
    }
}

impl std::ops::Deref for HashmapInner {
    type Target = HashContent;
    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl std::ops::DerefMut for HashmapInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.content
    }
}

impl alloc::GC<RValue> for HashmapInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        match self.default {
            HashDefault::Proc(p) => p.mark(alloc),
            HashDefault::Value(v) => v.mark(alloc),
        }
        for (k, v) in self.content.iter() {
            k.mark(alloc);
            v.mark(alloc);
        }
    }
}

impl HashmapInner {
    pub fn new(map: RubyMap<HashKey, Value>) -> Self {
        HashmapInner {
            default: HashDefault::default(),
            content: HashContent::new(map),
        }
    }

    pub fn new_with_default(map: RubyMap<HashKey, Value>, default: Value) -> Self {
        HashmapInner {
            default: HashDefault::Value(default),
            content: HashContent::new(map),
        }
    }

    pub fn new_with_default_proc(map: RubyMap<HashKey, Value>, default_proc: Proc) -> Self {
        HashmapInner {
            default: HashDefault::Proc(default_proc),
            content: HashContent::new(map),
        }
    }

    pub fn defalut_value(&self) -> Option<Value> {
        if let HashDefault::Value(v) = self.default {
            Some(v)
        } else {
            None
        }
    }

    pub fn defalut_proc(&self) -> Option<Proc> {
        if let HashDefault::Proc(p) = self.default {
            Some(p)
        } else {
            None
        }
    }

    pub fn set_defalut_value(&mut self, default: Value) {
        self.default = HashDefault::Value(default);
    }

    pub fn set_defalut_proc(&mut self, default_proc: Proc) {
        self.default = HashDefault::Proc(default_proc);
    }

    fn id(&self) -> HashId {
        HashId(&self as *const _ as usize)
    }

    pub fn is_empty(&self) -> bool {
        match &self.content {
            HashContent::Map(box map) => map.is_empty(),
            HashContent::IdentMap(box map) => map.is_empty(),
        }
    }

    pub fn get(&self, v: Value, vm: &mut Executor, globals: &mut Globals) -> Result<Option<Value>> {
        Ok(match &self.content {
            HashContent::Map(box map) => map.get(&HashKey(v), vm, globals)?.copied(),
            HashContent::IdentMap(box map) => map.get(&IdentKey(v), vm, globals)?.copied(),
        })
    }

    pub fn remove(
        &mut self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Value>> {
        Ok(match &mut self.content {
            HashContent::Map(map) => map.shift_remove(&HashKey(k), vm, globals)?,
            HashContent::IdentMap(map) => map.shift_remove(&IdentKey(k), vm, globals)?,
        })
    }

    /*pub fn entry_and_modify<F>(&mut self, k: Value, f: F)
    where
        F: FnOnce(&mut Value),
    {
        match &mut self.content {
            HashContent::Map(map) => {
                map.entry(HashKey(k)).and_modify(f);
            }
            HashContent::IdentMap(map) => {
                map.entry(IdentKey(k)).and_modify(f);
            }
        }
    }*/

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

    pub fn to_s(&self, store: &Store) -> String {
        match self.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in self.iter() {
                    let k_inspect = if let Some(h) = k.try_hash_ty()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        k.inspect(store)
                    };
                    let v_inspect = if let Some(h) = v.try_hash_ty()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        v.inspect(store)
                    };
                    let s = if let Some(k) = k.try_symbol() {
                        format!("{k}: {v_inspect}")
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

#[derive(Debug, Clone)]
pub enum HashContent {
    Map(Box<RubyMap<HashKey, Value>>),
    IdentMap(Box<RubyMap<IdentKey, Value>>),
}

impl std::default::Default for HashContent {
    fn default() -> Self {
        HashContent::Map(Box::new(RubyMap::default()))
    }
}

impl Hash for HashContent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            HashContent::Map(h) => {
                for (key, val) in h.iter() {
                    key.hash(state);
                    val.hash(state);
                }
            }
            HashContent::IdentMap(h) => {
                for (key, val) in h.iter() {
                    key.hash(state);
                    val.hash(state);
                }
            }
        }
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for HashContent {
    // This type of equality is used for comparison for keys of Hash.
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        match (self, other) {
            (HashContent::Map(map1), HashContent::Map(map2)) => {
                if map1.len() != map2.len() {
                    return Ok(false);
                };
                for (k, v1) in map1.iter() {
                    match map2.get(k, vm, globals)? {
                        None => return Ok(false),
                        Some(v2) => {
                            if !HashKey(*v1).eql(&HashKey(*v2), vm, globals)? {
                                return Ok(false);
                            }
                        }
                    }
                }
                Ok(true)
            }
            (HashContent::IdentMap(map1), HashContent::IdentMap(map2)) => {
                if map1.len() != map2.len() {
                    return Ok(false);
                };
                let mut m1 = RubyMap::default();
                for (k, v) in map1.iter() {
                    m1.insert(HashKey(k.0), *v, vm, globals)?;
                }
                for (k, v) in map2.iter() {
                    match m1.get(&HashKey(k.0), vm, globals)? {
                        None => return Ok(false),
                        Some(v1) => {
                            if !HashKey(*v1).eql(&HashKey(*v), vm, globals)? {
                                return Ok(false);
                            }
                        }
                    }
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }
}

#[derive(Clone, Copy)]
pub struct HashKey(pub Value);

impl std::fmt::Debug for HashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HashKey({:?})", self.0)
    }
}

impl Deref for HashKey {
    type Target = Value;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Hash for HashKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self.try_rvalue() {
            None => self.0.hash(state),
            Some(lhs) => unsafe {
                match lhs.ty() {
                    //ObjTy::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", lhs),
                    ObjTy::BIGNUM => lhs.as_bignum().hash(state),
                    ObjTy::FLOAT => lhs.as_float().to_bits().hash(state),
                    ObjTy::STRING => lhs.as_rstring().hash(state),
                    ObjTy::ARRAY => lhs.as_array().hash(state),
                    ObjTy::RANGE => lhs.as_range().hash(state),
                    ObjTy::HASH => lhs.as_hashmap().hash(state),
                    //ObjTy::METHOD => lhs.method().hash(state),
                    _ => self.0.hash(state),
                }
            },
        }
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for HashKey {
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        if self.0.id() == other.0.id() {
            return Ok(true);
        }
        match (self.try_rvalue(), other.try_rvalue()) {
            (None, None) => Ok(self.0.id() == other.0.id()),
            (Some(lhs), Some(rhs)) => lhs.eql(rhs, vm, globals),
            _ => Ok(false),
        }
    }
}

impl HashKey {
    pub fn calculate_hash(self) -> u64 {
        let mut s = std::hash::DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentKey(pub Value);

impl Deref for IdentKey {
    type Target = Value;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Hash for IdentKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.id()).hash(state);
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for IdentKey {
    // Object#eql?()
    // This type of equality is used for comparison for keys of Hash.
    fn eql(&self, other: &Self, _: &mut Executor, _: &mut Globals) -> Result<bool> {
        Ok(self.0.id() == other.0.id())
    }
}

pub enum MonorubyHashIntoIter {
    Map(rubymap::map::IntoIter<HashKey, Value>),
    IdentMap(rubymap::map::IntoIter<IdentKey, Value>),
}

impl MonorubyHashIntoIter {
    fn new(hash: HashContent) -> MonorubyHashIntoIter {
        match hash {
            HashContent::Map(map) => MonorubyHashIntoIter::Map(map.into_iter()),
            HashContent::IdentMap(map) => MonorubyHashIntoIter::IdentMap(map.into_iter()),
        }
    }
}

impl Iterator for MonorubyHashIntoIter {
    type Item = (Value, Value);
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MonorubyHashIntoIter::Map(map) => map.next().map(|(k, v)| (k.0, v)),
            MonorubyHashIntoIter::IdentMap(map) => map.next().map(|(k, v)| (k.0, v)),
        }
    }
}
macro_rules! define_iter {
    ($($trait:ident),*) => {
        $(
            pub enum $trait<'a> {
                Map(rubymap::map::$trait<'a, HashKey, Value>),
                IdentMap(rubymap::map::$trait<'a, IdentKey, Value>),
            }
        )*
    };
}

define_iter!(Iter, IterMut);

macro_rules! define_iter_new {
    ($ty1: ident, $ty2: ty, $method: ident) => {
        impl<'a> $ty1<'a> {
            fn new(hash: $ty2) -> $ty1 {
                match hash {
                    HashContent::Map(map) => $ty1::Map(map.$method()),
                    HashContent::IdentMap(map) => $ty1::IdentMap(map.$method()),
                }
            }
        }
    };
}

define_iter_new!(Iter, &HashContent, iter);
define_iter_new!(IterMut, &mut HashContent, iter_mut);

macro_rules! define_iterator {
    ($ty2:ident) => {
        impl<'a> Iterator for $ty2<'a> {
            type Item = (Value, Value);
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    $ty2::Map(map) => map.next().map(|(k, v)| (k.0, *v)),
                    $ty2::IdentMap(map) => map.next().map(|(k, v)| (k.0, *v)),
                }
            }
        }
    };
}

define_iterator!(Iter);
define_iterator!(IterMut);

macro_rules! define_into_iterator {
    ($ty1:ty, $ty2:ident) => {
        impl<'a> IntoIterator for $ty1 {
            type Item = (Value, Value);
            type IntoIter = $ty2<'a>;
            fn into_iter(self) -> $ty2<'a> {
                $ty2::new(self)
            }
        }
    };
}

define_into_iterator!(&'a HashContent, Iter);
define_into_iterator!(&'a mut HashContent, IterMut);

impl IntoIterator for HashContent {
    type Item = (Value, Value);
    type IntoIter = MonorubyHashIntoIter;

    fn into_iter(self) -> MonorubyHashIntoIter {
        MonorubyHashIntoIter::new(self)
    }
}

impl HashContent {
    pub(crate) fn new(map: RubyMap<HashKey, Value>) -> Self {
        HashContent::Map(Box::new(map))
    }

    pub(crate) fn iter(&self) -> Iter {
        Iter::new(self)
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            HashContent::Map(box map) => map.len(),
            HashContent::IdentMap(box map) => map.len(),
        }
    }

    pub(crate) fn clear(&mut self) {
        match self {
            HashContent::Map(box map) => map.clear(),
            HashContent::IdentMap(box map) => map.clear(),
        }
    }

    pub(crate) fn insert(
        &mut self,
        k: Value,
        v: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        match self {
            HashContent::Map(box map) => map.insert(HashKey(k), v, vm, globals)?,
            HashContent::IdentMap(box map) => map.insert(IdentKey(k), v, vm, globals)?,
        };
        Ok(())
    }

    /*pub(crate) fn remove(&mut self, k: Value) -> Option<Value> {
        match self {
            HashInfo::Map(map) => map.remove(&HashKey(k)),
            HashInfo::IdentMap(map) => map.remove(&IdentKey(k)),
        }
    }*/

    pub(crate) fn contains_key(
        &self,
        k: Value,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<bool> {
        match self {
            HashContent::Map(map) => map.contains_key(&HashKey(k), vm, globals),
            HashContent::IdentMap(map) => map.contains_key(&IdentKey(k), vm, globals),
        }
    }

    pub(crate) fn keys(&self) -> Vec<Value> {
        match self {
            HashContent::Map(map) => map.keys().map(|x| x.0).collect(),
            HashContent::IdentMap(map) => map.keys().map(|x| x.0).collect(),
        }
    }

    pub(crate) fn values(&self) -> Vec<Value> {
        match self {
            HashContent::Map(map) => map.values().cloned().collect(),
            HashContent::IdentMap(map) => map.values().cloned().collect(),
        }
    }

    pub(crate) fn compare_by_identity(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        match self {
            HashContent::Map(box map) => {
                let mut new_map = RubyMap::default();
                for (k, v) in map.iter() {
                    new_map.insert(IdentKey(k.0), *v, vm, globals)?;
                }
                *self = HashContent::IdentMap(Box::new(new_map));
            }
            HashContent::IdentMap(_) => {}
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
