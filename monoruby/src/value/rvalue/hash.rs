use super::*;
use std::hash::Hash;
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
    pub fn new(map: RubyMap<Value, Value>) -> Self {
        HashmapInner {
            default: HashDefault::default(),
            content: HashContent::new(map),
        }
    }

    pub fn new_with_default(map: RubyMap<Value, Value>, default: Value) -> Self {
        HashmapInner {
            default: HashDefault::Value(default),
            content: HashContent::new(map),
        }
    }

    pub fn new_with_default_proc(map: RubyMap<Value, Value>, default_proc: Proc) -> Self {
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
            HashContent::Map(box map) => map.get(&v, vm, globals)?.copied(),
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
            HashContent::Map(map) => map.shift_remove(&k, vm, globals)?,
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
    Map(Box<RubyMap<Value, Value>>),
    IdentMap(Box<RubyMap<IdentKey, Value>>),
}

impl std::default::Default for HashContent {
    fn default() -> Self {
        HashContent::Map(Box::new(RubyMap::default()))
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for HashContent {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        e: &mut Executor,
        g: &mut Globals,
    ) -> Result<()> {
        match self {
            HashContent::Map(h) => {
                for (key, val) in h.iter() {
                    key.ruby_hash(state, e, g)?;
                    val.ruby_hash(state, e, g)?;
                }
            }
            HashContent::IdentMap(h) => {
                for (key, val) in h.iter() {
                    key.ruby_hash(state, e, g)?;
                    val.ruby_hash(state, e, g)?;
                }
            }
        }
        Ok(())
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for HashContent {
    // This type of equality is used for comparison for keys of Hash.
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        match (self, other) {
            (HashContent::Map(map1), HashContent::Map(map2)) => map1.eql(map2, vm, globals),
            (HashContent::IdentMap(map1), HashContent::IdentMap(map2)) => {
                map1.eql(map2, vm, globals)
            }
            _ => Ok(false),
        }
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

pub enum MonorubyHashIntoIter {
    Map(rubymap::map::IntoIter<Value, Value>),
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
            MonorubyHashIntoIter::Map(map) => map.next().map(|(k, v)| (k, v)),
            MonorubyHashIntoIter::IdentMap(map) => map.next().map(|(k, v)| (k.0, v)),
        }
    }
}
macro_rules! define_iter {
    ($($trait:ident),*) => {
        $(
            pub enum $trait<'a> {
                Map(rubymap::map::$trait<'a, Value, Value>),
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
                    $ty2::Map(map) => map.next().map(|(k, v)| (*k, *v)),
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
    pub(crate) fn new(map: RubyMap<Value, Value>) -> Self {
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
            HashContent::Map(box map) => map.insert(k, v, vm, globals)?,
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
            HashContent::Map(map) => map.contains_key(&k, vm, globals),
            HashContent::IdentMap(map) => map.contains_key(&IdentKey(k), vm, globals),
        }
    }

    pub(crate) fn keys(&self) -> Vec<Value> {
        match self {
            HashContent::Map(map) => map.keys().map(|x| *x).collect(),
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
                    new_map.insert(IdentKey(*k), *v, vm, globals)?;
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
