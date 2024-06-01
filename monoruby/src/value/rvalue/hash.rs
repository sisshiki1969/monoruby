use super::*;
use std::hash::Hash;
use std::ops::Deref;

#[monoruby_object]
pub struct Hashmap(Value);

impl Hashmap {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::HASH));
        Self(val)
    }

    pub fn index(&self, vm: &mut Executor, globals: &mut Globals, key: Value) -> Result<Value> {
        if let Some(v) = self.get(key) {
            Ok(v)
        } else if let Some(p) = self.default_proc {
            vm.invoke_proc(globals, p, &[self.0, key])
        } else {
            Ok(Value::nil())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HashId(usize);

#[derive(Debug, Clone)]
pub struct HashmapInner {
    default_proc: Option<Proc>,
    content: HashContent,
}

impl PartialEq for HashmapInner {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
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
        if let Some(p) = self.default_proc {
            p.mark(alloc)
        };
        for (k, v) in self.content.iter() {
            k.mark(alloc);
            v.mark(alloc);
        }
    }
}

impl HashmapInner {
    pub fn new(map: IndexMap<HashKey, Value>, default_proc: Option<Proc>) -> Self {
        HashmapInner {
            default_proc,
            content: HashContent::new(map),
        }
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

    pub fn get(&self, v: Value) -> Option<Value> {
        match &self.content {
            HashContent::Map(box map) => map.get(&HashKey(v)).copied(),
            HashContent::IdentMap(box map) => map.get(&IdentKey(v)).copied(),
        }
    }

    pub fn remove(&mut self, k: Value) -> Option<Value> {
        match &mut self.content {
            HashContent::Map(map) => map.shift_remove(&HashKey(k)),
            HashContent::IdentMap(map) => map.shift_remove(&IdentKey(k)),
        }
    }

    pub fn to_s(&self, globals: &Globals) -> String {
        match self.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in self.iter() {
                    let k_inspect = if let Some(h) = k.is_hash()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        k.inspect(globals)
                    };
                    let v_inspect = if let Some(h) = v.is_hash()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        v.inspect(globals)
                    };
                    result = if first {
                        format!("{k_inspect}=>{v_inspect}")
                    } else {
                        format!("{result}, {k_inspect}=>{v_inspect}")
                    };
                    first = false;
                }
                format! {"{{{}}}", result}
            }
        }
    }

    pub fn inspect2(&self, globals: &Globals) -> String {
        match self.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in self.iter().take(3) {
                    let k_inspect = if let Some(h) = k.is_hash()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        globals.inspect2(k)
                    };
                    let v_inspect = if let Some(h) = v.is_hash()
                        && h.id() == self.id()
                    {
                        "{...}".to_string()
                    } else {
                        globals.inspect2(v)
                    };
                    result = if first {
                        format!("{k_inspect}=>{v_inspect}")
                    } else {
                        format!("{result}, {k_inspect}=>{v_inspect}")
                    };
                    first = false;
                }
                format! {"{{{} .. }}", result}
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum HashContent {
    Map(Box<IndexMap<HashKey, Value>>),
    IdentMap(Box<IndexMap<IdentKey, Value>>),
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

impl PartialEq for HashContent {
    // This type of equality is used for comparison for keys of Hash.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HashContent::Map(map1), HashContent::Map(map2)) => map1 == map2,
            (HashContent::IdentMap(map1), HashContent::IdentMap(map2)) => {
                if map1.len() != map2.len() {
                    return false;
                };
                let mut m1 = HashMap::default();
                map1.iter().for_each(|(k, v)| {
                    m1.insert(HashKey(k.0), *v);
                });
                map2.iter()
                    .all(|(k, v)| m1.get(&HashKey(k.0)).map_or(false, |v2| *v == *v2))
            }
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Eq)]
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
                    ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", lhs),
                    ObjKind::BIGNUM => lhs.as_bignum().hash(state),
                    ObjKind::FLOAT => lhs.as_float().to_bits().hash(state),
                    ObjKind::STRING => lhs.as_bytes().hash(state),
                    ObjKind::ARRAY => lhs.as_array().hash(state),
                    ObjKind::RANGE => lhs.as_range().hash(state),
                    ObjKind::HASH => lhs.as_hashmap().hash(state),
                    //ObjKind::METHOD => lhs.method().hash(state),
                    _ => self.0.hash(state),
                }
            },
        }
    }
}

impl PartialEq for HashKey {
    // Object#eql?()
    // This type of equality is used for comparison for keys of Hash.
    fn eq(&self, other: &Self) -> bool {
        if self.0.id() == other.0.id() {
            return true;
        }
        match (self.try_rvalue(), other.try_rvalue()) {
            (None, None) => self.0.id() == other.0.id(),
            (Some(lhs), Some(rhs)) => lhs.eql(rhs),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq)]
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

impl PartialEq for IdentKey {
    // Object#eql?()
    // This type of equality is used for comparison for keys of Hash.
    fn eq(&self, other: &Self) -> bool {
        self.0.id() == other.0.id()
    }
}

pub enum MonorubyHashIntoIter {
    Map(indexmap::map::IntoIter<HashKey, Value>),
    IdentMap(indexmap::map::IntoIter<IdentKey, Value>),
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
    ($trait:ident) => {
        pub enum $trait<'a> {
            Map(indexmap::map::$trait<'a, HashKey, Value>),
            IdentMap(indexmap::map::$trait<'a, IdentKey, Value>),
        }
    };
}

define_iter!(Iter);
define_iter!(IterMut);

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
    pub(crate) fn new(map: IndexMap<HashKey, Value>) -> Self {
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

    pub(crate) fn insert(&mut self, k: Value, v: Value) {
        match self {
            HashContent::Map(box map) => map.insert(HashKey(k), v),
            HashContent::IdentMap(box map) => map.insert(IdentKey(k), v),
        };
    }

    /*pub(crate) fn remove(&mut self, k: Value) -> Option<Value> {
        match self {
            HashInfo::Map(map) => map.remove(&HashKey(k)),
            HashInfo::IdentMap(map) => map.remove(&IdentKey(k)),
        }
    }*/

    pub(crate) fn contains_key(&self, k: Value) -> bool {
        match self {
            HashContent::Map(map) => map.contains_key(&HashKey(k)),
            HashContent::IdentMap(map) => map.contains_key(&IdentKey(k)),
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

    pub(crate) fn compare_by_identity(&mut self) {
        match self {
            HashContent::Map(box map) => {
                let mut new_map = indexmap::IndexMap::default();
                for (k, v) in map.iter() {
                    new_map.insert(IdentKey(k.0), *v);
                }
                *self = HashContent::IdentMap(Box::new(new_map));
            }
            HashContent::IdentMap(_) => {}
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn hash0() {
        let mut map = HashmapInner::new(IndexMap::default(), None);
        map.insert(Value::integer(5), Value::float(12.0));
        map.insert(Value::integer(5), Value::float(5.7));
        map.insert(Value::integer(7), Value::float(42.5));
        assert_eq!(Some(Value::float(5.7)), map.get(Value::integer(5)));
        assert_eq!(vec![Value::integer(5), Value::integer(7)], map.keys());
        assert_eq!(vec![Value::float(5.7), Value::float(42.5)], map.values());
        assert_eq!(2, map.len())
    }
}
