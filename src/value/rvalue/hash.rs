use crate::*;
use std::hash::Hash;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub enum HashInfo {
    Map(Box<IndexMap<HashKey, Value>>),
    IdentMap(Box<IndexMap<IdentKey, Value>>),
}

impl Hash for HashInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            HashInfo::Map(h) => {
                for (key, val) in h.iter() {
                    key.hash(state);
                    val.hash(state);
                }
            }
            HashInfo::IdentMap(h) => {
                for (key, val) in h.iter() {
                    key.hash(state);
                    val.hash(state);
                }
            }
        }
    }
}

impl PartialEq for HashInfo {
    // This type of equality is used for comparison for keys of Hash.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HashInfo::Map(map1), HashInfo::Map(map2)) => map1 == map2,
            (HashInfo::IdentMap(map1), HashInfo::IdentMap(map2)) => {
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

#[derive(Debug, Clone, Copy, Eq)]
pub struct HashKey(pub Value);

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
            Some(lhs) => match lhs.kind() {
                ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", lhs),
                ObjKind::BIGNUM => lhs.as_bignum().hash(state),
                ObjKind::FLOAT => lhs.as_float().to_bits().hash(state),
                ObjKind::BYTES => lhs.as_bytes().hash(state),
                ObjKind::ARRAY => lhs.as_array().hash(state),
                ObjKind::RANGE => lhs.as_range().hash(state),
                ObjKind::HASH => lhs.as_hash().hash(state),
                //ObjKind::METHOD => lhs.method().hash(state),
                _ => self.0.hash(state),
            },
        }
    }
}

impl PartialEq for HashKey {
    // Object#eql?()
    // This type of equality is used for comparison for keys of Hash.
    fn eq(&self, other: &Self) -> bool {
        if self.0.get() == other.0.get() {
            return true;
        }
        match (self.try_rvalue(), other.try_rvalue()) {
            (None, None) => self.0.get() == other.0.get(),
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
        (self.0.get()).hash(state);
    }
}

impl PartialEq for IdentKey {
    // Object#eql?()
    // This type of equality is used for comparison for keys of Hash.
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}

pub enum IntoIter {
    Map(indexmap::map::IntoIter<HashKey, Value>),
    IdentMap(indexmap::map::IntoIter<IdentKey, Value>),
}

impl IntoIter {
    fn new(hash: HashInfo) -> IntoIter {
        match hash {
            HashInfo::Map(map) => IntoIter::Map(map.into_iter()),
            HashInfo::IdentMap(map) => IntoIter::IdentMap(map.into_iter()),
        }
    }
}

impl Iterator for IntoIter {
    type Item = (Value, Value);
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            IntoIter::Map(map) => map.next().map(|(k, v)| (k.0, v)),
            IntoIter::IdentMap(map) => map.next().map(|(k, v)| (k.0, v)),
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
                    HashInfo::Map(map) => $ty1::Map(map.$method()),
                    HashInfo::IdentMap(map) => $ty1::IdentMap(map.$method()),
                }
            }
        }
    };
}

define_iter_new!(Iter, &HashInfo, iter);
define_iter_new!(IterMut, &mut HashInfo, iter_mut);

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

define_into_iterator!(&'a HashInfo, Iter);
define_into_iterator!(&'a mut HashInfo, IterMut);

impl IntoIterator for HashInfo {
    type Item = (Value, Value);
    type IntoIter = IntoIter;

    fn into_iter(self) -> IntoIter {
        IntoIter::new(self)
    }
}

impl GC<RValue> for HashInfo {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        for (k, v) in self.iter() {
            k.mark(alloc);
            v.mark(alloc);
        }
    }
}

impl HashInfo {
    pub(crate) fn new(map: IndexMap<HashKey, Value>) -> Self {
        HashInfo::Map(Box::new(map))
    }

    pub(crate) fn iter(&self) -> Iter {
        Iter::new(self)
    }

    pub(crate) fn get(&self, v: Value) -> Option<Value> {
        match self {
            HashInfo::Map(box map) => map.get(&HashKey(v)).copied(),
            HashInfo::IdentMap(box map) => map.get(&IdentKey(v)).copied(),
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            HashInfo::Map(box map) => map.len(),
            HashInfo::IdentMap(box map) => map.len(),
        }
    }

    pub(crate) fn clear(&mut self) {
        match self {
            HashInfo::Map(box map) => map.clear(),
            HashInfo::IdentMap(box map) => map.clear(),
        }
    }

    pub(crate) fn insert(&mut self, k: Value, v: Value) {
        match self {
            HashInfo::Map(box map) => map.insert(HashKey(k), v),
            HashInfo::IdentMap(box map) => map.insert(IdentKey(k), v),
        };
    }

    pub(crate) fn remove(&mut self, k: Value) -> Option<Value> {
        match self {
            HashInfo::Map(map) => map.remove(&HashKey(k)),
            HashInfo::IdentMap(map) => map.remove(&IdentKey(k)),
        }
    }

    pub(crate) fn contains_key(&self, k: Value) -> bool {
        match self {
            HashInfo::Map(map) => map.contains_key(&HashKey(k)),
            HashInfo::IdentMap(map) => map.contains_key(&IdentKey(k)),
        }
    }

    pub(crate) fn keys(&self) -> Vec<Value> {
        match self {
            HashInfo::Map(map) => map.keys().map(|x| x.0).collect(),
            HashInfo::IdentMap(map) => map.keys().map(|x| x.0).collect(),
        }
    }

    pub(crate) fn values(&self) -> Vec<Value> {
        match self {
            HashInfo::Map(map) => map.values().cloned().collect(),
            HashInfo::IdentMap(map) => map.values().cloned().collect(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn hash0() {
        let mut map = HashInfo::new(IndexMap::default());
        map.insert(Value::new_integer(5), Value::new_float(12.0));
        map.insert(Value::new_integer(5), Value::new_float(5.7));
        map.insert(Value::new_integer(7), Value::new_float(42.5));
        assert_eq!(Some(Value::new_float(5.7)), map.get(Value::new_integer(5)));
        assert_eq!(
            vec![Value::new_integer(5), Value::new_integer(7)],
            map.keys()
        );
        assert_eq!(
            vec![Value::new_float(5.7), Value::new_float(42.5)],
            map.values()
        );
        assert_eq!(2, map.len())
    }
}
