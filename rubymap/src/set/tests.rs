use super::*;
use std::string::String;
struct E;
struct G;

#[test]
fn it_works() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    assert_eq!(set.is_empty(), true);
    set.insert(1, &mut e, &mut g).unwrap();
    set.insert(1, &mut e, &mut g).unwrap();
    assert_eq!(set.len(), 1);
    assert!(set.get(&1, &mut e, &mut g).unwrap().is_some());
    assert_eq!(set.is_empty(), false);
}

#[test]
fn new() {
    let set: RubySet<String, E, G, ()> = RubySet::new();
    println!("{:?}", set);
    assert_eq!(set.capacity(), 0);
    assert_eq!(set.len(), 0);
    assert_eq!(set.is_empty(), true);
}

#[test]
fn insert() {
    let insert = [0, 4, 2, 12, 8, 7, 11, 5];
    let not_present = [1, 3, 6, 9, 10];
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::with_capacity(insert.len());

    for (i, &elt) in insert.iter().enumerate() {
        assert_eq!(set.len(), i);
        set.insert(elt, &mut e, &mut g).unwrap();
        assert_eq!(set.len(), i + 1);
        assert_eq!(set.get(&elt, &mut e, &mut g).unwrap(), Some(&elt));
    }
    println!("{:?}", set);

    for &elt in &not_present {
        assert!(set.get(&elt, &mut e, &mut g).unwrap().is_none());
    }
}

#[test]
fn insert_full() {
    let insert = vec![9, 2, 7, 1, 4, 6, 13];
    let present = vec![1, 6, 2];
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::with_capacity(insert.len());

    for (i, &elt) in insert.iter().enumerate() {
        assert_eq!(set.len(), i);
        let (index, success) = set.insert_full(elt, &mut e, &mut g).unwrap();
        assert!(success);
        assert_eq!(
            Some(index),
            set.get_full(&elt, &mut e, &mut g).unwrap().map(|x| x.0)
        );
        assert_eq!(set.len(), i + 1);
    }

    let len = set.len();
    for &elt in &present {
        let (index, success) = set.insert_full(elt, &mut e, &mut g).unwrap();
        assert!(!success);
        assert_eq!(
            Some(index),
            set.get_full(&elt, &mut e, &mut g).unwrap().map(|x| x.0)
        );
        assert_eq!(set.len(), len);
    }
}

#[test]
fn insert_2() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::with_capacity(16);

    let mut values = vec![];
    values.extend(0..16);
    values.extend(if cfg!(miri) { 32..64 } else { 128..267 });

    for &i in &values {
        let old_set = set.clone();
        set.insert(i, &mut e, &mut g).unwrap();
        for value in old_set.iter() {
            if set.get(value, &mut e, &mut g).unwrap().is_none() {
                println!("old_set: {:?}", old_set);
                println!("set: {:?}", set);
                panic!("did not find {} in set", value);
            }
        }
    }

    for &i in &values {
        assert!(
            set.get(&i, &mut e, &mut g).unwrap().is_some(),
            "did not find {}",
            i
        );
    }
}

#[test]
fn insert_dup() {
    let mut elements = vec![0, 2, 4, 6, 8];
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<u8, E, G, ()> =
        RubySet::from_iter(elements.drain(..), &mut e, &mut g).unwrap();
    {
        let (i, v) = set.get_full(&0, &mut e, &mut g).unwrap().unwrap();
        assert_eq!(set.len(), 5);
        assert_eq!(i, 0);
        assert_eq!(*v, 0);
    }
    {
        let inserted = set.insert(0, &mut e, &mut g).unwrap();
        let (i, v) = set.get_full(&0, &mut e, &mut g).unwrap().unwrap();
        assert_eq!(set.len(), 5);
        assert_eq!(inserted, false);
        assert_eq!(i, 0);
        assert_eq!(*v, 0);
    }
}

#[test]
fn insert_order() {
    let insert = [0, 4, 2, 12, 8, 7, 11, 5, 3, 17, 19, 22, 23];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::new();

    for &elt in &insert {
        set.insert(elt, &mut e, &mut g).unwrap();
    }

    assert_eq!(set.iter().count(), set.len());
    assert_eq!(set.iter().count(), insert.len());
    for (a, b) in insert.iter().zip(set.iter()) {
        assert_eq!(a, b);
    }
    for (i, v) in (0..insert.len()).zip(set.iter()) {
        assert_eq!(set.get_index(i).unwrap(), v);
    }
}

#[test]
fn shift_insert() {
    let insert = [0, 4, 2, 12, 8, 7, 11, 5, 3, 17, 19, 22, 23];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::new();

    for &elt in &insert {
        set.shift_insert(0, elt, &mut e, &mut g).unwrap();
    }

    assert_eq!(set.iter().count(), set.len());
    assert_eq!(set.iter().count(), insert.len());
    for (a, b) in insert.iter().rev().zip(set.iter()) {
        assert_eq!(a, b);
    }
    for (i, v) in (0..insert.len()).zip(set.iter()) {
        assert_eq!(set.get_index(i).unwrap(), v);
    }

    // "insert" that moves an existing entry
    set.shift_insert(0, insert[0], &mut e, &mut g).unwrap();
    assert_eq!(set.iter().count(), insert.len());
    assert_eq!(&insert[0], set.get(&0, &mut e, &mut g).unwrap().unwrap());
    for (a, b) in insert[1..].iter().rev().zip(set.iter().skip(1)) {
        assert_eq!(a, b);
    }
}

#[test]
fn replace() {
    let replace = [0, 4, 2, 12, 8, 7, 11, 5];
    let not_present = [1, 3, 6, 9, 10];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::with_capacity(replace.len());

    for (i, &elt) in replace.iter().enumerate() {
        assert_eq!(set.len(), i);
        set.replace(elt, &mut e, &mut g).unwrap();
        assert_eq!(set.len(), i + 1);
        assert_eq!(set.get(&elt, &mut e, &mut g).unwrap(), Some(&elt));
    }
    println!("{:?}", set);

    for &elt in &not_present {
        assert!(set.get(&elt, &mut e, &mut g).unwrap().is_none());
    }
}

#[test]
fn replace_full() {
    let replace = vec![9, 2, 7, 1, 4, 6, 13];
    let present = vec![1, 6, 2];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::with_capacity(replace.len());

    for (i, &elt) in replace.iter().enumerate() {
        assert_eq!(set.len(), i);
        let (index, replaced) = set.replace_full(elt, &mut e, &mut g).unwrap();
        assert!(replaced.is_none());
        assert_eq!(
            Some(index),
            set.get_full(&elt, &mut e, &mut g).unwrap().map(|x| x.0)
        );
        assert_eq!(set.len(), i + 1);
    }

    let len = set.len();
    for &elt in &present {
        let (index, replaced) = set.replace_full(elt, &mut e, &mut g).unwrap();
        assert_eq!(Some(elt), replaced);
        assert_eq!(
            Some(index),
            set.get_full(&elt, &mut e, &mut g).unwrap().map(|x| x.0)
        );
        assert_eq!(set.len(), len);
    }
}

#[test]
fn replace_2() {
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::with_capacity(16);

    let mut values = vec![];
    values.extend(0..16);
    values.extend(if cfg!(miri) { 32..64 } else { 128..267 });

    for &i in &values {
        let old_set = set.clone();
        set.replace(i, &mut e, &mut g).unwrap();
        for value in old_set.iter() {
            if set.get(value, &mut e, &mut g).unwrap().is_none() {
                println!("old_set: {:?}", old_set);
                println!("set: {:?}", set);
                panic!("did not find {} in set", value);
            }
        }
    }

    for &i in &values {
        assert!(
            set.get(&i, &mut e, &mut g).unwrap().is_some(),
            "did not find {}",
            i
        );
    }
}

#[test]
fn replace_dup() {
    let mut elements = vec![0, 2, 4, 6, 8];
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<u8, E, G, ()> =
        RubySet::from_iter(elements.drain(..), &mut e, &mut g).unwrap();
    {
        let (i, v) = set.get_full(&0, &mut e, &mut g).unwrap().unwrap();
        assert_eq!(set.len(), 5);
        assert_eq!(i, 0);
        assert_eq!(*v, 0);
    }
    {
        let replaced = set.replace(0, &mut e, &mut g).unwrap();
        let (i, v) = set.get_full(&0, &mut e, &mut g).unwrap().unwrap();
        assert_eq!(set.len(), 5);
        assert_eq!(replaced, Some(0));
        assert_eq!(i, 0);
        assert_eq!(*v, 0);
    }
}

#[test]
fn replace_order() {
    let replace = [0, 4, 2, 12, 8, 7, 11, 5, 3, 17, 19, 22, 23];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::new();

    for &elt in &replace {
        set.replace(elt, &mut e, &mut g).unwrap();
    }

    assert_eq!(set.iter().count(), set.len());
    assert_eq!(set.iter().count(), replace.len());
    for (a, b) in replace.iter().zip(set.iter()) {
        assert_eq!(a, b);
    }
    for (i, v) in (0..replace.len()).zip(set.iter()) {
        assert_eq!(set.get_index(i).unwrap(), v);
    }
}

#[test]
fn replace_change() {
    // Check pointers to make sure it really changes
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<Vec<i32>, E, G, ()> = indexset!(&mut e; &mut g; vec![42]);
    let old_ptr = set.get_index(0).unwrap().as_ptr();
    let new = set.get_index(0).unwrap().clone();
    let new_ptr = new.as_ptr();
    assert_ne!(old_ptr, new_ptr);
    let replaced = set.replace(new, &mut e, &mut g).unwrap().unwrap();
    assert_eq!(replaced.as_ptr(), old_ptr);
}

#[test]
fn grow() {
    let insert = [0, 4, 2, 12, 8, 7, 11];
    let not_present = [1, 3, 6, 9, 10];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::with_capacity(insert.len());

    for (i, &elt) in insert.iter().enumerate() {
        assert_eq!(set.len(), i);
        set.insert(elt, &mut e, &mut g).unwrap();
        assert_eq!(set.len(), i + 1);
        assert_eq!(set.get(&elt, &mut e, &mut g).unwrap(), Some(&elt));
    }

    println!("{:?}", set);
    for &elt in &insert {
        set.insert(elt * 10, &mut e, &mut g).unwrap();
    }
    for &elt in &insert {
        set.insert(elt * 100, &mut e, &mut g).unwrap();
    }
    for (i, &elt) in insert.iter().cycle().enumerate().take(100) {
        set.insert(elt * 100 + i as i32, &mut e, &mut g).unwrap();
    }
    println!("{:?}", set);
    for &elt in &not_present {
        assert!(set.get(&elt, &mut e, &mut g).unwrap().is_none());
    }
}

#[test]
fn reserve() {
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<usize, E, G, ()>::new();
    assert_eq!(set.capacity(), 0);
    set.reserve(100);
    let capacity = set.capacity();
    assert!(capacity >= 100);
    for i in 0..capacity {
        assert_eq!(set.len(), i);
        set.insert(i, &mut e, &mut g).unwrap();
        assert_eq!(set.len(), i + 1);
        assert_eq!(set.capacity(), capacity);
        assert_eq!(set.get(&i, &mut e, &mut g).unwrap(), Some(&i));
    }
    set.insert(capacity, &mut e, &mut g).unwrap();
    assert_eq!(set.len(), capacity + 1);
    assert!(set.capacity() > capacity);
    assert_eq!(set.get(&capacity, &mut e, &mut g).unwrap(), Some(&capacity));
}

#[test]
fn shrink_to_fit() {
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<usize, E, G, ()>::new();
    assert_eq!(set.capacity(), 0);
    for i in 0..100 {
        assert_eq!(set.len(), i);
        set.insert(i, &mut e, &mut g).unwrap();
        assert_eq!(set.len(), i + 1);
        assert!(set.capacity() >= i + 1);
        assert_eq!(set.get(&i, &mut e, &mut g).unwrap(), Some(&i));
        set.shrink_to_fit();
        assert_eq!(set.len(), i + 1);
        assert_eq!(set.capacity(), i + 1);
        assert_eq!(set.get(&i, &mut e, &mut g).unwrap(), Some(&i));
    }
}

#[test]
fn remove() {
    let insert = [0, 4, 2, 12, 8, 7, 11, 5, 3, 17, 19, 22, 23];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::new();

    for &elt in &insert {
        set.insert(elt, &mut e, &mut g).unwrap();
    }

    assert_eq!(set.iter().count(), set.len());
    assert_eq!(set.iter().count(), insert.len());
    for (a, b) in insert.iter().zip(set.iter()) {
        assert_eq!(a, b);
    }

    let remove_fail = [99, 77];
    let remove = [4, 12, 8, 7];

    for &value in &remove_fail {
        assert!(set
            .swap_remove_full(&value, &mut e, &mut g)
            .unwrap()
            .is_none());
    }
    println!("{:?}", set);
    for &value in &remove {
        //println!("{:?}", set);
        let index = set.get_full(&value, &mut e, &mut g).unwrap().unwrap().0;
        assert_eq!(
            set.swap_remove_full(&value, &mut e, &mut g).unwrap(),
            Some((index, value))
        );
    }
    println!("{:?}", set);

    for value in &insert {
        assert_eq!(
            set.get(value, &mut e, &mut g).unwrap().is_some(),
            !remove.contains(value)
        );
    }
    assert_eq!(set.len(), insert.len() - remove.len());
    assert_eq!(set.iter().count(), insert.len() - remove.len());
}

#[test]
fn swap_remove_index() {
    let insert = [0, 4, 2, 12, 8, 7, 11, 5, 3, 17, 19, 22, 23];
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::new();

    for &elt in &insert {
        set.insert(elt, &mut e, &mut g).unwrap();
    }

    let mut vector = insert.to_vec();
    let remove_sequence = &[3, 3, 10, 4, 5, 4, 3, 0, 1];

    // check that the same swap remove sequence on vec and set
    // have the same result.
    for &rm in remove_sequence {
        let out_vec = vector.swap_remove(rm);
        let out_set = set.swap_remove_index(rm, &mut e, &mut g).unwrap().unwrap();
        assert_eq!(out_vec, out_set);
    }
    assert_eq!(vector.len(), set.len());
    for (a, b) in vector.iter().zip(set.iter()) {
        assert_eq!(a, b);
    }
}

#[test]
fn partial_eq_and_eq() {
    let mut e = E;
    let mut g = G;
    let mut set_a = RubySet::new();
    set_a.insert(1, &mut e, &mut g).unwrap();
    set_a.insert(2, &mut e, &mut g).unwrap();
    let mut set_b = set_a.clone();
    assert!(set_a.eql(&set_b, &mut e, &mut g).unwrap());
    set_b.swap_remove(&1, &mut e, &mut g).unwrap();
    assert!(!set_a.eql(&set_b, &mut e, &mut g).unwrap());

    let set_c = RubySet::<i32, E, G, ()>::from_iter(set_b.into_iter(), &mut e, &mut g).unwrap();
    assert!(!set_a.eql(&set_c, &mut e, &mut g).unwrap());
    assert!(!set_c.eql(&set_a, &mut e, &mut g).unwrap());
}

#[test]
fn extend() {
    let mut e = E;
    let mut g = G;
    let mut set = RubySet::<i32, E, G, ()>::new();
    set.extend(vec![1, 2, 3, 4], &mut e, &mut g).unwrap();
    set.extend(vec![5, 6], &mut e, &mut g).unwrap();
    assert_eq!(set.into_iter().collect::<Vec<_>>(), vec![1, 2, 3, 4, 5, 6]);
}

#[test]
fn comparisons() {
    let mut e = E;
    let mut g = G;
    let set_a: RubySet<i32, E, G, ()> = RubySet::from_iter(0..3, &mut e, &mut g).unwrap();
    let set_b: RubySet<i32, E, G, ()> = RubySet::from_iter(3..6, &mut e, &mut g).unwrap();
    let set_c: RubySet<i32, E, G, ()> = RubySet::from_iter(0..6, &mut e, &mut g).unwrap();
    let set_d: RubySet<i32, E, G, ()> = RubySet::from_iter(3..9, &mut e, &mut g).unwrap();

    assert!(!set_a.is_disjoint(&set_a, &mut e, &mut g).unwrap());
    assert!(set_a.is_subset(&set_a, &mut e, &mut g).unwrap());
    assert!(set_a.is_superset(&set_a, &mut e, &mut g).unwrap());

    assert!(set_a.is_disjoint(&set_b, &mut e, &mut g).unwrap());
    assert!(set_b.is_disjoint(&set_a, &mut e, &mut g).unwrap());
    assert!(!set_a.is_subset(&set_b, &mut e, &mut g).unwrap());
    assert!(!set_b.is_subset(&set_a, &mut e, &mut g).unwrap());
    assert!(!set_a.is_superset(&set_b, &mut e, &mut g).unwrap());
    assert!(!set_b.is_superset(&set_a, &mut e, &mut g).unwrap());

    assert!(!set_a.is_disjoint(&set_c, &mut e, &mut g).unwrap());
    assert!(!set_c.is_disjoint(&set_a, &mut e, &mut g).unwrap());
    assert!(set_a.is_subset(&set_c, &mut e, &mut g).unwrap());
    assert!(!set_c.is_subset(&set_a, &mut e, &mut g).unwrap());
    assert!(!set_a.is_superset(&set_c, &mut e, &mut g).unwrap());
    assert!(set_c.is_superset(&set_a, &mut e, &mut g).unwrap());

    assert!(!set_c.is_disjoint(&set_d, &mut e, &mut g).unwrap());
    assert!(!set_d.is_disjoint(&set_c, &mut e, &mut g).unwrap());
    assert!(!set_c.is_subset(&set_d, &mut e, &mut g).unwrap());
    assert!(!set_d.is_subset(&set_c, &mut e, &mut g).unwrap());
    assert!(!set_c.is_superset(&set_d, &mut e, &mut g).unwrap());
    assert!(!set_d.is_superset(&set_c, &mut e, &mut g).unwrap());
}

#[test]
fn from_array() {
    let mut e = E;
    let mut g = G;
    let set1 = RubySet::<i32, E, G, ()>::from_ary([1, 2, 3, 4], &mut e, &mut g).unwrap();
    let set2 = RubySet::<i32, E, G, ()>::from_iter([1, 2, 3, 4], &mut e, &mut g).unwrap();

    assert!(set1.eql(&set2, &mut e, &mut g).unwrap());
}

#[test]
fn iter_default() {
    struct Item;
    fn assert_default<T>()
    where
        T: Default + Iterator,
    {
        assert!(T::default().next().is_none());
    }
    assert_default::<Iter<'static, Item>>();
    assert_default::<IntoIter<Item>>();
}

#[test]
fn swap_take() {
    let mut e = E;
    let mut g = G;
    let mut index_set: RubySet<i32, E, G, ()> = RubySet::new();
    index_set.insert(10, &mut e, &mut g).unwrap();
    index_set.insert(20, &mut e, &mut g).unwrap();
    index_set.insert(30, &mut e, &mut g).unwrap();
    index_set.insert(40, &mut e, &mut g).unwrap();
    assert_eq!(index_set.len(), 4);

    let result = index_set.swap_take(&20, &mut e, &mut g).unwrap();
    assert_eq!(result, Some(20));
    assert_eq!(index_set.len(), 3);
    assert_eq!(index_set.as_slice(), &[10, 40, 30]);

    let result = index_set.swap_take(&50, &mut e, &mut g).unwrap();
    assert_eq!(result, None);
}

#[test]
fn sort_unstable() {
    let mut e = E;
    let mut g = G;
    let mut index_set: RubySet<i32, E, G, ()> = RubySet::new();
    index_set.insert(30, &mut e, &mut g).unwrap();
    index_set.insert(20, &mut e, &mut g).unwrap();
    index_set.insert(10, &mut e, &mut g).unwrap();

    index_set.sort_unstable();
    assert_eq!(index_set.as_slice(), &[10, 20, 30]);
}

#[test]
fn shift_remove_full() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(10, &mut e, &mut g).unwrap();
    set.insert(20, &mut e, &mut g).unwrap();
    set.insert(30, &mut e, &mut g).unwrap();
    set.insert(40, &mut e, &mut g).unwrap();
    set.insert(50, &mut e, &mut g).unwrap();

    let result = set.shift_remove_full(&20, &mut e, &mut g).unwrap();
    assert_eq!(result, Some((1, 20)));
    assert_eq!(set.len(), 4);
    assert_eq!(set.as_slice(), &[10, 30, 40, 50]);

    let result = set.shift_remove_full(&50, &mut e, &mut g).unwrap();
    assert_eq!(result, Some((3, 50)));
    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[10, 30, 40]);

    let result = set.shift_remove_full(&60, &mut e, &mut g).unwrap();
    assert_eq!(result, None);
    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[10, 30, 40]);
}

#[test]
fn shift_remove_index() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(10, &mut e, &mut g).unwrap();
    set.insert(20, &mut e, &mut g).unwrap();
    set.insert(30, &mut e, &mut g).unwrap();
    set.insert(40, &mut e, &mut g).unwrap();
    set.insert(50, &mut e, &mut g).unwrap();

    let result = set.shift_remove_index(1, &mut e, &mut g).unwrap();
    assert_eq!(result, Some(20));
    assert_eq!(set.len(), 4);
    assert_eq!(set.as_slice(), &[10, 30, 40, 50]);

    let result = set.shift_remove_index(1, &mut e, &mut g).unwrap();
    assert_eq!(result, Some(30));
    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[10, 40, 50]);

    let result = set.shift_remove_index(3, &mut e, &mut g).unwrap();
    assert_eq!(result, None);
    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[10, 40, 50]);
}

#[test]
fn sort_unstable_by() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], &mut e, &mut g).unwrap();
    set.sort_unstable_by(|a, b| b.cmp(a));
    assert_eq!(set.as_slice(), &[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
}

#[test]
fn sort_by() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(3, &mut e, &mut g).unwrap();
    set.insert(1, &mut e, &mut g).unwrap();
    set.insert(2, &mut e, &mut g).unwrap();
    set.sort_by(|a, b| a.cmp(b));
    assert_eq!(set.as_slice(), &[1, 2, 3]);
}

#[test]
fn drain() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(1, &mut e, &mut g).unwrap();
    set.insert(2, &mut e, &mut g).unwrap();
    set.insert(3, &mut e, &mut g).unwrap();

    {
        let drain = set.drain(0..2, &mut e, &mut g).unwrap();
        assert_eq!(drain.as_slice(), &[1, 2]);
    }

    assert_eq!(set.len(), 1);
    assert_eq!(set.as_slice(), &[3]);
}

#[test]
fn split_off() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 3, 4, 5], &mut e, &mut g).unwrap();
    let split_set: RubySet<i32, E, G, ()> = set.split_off(3, &mut e, &mut g).unwrap();

    assert_eq!(split_set.len(), 2);
    assert_eq!(split_set.as_slice(), &[4, 5]);

    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[1, 2, 3]);
}

#[test]
fn retain() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], &mut e, &mut g).unwrap();
    set.retain(|&x| x > 4);
    assert_eq!(set.len(), 6);
    assert_eq!(set.as_slice(), &[5, 6, 7, 8, 9, 10]);

    set.retain(|_| false);
    assert_eq!(set.len(), 0);
}

#[test]
fn first() {
    let mut e = E;
    let mut g = G;
    let mut index_set: RubySet<i32, E, G, ()> = RubySet::new();
    index_set.insert(10, &mut e, &mut g).unwrap();
    index_set.insert(20, &mut e, &mut g).unwrap();
    index_set.insert(30, &mut e, &mut g).unwrap();

    let result = index_set.first();
    assert_eq!(*result.unwrap(), 10);

    index_set.clear();
    let result = index_set.first();
    assert!(result.is_none());
}

#[test]
fn sort_by_cached_key() {
    let mut e = E;
    let mut g = G;
    let mut index_set: RubySet<i32, E, G, ()> = RubySet::new();
    index_set.insert(3, &mut e, &mut g).unwrap();
    index_set.insert(1, &mut e, &mut g).unwrap();
    index_set.insert(2, &mut e, &mut g).unwrap();
    index_set.insert(0, &mut e, &mut g).unwrap();
    index_set.sort_by_cached_key(|&x| -x);
    assert_eq!(index_set.as_slice(), &[3, 2, 1, 0]);
}

#[test]
fn insert_sorted() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert_sorted(1, &mut e, &mut g).unwrap();
    set.insert_sorted(3, &mut e, &mut g).unwrap();
    assert_eq!(set.insert_sorted(2, &mut e, &mut g).unwrap(), (1, true));
}

#[test]
fn binary_search() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(100, &mut e, &mut g).unwrap();
    set.insert(300, &mut e, &mut g).unwrap();
    set.insert(200, &mut e, &mut g).unwrap();
    set.insert(400, &mut e, &mut g).unwrap();
    let result = set.binary_search(&200);
    assert_eq!(result, Ok(2));

    let result = set.binary_search(&500);
    assert_eq!(result, Err(4));
}

#[test]
fn sorted_unstable_by() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], &mut e, &mut g).unwrap();
    set.sort_unstable_by(|a, b| b.cmp(a));
    assert_eq!(set.as_slice(), &[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
}

#[test]
fn last() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(1, &mut e, &mut g).unwrap();
    set.insert(2, &mut e, &mut g).unwrap();
    set.insert(3, &mut e, &mut g).unwrap();
    set.insert(4, &mut e, &mut g).unwrap();
    set.insert(5, &mut e, &mut g).unwrap();
    set.insert(6, &mut e, &mut g).unwrap();

    assert_eq!(set.last(), Some(&6));

    set.pop(&mut e, &mut g).unwrap();
    assert_eq!(set.last(), Some(&5));

    set.clear();
    assert_eq!(set.last(), None);
}

#[test]
fn get_range() {
    let mut e = E;
    let mut g = G;
    let set: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 2, 3, 4, 5], &mut e, &mut g).unwrap();
    let result = set.get_range(0..3);
    let slice: &Slice<i32> = result.unwrap();
    assert_eq!(slice, &[1, 2, 3]);

    let result = set.get_range(0..0);
    assert_eq!(result.unwrap().len(), 0);

    let result = set.get_range(2..1);
    assert!(result.is_none());
}

#[test]
fn shift_take() {
    let mut e = E;
    let mut g = G;
    let mut set: RubySet<i32, E, G, ()> = RubySet::new();
    set.insert(1, &mut e, &mut g).unwrap();
    set.insert(2, &mut e, &mut g).unwrap();
    set.insert(3, &mut e, &mut g).unwrap();
    set.insert(4, &mut e, &mut g).unwrap();
    set.insert(5, &mut e, &mut g).unwrap();

    let result = set.shift_take(&2, &mut e, &mut g).unwrap();
    assert_eq!(result, Some(2));
    assert_eq!(set.len(), 4);
    assert_eq!(set.as_slice(), &[1, 3, 4, 5]);

    let result = set.shift_take(&5, &mut e, &mut g).unwrap();
    assert_eq!(result, Some(5));
    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[1, 3, 4]);

    let result = set.shift_take(&5, &mut e, &mut g).unwrap();
    assert_eq!(result, None);
    assert_eq!(set.len(), 3);
    assert_eq!(set.as_slice(), &[1, 3, 4]);
}

#[test]
fn test_binary_search_by() {
    // adapted from std's test for binary_search
    let mut e = E;
    let mut g = G;
    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&5)), Err(0));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([4], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&3)), Err(0));
    assert_eq!(b.binary_search_by(|x| x.cmp(&4)), Ok(0));
    assert_eq!(b.binary_search_by(|x| x.cmp(&5)), Err(1));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 2, 4, 6, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&5)), Err(3));
    assert_eq!(b.binary_search_by(|x| x.cmp(&6)), Ok(3));
    assert_eq!(b.binary_search_by(|x| x.cmp(&7)), Err(4));
    assert_eq!(b.binary_search_by(|x| x.cmp(&8)), Ok(4));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 2, 4, 5, 6, 8], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&9)), Err(6));

    let b: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 4, 6, 7, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&6)), Ok(3));
    assert_eq!(b.binary_search_by(|x| x.cmp(&5)), Err(3));
    assert_eq!(b.binary_search_by(|x| x.cmp(&8)), Ok(5));

    let b: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 4, 5, 6, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&7)), Err(5));
    assert_eq!(b.binary_search_by(|x| x.cmp(&0)), Err(0));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 3, 3, 3, 7], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by(|x| x.cmp(&0)), Err(0));
    assert_eq!(b.binary_search_by(|x| x.cmp(&1)), Ok(0));
    assert_eq!(b.binary_search_by(|x| x.cmp(&2)), Err(1));
    // diff from std as set merges the duplicate keys
    assert!(match b.binary_search_by(|x| x.cmp(&3)) {
        Ok(1..=2) => true,
        _ => false,
    });
    assert!(match b.binary_search_by(|x| x.cmp(&3)) {
        Ok(1..=2) => true,
        _ => false,
    });
    assert_eq!(b.binary_search_by(|x| x.cmp(&4)), Err(2));
    assert_eq!(b.binary_search_by(|x| x.cmp(&5)), Err(2));
    assert_eq!(b.binary_search_by(|x| x.cmp(&6)), Err(2));
    assert_eq!(b.binary_search_by(|x| x.cmp(&7)), Ok(2));
    assert_eq!(b.binary_search_by(|x| x.cmp(&8)), Err(3));
}

#[test]
fn test_binary_search_by_key() {
    // adapted from std's test for binary_search
    let mut e = E;
    let mut g = G;
    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&5, |&x| x), Err(0));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([4], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&3, |&x| x), Err(0));
    assert_eq!(b.binary_search_by_key(&4, |&x| x), Ok(0));
    assert_eq!(b.binary_search_by_key(&5, |&x| x), Err(1));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 2, 4, 6, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&5, |&x| x), Err(3));
    assert_eq!(b.binary_search_by_key(&6, |&x| x), Ok(3));
    assert_eq!(b.binary_search_by_key(&7, |&x| x), Err(4));
    assert_eq!(b.binary_search_by_key(&8, |&x| x), Ok(4));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 2, 4, 5, 6, 8], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&9, |&x| x), Err(6));

    let b: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 4, 6, 7, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&6, |&x| x), Ok(3));
    assert_eq!(b.binary_search_by_key(&5, |&x| x), Err(3));
    assert_eq!(b.binary_search_by_key(&8, |&x| x), Ok(5));

    let b: RubySet<i32, E, G, ()> =
        RubySet::from_ary([1, 2, 4, 5, 6, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&7, |&x| x), Err(5));
    assert_eq!(b.binary_search_by_key(&0, |&x| x), Err(0));

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([1, 3, 3, 3, 7], &mut e, &mut g).unwrap();
    assert_eq!(b.binary_search_by_key(&0, |&x| x), Err(0));
    assert_eq!(b.binary_search_by_key(&1, |&x| x), Ok(0));
    assert_eq!(b.binary_search_by_key(&2, |&x| x), Err(1));
    // diff from std as set merges the duplicate keys
    assert!(match b.binary_search_by_key(&3, |&x| x) {
        Ok(1..=2) => true,
        _ => false,
    });
    assert!(match b.binary_search_by_key(&3, |&x| x) {
        Ok(1..=2) => true,
        _ => false,
    });
    assert_eq!(b.binary_search_by_key(&4, |&x| x), Err(2));
    assert_eq!(b.binary_search_by_key(&5, |&x| x), Err(2));
    assert_eq!(b.binary_search_by_key(&6, |&x| x), Err(2));
    assert_eq!(b.binary_search_by_key(&7, |&x| x), Ok(2));
    assert_eq!(b.binary_search_by_key(&8, |&x| x), Err(3));
}

#[test]
fn test_partition_point() {
    // adapted from std's test for partition_point
    let mut e = E;
    let mut g = G;
    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 5), 0);

    let b: RubySet<i32, E, G, ()> = RubySet::from_ary([4], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 3), 0);
    assert_eq!(b.partition_point(|&x| x < 4), 0);
    assert_eq!(b.partition_point(|&x| x < 5), 1);

    let b: RubySet<i32, E, G, ()> = RubySet::from_iter([1, 2, 4, 6, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 5), 3);
    assert_eq!(b.partition_point(|&x| x < 6), 3);
    assert_eq!(b.partition_point(|&x| x < 7), 4);
    assert_eq!(b.partition_point(|&x| x < 8), 4);

    let b: RubySet<i32, E, G, ()> = RubySet::from_iter([1, 2, 4, 5, 6, 8], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 9), 6);

    let b: RubySet<i32, E, G, ()> =
        RubySet::from_iter([1, 2, 4, 6, 7, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 6), 3);
    assert_eq!(b.partition_point(|&x| x < 5), 3);
    assert_eq!(b.partition_point(|&x| x < 8), 5);

    let b: RubySet<i32, E, G, ()> =
        RubySet::from_iter([1, 2, 4, 5, 6, 8, 9], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 7), 5);
    assert_eq!(b.partition_point(|&x| x < 0), 0);

    let b: RubySet<i32, E, G, ()> = RubySet::from_iter([1, 3, 3, 3, 7], &mut e, &mut g).unwrap();
    assert_eq!(b.partition_point(|&x| x < 0), 0);
    assert_eq!(b.partition_point(|&x| x < 1), 0);
    assert_eq!(b.partition_point(|&x| x < 2), 1);
    assert_eq!(b.partition_point(|&x| x < 3), 1);
    assert_eq!(b.partition_point(|&x| x < 4), 2); // diff from std as set merges the duplicate keys
    assert_eq!(b.partition_point(|&x| x < 5), 2);
    assert_eq!(b.partition_point(|&x| x < 6), 2);
    assert_eq!(b.partition_point(|&x| x < 7), 2);
    assert_eq!(b.partition_point(|&x| x < 8), 3);
}
