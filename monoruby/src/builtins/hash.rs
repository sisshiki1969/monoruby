use indexmap::IndexMap;

use super::*;

//
// Hash class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Hash", HASH_CLASS);
    globals.define_builtin_class_func(HASH_CLASS, "new", new, 0);

    globals.define_builtin_funcs(HASH_CLASS, "==", &["===", "eql?"], eq, 1);
    globals.define_builtin_func(HASH_CLASS, "[]", index, 1);
    globals.define_builtin_func(HASH_CLASS, "[]=", index_assign, 2);
    globals.define_builtin_func(HASH_CLASS, "clear", clear, 0);
    globals.define_builtin_func(HASH_CLASS, "compare_by_identity", compare_by_identity, 0);
    globals.define_builtin_func(HASH_CLASS, "delete", delete, 1);
    globals.define_builtin_funcs(HASH_CLASS, "each", &["each_pair"], each, 0);
    globals.define_builtin_func(HASH_CLASS, "each_key", each_key, 0);
    globals.define_builtin_func(HASH_CLASS, "each_value", each_value, 0);
    globals.define_builtin_funcs(HASH_CLASS, "select", &["filter"], select, 0);
    globals.define_builtin_func(HASH_CLASS, "empty?", empty_, 0);
    globals.define_builtin_func_with(HASH_CLASS, "fetch", fetch, 1, 2, false);
    globals.define_builtin_funcs(
        HASH_CLASS,
        "include?",
        &["has_key?", "key?", "member?"],
        include,
        1,
    );
    globals.define_builtin_funcs(HASH_CLASS, "inspect", &["to_s"], inspect, 0);
    globals.define_builtin_func(HASH_CLASS, "invert", invert, 0);
    globals.define_builtin_func(HASH_CLASS, "keys", keys, 0);
    globals.define_builtin_func_rest(HASH_CLASS, "merge", merge);
    globals.define_builtin_funcs_rest(HASH_CLASS, "merge!", &["update"], merge_);
    globals.define_builtin_funcs(HASH_CLASS, "size", &["length"], size, 0);
    globals.define_builtin_func(HASH_CLASS, "reject", reject, 0);
    globals.define_builtin_func(HASH_CLASS, "sort", sort, 0);
    globals.define_builtin_func(HASH_CLASS, "store", index_assign, 2);
    globals.define_builtin_func(HASH_CLASS, "values", values, 0);

    let mut env_map = IndexMap::default();
    std::env::vars().for_each(|(var, val)| {
        env_map.insert(HashKey(Value::string(var)), Value::string(val));
    });
    #[cfg(windows)]
    if let None = env_map.get(&Value::string("HOME")) {
        let home_drive = env_map.get(&Value::string("HOMEDRIVE"));
        let home_path = env_map.get(&Value::string("HOMEPATH"));
        let user_profile = env_map.get(&Value::string("USERPROFILE"));
        let home = if home_drive.is_some() && home_drive.is_some() {
            home_drive.unwrap().as_string().unwrap().to_string()
                + home_path.unwrap().as_string().unwrap()
        } else if let Some(up) = user_profile {
            up.as_string().unwrap().to_string()
        } else {
            "".to_string()
        };
        env_map.insert(
            Value::string("HOME"),
            Value::string(home.replace('\\', "/")),
        );
    };

    let env = Value::hash(env_map);
    globals.set_constant_by_str(OBJECT_CLASS, "ENV", env);
    globals.define_builtin_singleton_func_with(env, "fetch", fetch, 1, 2, false);
    globals.define_builtin_singleton_func(env, "[]", env_index, 1);
}

///
/// ### Hash.new
///
/// - new(ifnone = nil) -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let default_proc = if let Some(bh) = lfp.block() {
        Some(vm.generate_proc(globals, bh)?)
    } else {
        None
    };
    let obj = Value::empty_hash_with_class(class, default_proc);
    Ok(obj)
}

///
/// ### Hash#length
///
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let len = lfp.self_val().as_hashmap_inner().len();
    Ok(Value::integer(len as i64))
}

///
/// ### Hash#==
///
/// - self == other -> bool
/// - self === other -> bool
/// - self.eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let lhs_v = lfp.self_val();
    let rhs_v = lfp.arg(0);
    let lhs = lhs_v.as_hashmap_inner();
    let rhs = if let Some(rhs) = rhs_v.is_hash() {
        rhs
    } else {
        return Ok(Value::bool(false));
    };
    if lhs.len() != rhs.len() {
        return Ok(Value::bool(false));
    }
    for (k, lhs_value) in lhs.iter() {
        if let Some(rhs_value) = rhs.get(k)
            && vm.eq_values_bool(globals, lhs_value, rhs_value)?
        {
            continue;
        } else {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

///
/// ### Hash#[]=
///
/// - self[key] = value
/// - store(key, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let key = lfp.arg(0);
    let val = lfp.arg(1);
    lfp.self_val().as_hashmap_inner_mut().insert(key, val);
    Ok(val)
}

///
/// ### Hash#[]
///
/// - self[key] -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d.html]
#[monoruby_builtin]
fn index(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let key = lfp.arg(0);
    let h = Hashmap::new(lfp.self_val());
    h.index(vm, globals, key)
}

///
/// ### Hash#clear
///
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
#[monoruby_builtin]
fn clear(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_hashmap_inner_mut().clear();
    Ok(self_)
}

///
/// ### Hash#keys
///
/// - keys -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keys.html]
#[monoruby_builtin]
fn keys(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let keys = lfp.self_val().as_hashmap_inner().keys();
    Ok(Value::array_from_vec(keys))
}

///
/// ### Hash#values
///
/// - values -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/values.html]
#[monoruby_builtin]
fn values(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let keys = lfp.self_val().as_hashmap_inner().values();
    Ok(Value::array_from_vec(keys))
}

///
/// ### Hash#delete
///
/// - delete(key) -> object | nil
/// - delete(key) {|key| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/delete.html]
#[monoruby_builtin]
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut h = lfp.self_val().is_hash().unwrap();
    let key = lfp.arg(0);
    let removed_value = h.remove(key);
    if removed_value.is_none()
        && let Some(bh) = lfp.block()
    {
        return vm.invoke_block_once(globals, bh, &[key]);
    }

    Ok(removed_value.unwrap_or_default())
}

///
/// ### Hash#each
///
/// - each {|key, value| ... } -> self
/// - each_pair {|key, value| ... } -> self
/// - each -> Enumerator
/// - each_pair -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("each");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let ary = lfp.self_val();
    let data = vm.get_block_data(globals, bh)?;
    for (k, v) in ary.as_hashmap_inner().iter() {
        vm.invoke_block(globals, &data, &[k, v])?;
    }
    Ok(lfp.self_val())
}

///
/// ### Hash#each_value
///
/// - each_value {|value| ... } -> self
/// - each_value -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each_value.html]
#[monoruby_builtin]
fn each_value(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("each_value");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let ary = lfp.self_val();
    let iter = ary.as_hashmap_inner().iter().map(|(_, v)| v);
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

///
/// ### Hash#each_key
///
/// - each_key {|value| ... } -> self
/// - each_key -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each_key.html]
#[monoruby_builtin]
fn each_key(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("each_key");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let ary = lfp.self_val();
    let iter = ary.as_hashmap_inner().iter().map(|(k, _)| k);
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

///
/// ### Hash#select
///
/// - select -> Enumerator
/// - select {|key, value| ... } -> Hash
/// - filter -> Enumerator
/// - filter {|key, value| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/filter.html]
#[monoruby_builtin]
fn select(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = match lfp.block() {
        None => {
            let id = IdentId::get_id("select");
            return vm.generate_enumerator(id, lfp.self_val(), lfp.iter().collect());
        }
        Some(block) => block,
    };
    let data = vm.get_block_data(globals, bh)?;
    let mut inner = HashmapInner::new(IndexMap::default(), None);
    for (k, v) in lfp.self_val().as_hashmap_inner().iter() {
        if vm.invoke_block(globals, &data, &[k, v])?.as_bool() {
            inner.insert(k, v);
        }
    }
    Ok(Value::hash_from_inner(inner))
}

///
/// ### Hash#empty?
///
/// - empty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/empty=3f.html]
#[monoruby_builtin]
fn empty_(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let self_ = lfp.self_val();
    let b = self_.as_hashmap_inner().is_empty();
    Ok(Value::bool(b))
}

///
/// ### Hash#has_key?
///
/// - has_key?(key) -> bool
/// - include?(key) -> bool
/// - key?(key) -> bool
/// - member?(key) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/has_key=3f.html]
#[monoruby_builtin]
fn include(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = lfp.self_val().as_hashmap_inner().contains_key(lfp.arg(0));
    Ok(Value::bool(b))
}

///
/// ### Hash#inspect
///
/// - to_s -> String
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/inspect.html]
#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let s = lfp.self_val().as_hashmap_inner().to_s(globals);
    Ok(Value::string(s))
}

///
/// ### Hash#reject
///
/// - reject {|key, value| ... } -> Hash
/// - [NOT SUPPORTED] reject -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/reject.html]
#[monoruby_builtin]
fn reject(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let h = lfp.self_val().dup();
    let p = vm.get_block_data(globals, bh)?;
    vm.temp_push(h);
    let mut res = Hashmap::new(h);
    for (k, v) in lfp.self_val().expect_hash()?.iter() {
        if vm.invoke_block(globals, &p, &[k, v])?.as_bool() {
            res.remove(k);
        }
    }
    let h = vm.temp_pop();
    Ok(h)
}

///
/// ### Enumerable#sort
///
/// - sort -> [object]
/// - [NOT SUPPORTED] sort {|a, b| ... } -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/sort.html]
#[monoruby_builtin]
fn sort(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let self_val = lfp.self_val();
    let inner = self_val.as_hashmap_inner();
    let mut ary = inner.keys();
    vm.sort(globals, &mut ary)?;
    let res: Vec<_> = ary
        .into_iter()
        .map(|k| Value::array2(k, inner.get(k).unwrap()))
        .collect();
    Ok(Value::array_from_vec(res))
}

///
/// ### Hash#invert
///
/// - invert -> Hash
///
/// [https://docs.ruby-lang.org/ja/3.2/method/Hash/i/invert.html]
#[monoruby_builtin]
fn invert(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let self_val = lfp.self_val();
    let inner = self_val.as_hashmap_inner();
    let mut map = IndexMap::default();
    for (k, v) in inner.iter() {
        map.insert(HashKey(v), k);
    }
    Ok(Value::hash(map))
}

///
/// ### Hash#merge
///
/// - merge(*others) -> Hash
/// - [NOT SUPPORTED]merge(*others) {|key, self_val, other_val| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/merge.html]
#[monoruby_builtin]
fn merge(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut h = lfp.self_val().dup().expect_hash()?;
    for arg in lfp.arg(0).as_array().iter() {
        let other = arg.expect_hash()?;
        for (k, v) in other.iter() {
            h.insert(k, v);
        }
    }

    Ok(h.into())
}

///
/// ### Hash#merge!
///
/// - merge!(*others) -> self
/// - [NOT SUPPORTED]merge!(*others) {|key, self_val, other_val| ... } -> self
/// - update(*others) -> self
/// - [NOT SUPPORTED]update(*others) {|key, self_val, other_val| ... } -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/merge=21.html]
#[monoruby_builtin]
fn merge_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut h = lfp.self_val().expect_hash()?;
    for arg in lfp.arg(0).as_array().iter() {
        let other = arg.expect_hash()?;
        for (k, v) in other.iter() {
            h.insert(k, v);
        }
    }

    Ok(lfp.self_val())
}

///
/// ### Hash#compare_by_identity
///
/// - compare_by_identity -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/compare_by_identity.html]
#[monoruby_builtin]
fn compare_by_identity(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let mut self_val = lfp.self_val();
    self_val.as_hashmap_inner_mut().compare_by_identity();
    Ok(lfp.self_val())
}

// ENV object

/// ###ENV.[]
/// - self[key] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d.html]
#[monoruby_builtin]
fn env_index(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let key = lfp.arg(0);
    if key.is_str().is_none() {
        return Err(MonorubyErr::no_implicit_conversion(key, STRING_CLASS));
    }
    let val = lfp
        .self_val()
        .as_hashmap_inner()
        .get(key)
        .unwrap_or_default();
    Ok(val)
}

///
/// ### Hash#fetch
///
/// - fetch(key) -> object
/// - fetch(key, default) -> object
/// - fetch(key) {|key| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/fetch.html]
#[monoruby_builtin]
fn fetch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let map = self_.as_hashmap_inner();
    let s = if let Some(bh) = lfp.block() {
        if lfp.try_arg(1).is_some() {
            eprintln!("warning: block supersedes default value argument");
        }
        match map.get(lfp.arg(0)) {
            Some(v) => v,
            None => vm.invoke_block_once(globals, bh, &[lfp.arg(0)])?,
        }
    } else if lfp.try_arg(1).is_none() {
        match map.get(lfp.arg(0)) {
            Some(v) => v,
            None => {
                return Err(MonorubyErr::keyerr(format!(
                    "key not found: {}",
                    lfp.arg(0).to_s(globals)
                )))
            }
        }
    } else {
        match map.get(lfp.arg(0)) {
            Some(v) => v,
            None => lfp.arg(1),
        }
    };
    Ok(s)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn test_hash() {
        run_test(
            r##"
        a = []
        h = Hash.new
        a << h.length
        h["Ruby"] = :ruby
        h[5] = 4.2
        h[:u] = "Ruby"
        a << h.length
        a << h[5]
        a << h[5.0]
        a << h["Ruby"]
        a << h.size
        a << h.keys
        a << h.values
        a << h.has_key?("ruby")
        a << h.include?(5.0)
        a << h.key?(5)
        a << h.member?(:u)
        a << h.inspect
        a << h.to_s
        a
        "##,
        );
        run_test("{}");
        run_test(r#"{1=>:ass, 4.5=>"Ruby", [1,2,3]=>{:f=>6}}"#);
        run_test("{}.empty?");
        run_test("{a:1}.empty?");
    }

    #[test]
    fn eq() {
        run_test(r##"{} == {}"##);
        run_test(r##"{a:4} == {a:4}"##);
        run_test(r##"{a:4} == {a:4.0}"##);
        run_test(r##"{a:4} == {a:5}"##);
        run_test(r##"{a:4} == {a:5, b:7}"##);
        run_test(r##"{a:4} == :a"##);
    }

    #[test]
    fn fetch() {
        run_test(
            r##"
        h = { one: nil }
        [h.fetch(:one), h.fetch(:two, "error")]
        "##,
        );
        run_test_error(
            r##"
        h = { one: nil }
        h.fetch(:two)
        "##,
        );
    }

    #[test]
    fn delete() {
        run_test(
            r##"
        a = []
        h = {:ab => "some" , :cd => "all"}
        a << h.delete(:ab) #=> "some"
        a << h.delete(:ef) #=> nil
        a << h.delete(:ef){|key|"#{key} Nothing"} #=> "ef Nothing"
        a
        "##,
        );
    }

    #[test]
    fn each() {
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each {|k, v|
            a << k
            a << v
        }
        a
        "##,
        );
    }

    #[test]
    fn each_value() {
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each_value {|v|
            a << v
        }
        a
        "##,
        );
        run_test(
            r##"
        a = []
        {:a=>1, :b=>2, :c=>3}.each_key {|k|
            a << k
        }
        a
        "##,
        );
    }

    #[test]
    fn select() {
        run_test(
            r##"
        res = []
        h = { "a" => 100, "b" => 200, "c" => 300 }
        res << h.select {|k,v| k > "a"}  #=> {"b" => 200, "c" => 300}
        res << h.select {|k,v| v < 200}  #=> {"a" => 100}
        res
        "##,
        );
    }

    #[test]
    fn invert() {
        run_test(
            r##"
        {5 => "5", 1 => "1", 2 => "2", 3 => "3"}.invert
        "##,
        );
    }

    #[test]
    fn sort() {
        run_test(
            r##"
        {5 => "5", 1 => "1", 2 => "2", 3 => "3"}.sort
        "##,
        );
    }

    #[test]
    fn reject() {
        run_test(
            r##"
        h = { 2 =>"8", 4 =>"6", 6 =>"4", 8 =>"2" }
        h2 = h.reject{|key, value| key.to_i < value.to_i} #=> {6=>"4", 8=>"2"}
        [h, h2]
        "##,
        );
    }

    #[test]
    fn merge() {
        run_test_with_prelude(
            r##"
        [h1.merge, h1.merge(h2), h1.merge(h2, h3)]
        "##,
            r#"
            h1 = { "a" => 100, "b" => 200 }
            h2 = { "b" => 246, "c" => 300 }
            h3 = { "b" => 357, "d" => 400 }
            "#,
        );
    }

    #[test]
    fn merge_() {
        run_test_with_prelude(
            r##"
        [h1.merge!, h1.update(h2), h1.merge!(h2, h3)]
        "##,
            r#"
            h1 = { "a" => 100, "b" => 200 }
            h2 = { "b" => 246, "c" => 300 }
            h3 = { "b" => 357, "d" => 400 }
            "#,
        );
    }

    #[test]
    fn compare_by_identity() {
        run_test_with_prelude(
            r##"
            [h1["a"], h1[75], h1[:c]]
        "##,
            r#"
            h1 = { "a" => 100, 75 => 200, :c => "c" }
            h1.compare_by_identity
            "#,
        );
    }

    #[test]
    fn to_h() {
        run_test(
            r##"
        hash = { "a" => 97, "b" => 98 }
        hash.to_h {|key, value| [key.upcase, value - 32] } # => {"A"=>65, "B"=>66}
        "##,
        );
    }

    #[test]
    fn env_fetch() {
        //run_test(r##"ENV["PWD"]"##);
        //run_test(r##"ENV.fetch("PWD")"##);
        run_test(r##"ENV.fetch("XZCDEWS", "ABC")"##);
        run_test(r##"ENV.fetch("XZCDEWS") {|key| key + "先生"}"##);
        run_test_error(r##"ENV[100]"##);
    }

    #[test]
    fn hash_new() {
        run_test(
            r##"
        h = Hash.new do |hash, key|
            hash[key] = "foo"
            "bar"
        end

        [h[:a], h[:a], h[:a]]
        "##,
        );
    }
}
