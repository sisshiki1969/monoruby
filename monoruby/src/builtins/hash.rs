use super::*;

//
// Hash class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Hash", HASH_CLASS);
    globals.define_builtin_class_func(HASH_CLASS, "new", new);
    globals.define_builtin_func(HASH_CLASS, "size", size);
    globals.define_builtin_func(HASH_CLASS, "length", size);
    globals.define_builtin_func(HASH_CLASS, "clear", clear);
    globals.define_builtin_func(HASH_CLASS, "[]", index);
    globals.define_builtin_func(HASH_CLASS, "[]=", index_assign);
    globals.define_builtin_func(HASH_CLASS, "store", index_assign);
    globals.define_builtin_func(HASH_CLASS, "fetch", fetch);
    globals.define_builtin_func(HASH_CLASS, "keys", keys);
    globals.define_builtin_func(HASH_CLASS, "values", values);
    globals.define_builtin_func(HASH_CLASS, "each", each);
    globals.define_builtin_func(HASH_CLASS, "each_key", each_key);
    globals.define_builtin_func(HASH_CLASS, "each_value", each_value);
    globals.define_builtin_func(HASH_CLASS, "has_key?", include);
    globals.define_builtin_func(HASH_CLASS, "include?", include);
    globals.define_builtin_func(HASH_CLASS, "key?", include);
    globals.define_builtin_func(HASH_CLASS, "member?", include);
    globals.define_builtin_func(HASH_CLASS, "to_s", inspect);
    globals.define_builtin_func(HASH_CLASS, "inspect", inspect);
    globals.define_builtin_func(HASH_CLASS, "merge", merge);
    globals.define_builtin_func(HASH_CLASS, "compare_by_identity", compare_by_identity);

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
    globals.define_builtin_singleton_func(env, "fetch", fetch);
    globals.define_builtin_singleton_func(env, "[]", env_index);
}

/// ### Hash.new
/// - new(ifnone = nil) -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
#[monoruby_builtin]
fn new(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let map = IndexMap::default();
    let obj = Value::hash_with_class(map, class);
    Ok(obj)
}

/// ### Hash#length
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let len = lfp.self_val().as_hash().len();
    Ok(Value::integer(len as i64))
}

/// ### Hash#[]=
/// - self[key] = value
/// - store(key, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let key = arg[0];
    let val = arg[1];
    lfp.self_val().as_hash_mut().insert(key, val);
    Ok(val)
}

/// ### Hash#[]
/// - self[key] -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d.html]
#[monoruby_builtin]
fn index(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let key = arg[0];
    let val = lfp.self_val().as_hash().get(key).unwrap_or_default();
    Ok(val)
}

/// ### Hash#clear
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
#[monoruby_builtin]
fn clear(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_hash_mut().clear();
    Ok(self_)
}

/// ### Hash#keys
/// - keys -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keys.html]
#[monoruby_builtin]
fn keys(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let keys = lfp.self_val().as_hash().keys();
    Ok(Value::array_from_vec(keys))
}

///
/// ### Hash#values
/// - values -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/values.html]
#[monoruby_builtin]
fn values(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let keys = lfp.self_val().as_hash().values();
    Ok(Value::array_from_vec(keys))
}

///
/// ### Hash#each
///
/// - each {|key, value| ... } -> self
/// - each_pair {|key, value| ... } -> self
/// - [NOT SUPPORTED] each -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each.html]
#[monoruby_builtin]
fn each(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let ary = lfp.self_val();
    let data = globals.get_block_data(vm.cfp(), bh);
    for (k, v) in ary.as_hash().iter() {
        vm.invoke_block(globals, &data, &[k, v])?;
    }
    Ok(lfp.self_val())
}

///
/// ### Hash#each_value
///
/// - each_value {|value| ... } -> self
/// - [NOT SUPPORTED] each_value -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each_value.html]
#[monoruby_builtin]
fn each_value(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let ary = lfp.self_val();
    let iter = ary.as_hash().iter().map(|(_, v)| v);
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
}

///
/// ### Hash#each_key
///
/// - each_key {|value| ... } -> self
/// - [NOT SUPPORTED] each_key -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/each_key.html]
#[monoruby_builtin]
fn each_key(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let ary = lfp.self_val();
    let iter = ary.as_hash().iter().map(|(k, _)| k);
    vm.invoke_block_iter1(globals, bh, iter)?;
    Ok(lfp.self_val())
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
fn include(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let b = lfp.self_val().as_hash().contains_key(arg[0]);
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
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let s = globals.inspect(lfp.self_val());
    Ok(Value::string(s))
}

///
/// ### Hash#merge
///
/// - merge(*others) -> Hash
/// - [NOT SUPPORTED]merge(*others) {|key, self_val, other_val| ... } -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/merge.html]
#[monoruby_builtin]
fn merge(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.expect_no_block()?;
    let self_val = lfp.self_val();
    let mut inner = self_val.as_hash().clone();
    for arg in lfp.iter() {
        let other = arg.expect_hash(globals)?;
        for (k, v) in other.iter() {
            inner.insert(k, v);
        }
    }

    Ok(Value::hash_from_inner(inner))
}

///
/// ### Hash#compare_by_identity
///
/// - compare_by_identity -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/compare_by_identity.html]
#[monoruby_builtin]
fn compare_by_identity(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 0)?;
    lfp.expect_no_block()?;
    let mut self_val = lfp.self_val();
    self_val.as_hash_mut().compare_by_identity();
    Ok(lfp.self_val())
}

// ENV object

/// ###ENV.[]
/// - self[key] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d.html]
#[monoruby_builtin]
fn env_index(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let key = arg[0];
    if key.is_string().is_none() {
        return Err(MonorubyErr::no_implicit_conversion(
            globals,
            key,
            STRING_CLASS,
        ));
    }
    let val = lfp.self_val().as_hash().get(key).unwrap_or_default();
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
fn fetch(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    let self_ = lfp.self_val();
    let map = self_.as_hash();
    let s = if let Some(bh) = lfp.block() {
        MonorubyErr::check_number_of_arguments(len, 1)?;
        match map.get(arg[0]) {
            Some(v) => v,
            None => vm.invoke_block_once(globals, bh, &[arg[0]])?,
        }
    } else if len == 1 {
        match map.get(arg[0]) {
            Some(v) => v,
            None => {
                return Err(MonorubyErr::keyerr(format!(
                    "key not found: {}",
                    globals.to_s(arg[0])
                )))
            }
        }
    } else {
        MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
        match map.get(arg[0]) {
            Some(v) => v,
            None => arg[1],
        }
    };
    Ok(s)
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
}