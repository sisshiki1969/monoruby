use crate::*;

//
// Hash class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(HASH_CLASS, "new", new, -1);
    globals.define_builtin_func(HASH_CLASS, "size", size, 0);
    globals.define_builtin_func(HASH_CLASS, "length", size, 0);
    globals.define_builtin_func(HASH_CLASS, "clear", clear, 0);
    globals.define_builtin_func(HASH_CLASS, "[]", index, 1);
    globals.define_builtin_func(HASH_CLASS, "[]=", index_assign, 2);
    globals.define_builtin_func(HASH_CLASS, "store", index_assign, 2);
    globals.define_builtin_func(HASH_CLASS, "keys", keys, 0);
    globals.define_builtin_func(HASH_CLASS, "values", values, 0);
    globals.define_builtin_func(HASH_CLASS, "each", each, 0);
    globals.define_builtin_func(HASH_CLASS, "has_key?", include, 1);
    globals.define_builtin_func(HASH_CLASS, "include?", include, 1);
    globals.define_builtin_func(HASH_CLASS, "key?", include, 1);
    globals.define_builtin_func(HASH_CLASS, "member?", include, 1);
    globals.define_builtin_func(HASH_CLASS, "to_s", inspect, 0);
    globals.define_builtin_func(HASH_CLASS, "inspect", inspect, 0);

    let mut env_map = IndexMap::default();
    std::env::vars().for_each(|(var, val)| {
        env_map.insert(HashKey(Value::new_string(var)), Value::new_string(val));
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

    let env = Value::new_hash(env_map);
    globals.set_constant_by_str(OBJECT_CLASS, "ENV", env);
    globals.define_builtin_singleton_func(env, "fetch", env_fetch, -1);
    globals.define_builtin_singleton_func(env, "[]", env_index, 1);
}

/// ### Hash.new
/// - new(ifnone = nil) -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
#[monoruby_builtin]
fn new(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let class = lfp.self_val().as_class_id();
    let map = IndexMap::default();
    let obj = Value::new_hash_with_class(map, class);
    Ok(obj)
}

/// ### Hash#length
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
#[monoruby_builtin]
fn size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let len = lfp.self_val().as_hash().len();
    Ok(Value::new_integer(len as i64))
}

/// ### Hash#[]=
/// - self[key] = value
/// - store(key, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d=3d.html]
#[monoruby_builtin]
fn index_assign(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
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
fn index(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let key = arg[0];
    let val = lfp.self_val().as_hash().get(key).unwrap_or_default();
    Ok(val)
}

/// ### Hash#clear
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
#[monoruby_builtin]
fn clear(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_hash_mut().clear();
    Ok(self_)
}

/// ### Hash#keys
/// - keys -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keys.html]
#[monoruby_builtin]
fn keys(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let keys = lfp.self_val().as_hash().keys();
    Ok(Value::new_array_from_vec(keys))
}

///
/// ### Hash#values
/// - values -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/values.html]
#[monoruby_builtin]
fn values(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let keys = lfp.self_val().as_hash().values();
    Ok(Value::new_array_from_vec(keys))
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
fn each(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let ary = lfp.self_val();
    let block_handler = if let Some(block) = lfp.block() {
        block
    } else {
        return Err(MonorubyErr::no_block_given());
    };
    let data = vm.get_block_data(globals, block_handler);
    for (k, v) in ary.as_hash().iter() {
        vm.invoke_block(globals, data.clone(), &[k, v])?;
    }
    Ok(lfp.self_val())
}

/// ### Hash#has_key?
/// - has_key?(key) -> bool
/// - include?(key) -> bool
/// - key?(key) -> bool
/// - member?(key) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/has_key=3f.html]
#[monoruby_builtin]
fn include(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let b = lfp.self_val().as_hash().contains_key(arg[0]);
    Ok(Value::bool(b))
}

/// ### Hash#inspect
/// - to_s -> String
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/inspect.html]
#[monoruby_builtin]
fn inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let s = globals.inspect(lfp.self_val());
    Ok(Value::new_string(s))
}

// ENV object

/// ###ENV.[]
/// - self[key] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d.html]
#[monoruby_builtin]
fn env_index(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
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

/// ### ENV.fetch
/// - fetch(key) -> String
/// - fetch(key, default) -> String
/// - fetch(key) {|key| ... } -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/fetch.html]
#[monoruby_builtin]
fn env_fetch(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let env_map = self_.as_hash();
    let s = if let Some(bh) = lfp.block() {
        Executor::check_number_of_arguments(len, 1..=1)?;
        match env_map.get(arg[0]) {
            Some(s) => s,
            None => {
                let data = vm.get_block_data(globals, bh);
                vm.invoke_block(globals, data, &[arg[0]])?
            }
        }
    } else if len == 1 {
        env_map.get(arg[0]).unwrap()
    } else {
        Executor::check_number_of_arguments(len, 1..=2)?;
        match env_map.get(arg[0]) {
            Some(s) => s,
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
