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
extern "C" fn new(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let class = lfp.self_val().as_class().class_id();
    let map = IndexMap::default();
    let obj = Value::new_hash_with_class(map, class);
    Some(obj)
}

/// ### Hash#length
/// - length -> Integer
/// - size -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/length.html]
extern "C" fn size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let len = lfp.self_val().as_hash().len();
    Some(Value::new_integer(len as i64))
}

/// ### Hash#[]=
/// - self[key] = value
/// - store(key, value) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d=3d.html]
extern "C" fn index_assign(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let key = arg[0];
    let val = arg[1];
    lfp.self_val().as_hash_mut().insert(key, val);
    Some(val)
}

/// ### Hash#[]
/// - self[key] -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d.html]
extern "C" fn index(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let key = arg[0];
    let val = lfp.self_val().as_hash().get(key).unwrap_or_default();
    Some(val)
}

/// ### Hash#clear
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
extern "C" fn clear(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let mut self_ = lfp.self_val();
    self_.as_hash_mut().clear();
    Some(self_)
}

/// ### Hash#keys
/// - keys -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keys.html]
extern "C" fn keys(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let keys = lfp.self_val().as_hash().keys();
    Some(Value::new_array_from_vec(keys))
}

/// ### Hash#values
/// - values -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/values.html]
extern "C" fn values(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let keys = lfp.self_val().as_hash().values();
    Some(Value::new_array_from_vec(keys))
}

/// ### Hash#has_key?
/// - has_key?(key) -> bool
/// - include?(key) -> bool
/// - key?(key) -> bool
/// - member?(key) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/has_key=3f.html]
extern "C" fn include(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let b = lfp.self_val().as_hash().contains_key(arg[0]);
    Some(Value::bool(b))
}

/// ### Hash#inspect
/// - to_s -> String
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/inspect.html]
extern "C" fn inspect(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let s = globals.val_inspect(lfp.self_val());
    Some(Value::new_string(s))
}

// ENV object

/// ###ENV.[]
/// - self[key] -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/=5b=5d.html]
extern "C" fn env_index(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let key = arg[0];
    if key.is_string().is_none() {
        globals.err_no_implicit_conversion(key, STRING_CLASS);
        return None;
    }
    let val = lfp.self_val().as_hash().get(key).unwrap_or_default();
    Some(val)
}

/// ### ENV.fetch
/// - fetch(key) -> String
/// - fetch(key, default) -> String
/// - fetch(key) {|key| ... } -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/ENV/s/fetch.html]
extern "C" fn env_fetch(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    let self_ = lfp.self_val();
    let env_map = self_.as_hash();
    let s = if let Some(bh) = lfp.block() {
        globals.check_number_of_arguments(len, 1..=1)?;
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
        globals.check_number_of_arguments(len, 1..=2)?;
        match env_map.get(arg[0]) {
            Some(s) => s,
            None => arg[1],
        }
    };
    Some(s)
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
    fn env_fetch() {
        run_test(r##"ENV["PWD"]"##);
        run_test(r##"ENV.fetch("PWD")"##);
        run_test(r##"ENV.fetch("XZCDEWS", "ABC")"##);
        run_test(r##"ENV.fetch("XZCDEWS") {|key| key + "先生"}"##);
        run_test_error(r##"ENV[100]"##);
    }
}
