use crate::*;

//
// Array class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(HASH_CLASS, "new", new, -1);
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
}

/// ### Hash.new
/// - new(ifnone = nil) -> Hash
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/s/new.html]
extern "C" fn new(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let class = self_val.as_class().class_id();
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
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let len = self_val.as_hash().len();
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
    mut self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let key = arg[0];
    let val = arg[1];
    self_val.as_hash_mut().insert(key, val);
    Some(val)
}

/// ### Hash#[]
/// - self[key] -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/=5b=5d.html]
extern "C" fn index(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let key = arg[0];
    let val = self_val.as_hash().get(key).unwrap_or_default();
    Some(val)
}

/// ### Hash#clear
/// - clear -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/clear.html]
extern "C" fn clear(
    _vm: &mut Executor,
    _globals: &mut Globals,
    mut self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    self_val.as_hash_mut().clear();
    Some(self_val)
}

/// ### Hash#keys
/// - keys -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/keys.html]
extern "C" fn keys(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let keys = self_val.as_hash().keys();
    Some(Value::new_array_from_vec(keys))
}

/// ### Hash#values
/// - values -> [object]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Hash/i/values.html]
extern "C" fn values(
    _vm: &mut Executor,
    _globals: &mut Globals,
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let keys = self_val.as_hash().values();
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
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let b = self_val.as_hash().contains_key(arg[0]);
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
    self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let s = globals.val_inspect(self_val);
    Some(Value::new_string(s))
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
}
