//! Three CRuby 4.0 behaviours around core classes that are C there.
//!
//! - **`Set` subclasses** (#2044). Subclassing `Set` directly runs
//!   `Set.inherited`, which includes `Set::SubclassCompatible` and extends
//!   its `ClassMethods` (CRuby's `set/subclass_compatible.rb`), so a
//!   subclass written for the pre-4.0 Ruby `Set` has its own
//!   `initialize` / `add` called. `Set::CoreSet` opts out.
//! - **`Encoding::UndefinedConversionError`** (#2089) has `error_char`
//!   but not `error_bytes`, which is `InvalidByteSequenceError`'s alone.
//! - **Messages the interpreter builds** (#2013). They are BINARY while
//!   ASCII (`rb_sprintf`), US-ASCII for a NameError / NoMethodError (a
//!   `NameError::message`, which `Marshal.dump` writes through its
//!   `_dump`), and a message the program gave keeps its own encoding.
//!   `Marshal.dump` writes an instance variable set after the raise after
//!   `bt_locations`.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

#[test]
fn set_subclass_compatible() {
    run_test_once(&format!(
        r#"
        {HELPER}
        class MySet < Set; end
        class MySet2 < Set; def initialize(*) = (super; @z = 1); end
        class MS4 < Set; def initialize(*a) = (super; $init = a); end
        class MS5 < Set; def add(o) = ($adds ||= []) << o; end
        class C2 < Set::CoreSet; end
        class S3 < MySet; end
        x = MySet[1]; x << x
        y = C2[1]; y << y
        [
          MySet.ancestors.take(4), MySet.singleton_class.ancestors.take(3),
          Set.const_defined?(:SubclassCompatible), Set.const_defined?(:CoreSet),
          MySet.method(:[]).owner, MySet.instance_method(:initialize).owner,
          t.() {{ MySet2[1].instance_variables }},
          t.() {{ $init = nil; MS4[1, 2]; $init }},
          t.() {{ MS5[1, 2]; $adds }},
          C2.ancestors.take(3), C2[1, 2], C2.new, Set::CoreSet[1], S3.ancestors.take(4),
          t.() {{ Set::SubclassCompatible }},
          Set.singleton_class.private_method_defined?(:inherited),
          MySet[1, 2, [3]], MySet[1, 2].to_s, x, y,
          [MySet[1, 2] == Set[1, 2], Set[1, 2] == MySet[1, 2], MySet[1, 2].eql?(Set[1, 2])],
          [MySet[1, 2] | [3], MySet[1, 2] & [2], MySet[1, 2] - [1], MySet[1, 2] ^ [2, 3]],
          [MySet[1, 2].subset?(Set[1, 2, 3]), MySet[1, 2] <=> Set[1], MySet[1, 2].intersect?([2])],
          [MySet[Set[1], 2].flatten, MySet[1, 2, 3].classify(&:odd?)],
          [MySet[1, 2, 3].select! {{ _1 > 1 }}, MySet[1, 2, 3].map! {{ _1 * 2 }}, MySet[1, 2].delete_if(&:odd?)],
          [MySet.new([1, 2]) {{ _1 * 10 }}, MySet[1, 2].to_set.class, MySet[1].replace([5, 6])],
          MySet[1, 2].join("-"), t.() {{ MySet.new(3) }},
          Marshal.load(Marshal.dump(MySet[1, 2])), MySet[1, 2].hash == Set[1, 2].hash,
        ].map(&:inspect)
        "#
    ));
}

#[test]
fn undefined_conversion_error_methods() {
    run_test_once(
        r#"
        e = ("あ".encode("ISO-8859-1") rescue $!)
        [Encoding::UndefinedConversionError.instance_methods(false).sort,
         Encoding::InvalidByteSequenceError.instance_methods(false).sort,
         e.respond_to?(:error_bytes), e.error_char]
        "#,
    );
}

#[test]
fn built_message_encodings() {
    run_test_once(&format!(
        r#"
        {HELPER}
        enc = ->(&b) {{ begin; b.call; rescue Exception => e; [e.class.name || "anon", e.message.encoding.to_s]; end }}
        def m(a); end
        [
          enc.() {{ {{}}.fetch(:a) }}, enc.() {{ foo }}, enc.() {{ 1.foo }}, enc.() {{ raise "x" }},
          enc.() {{ raise "x".b }}, enc.() {{ Integer("z") }}, enc.() {{ [].fetch(3) }}, enc.() {{ 1 / 0 }},
          enc.() {{ nil + 1 }}, enc.() {{ throw :a }}, enc.() {{ raise ArgumentError }}, enc.() {{ Foo }},
          enc.() {{ m }}, enc.() {{ exit }}, enc.() {{ abort }}, enc.() {{ abort "x".b }}, enc.() {{ Object.const_get(:lower) }},
          enc.() {{ "a".freeze << "b" }}, enc.() {{ [].each.next }}, enc.() {{ raise StopIteration }},
          enc.() {{ raise Interrupt }}, enc.() {{ raise SignalException, "INT" }},
          enc.() {{ raise Class.new(StandardError) }}, enc.() {{ raise ArgumentError, "given" }},
          enc.() {{ raise KeyError.new("k", receiver: 1, key: 2) }}, enc.() {{ raise NameError.new("n", :x) }},
          enc.() {{ Regexp.new("(") }}, enc.() {{ eval("1+") }}, enc.() {{ "あ".encode("ISO-8859-1") }},
          enc.() {{ raise IOError.new }}, enc.() {{ Fiber.new {{}}.tap(&:resume).resume }},
          enc.() {{ raise "é" }}, enc.() {{ wrong_é }},
          enc.() {{ RuntimeError.new("x").exception("y").then {{ raise _1 }} }},
          enc.() {{ raise Marshal.load(Marshal.dump(KeyError.new("x"))) }},
          enc.() {{ raise Marshal.load(Marshal.dump(({{}}.fetch(:a) rescue $!))) }},
          enc.() {{ raise Marshal.load(Marshal.dump((foo rescue $!))) }},
        ].map(&:inspect)
        "#
    ));
}

#[test]
fn exception_marshal_name_err_mesg_and_order() {
    run_test_once(&format!(
        r#"
        {HELPER}
        mesg = ->(e) {{ Marshal.dump(e).b[/:\tmesg.*?:\abt/m] }}
        order = ->(e) {{ Marshal.dump(e).b.scan(/:\tmesg|:\abt(?!_)|:\x11bt_locations|@[a-z]+|:\ncause/) }}
        e1 = (raise "x" rescue $!); e1.instance_variable_set(:@a, 1)
        e2 = RuntimeError.new("x"); e2.instance_variable_set(:@b, 1)
        e2 = (raise e2 rescue $!); e2.instance_variable_set(:@c, 2)
        e3 = (begin; raise "a"; rescue; raise "b"; end rescue $!); e3.instance_variable_set(:@d, 1)
        e4 = RuntimeError.new("x"); e4.instance_variable_set(:@e, 1)
        [
          mesg.((foo rescue $!)), mesg.((1.zz rescue $!)), mesg.((Foo rescue $!)),
          mesg.(({{}}.fetch(:a) rescue $!)), mesg.((throw :a rescue $!)), mesg.((raise "x" rescue $!)),
          mesg.(NameError.new("n")), mesg.((raise NameError rescue $!)),
          t.() {{ e = Marshal.load(Marshal.dump((foo rescue $!))); [e.message, e.message.encoding.to_s, e.name] }},
          t.() {{ Marshal.load("\x04\bIu:\x17NameError::message\x08abc\x06:\x06EF").then {{ [_1, _1.encoding.to_s] }} }},
          t.() {{ Marshal.load("\x04\bu:\x17NameError::message\x08abc").then {{ [_1, _1.encoding.to_s] }} }},
          t.() {{ a = Marshal.load(Marshal.dump([(foo rescue $!), "s", "s"])); [a[0].message, a[1]] }},
          order.(e1), order.(e2), order.(e3), order.(e4),
          t.() {{ order.(Marshal.load(Marshal.dump(e1))) }},
        ].map(&:inspect)
        "#
    ));
}
