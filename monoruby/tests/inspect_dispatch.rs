extern crate monoruby;
use monoruby::tests::*;

// `Object#inspect`, `Struct#inspect`, `Data#inspect`, `Set#inspect` and
// `Enumerator#inspect` show each value through `rb_inspect`: its own
// `#inspect`, dispatched (#1936, #1947). They used to render the values
// without a method call, so a user-defined `#inspect` was ignored, and a
// value whose `inspect` is written in Ruby — a Thread — came out as a
// bare address.

const HELPER: &str = r#"
  require "monitor"
  class B; def inspect; "BBB"; end; end
  class U; def inspect; "うう"; end; end
  class E; def inspect; "\xE3\x81\x82".dup.force_encoding("EUC-JP"); end; end
  class R; def inspect; raise "boom"; end; end
  class N; def inspect; nil; end; end
  class A; def initialize(*v); @a, @b = v; end; end
  S = Struct.new(:a, :b)
  D = Data.define(:a, :b)
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
  f = ->(x) { x.inspect.gsub(/0x\h+/, "0x") }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// The issue's program.
#[test]
fn a_thread_held_by_an_object_or_a_struct() {
    run(r#"
        class HoldsThreads
          def initialize; @t = Thread.current; @u = Thread.new { 1 }.join; end
        end
        [
          f.(HoldsThreads.new).sub(/ [^ ]*:\d+ dead/, " dead"),
          f.(S.new(Thread.current)),
          f.(D.new(Thread.current, 1)),
          f.([Thread.current]),
          S.new(Thread.current).to_s == S.new(Thread.current).inspect,
        ]
    "#);
}

#[test]
fn values_are_shown_by_their_own_inspect() {
    run(r#"
        o = Object.new
        def o.inspect = "O!"
        [
          f.(A.new(B.new, 1)),
          f.(A.new(o, [B.new])),
          f.(A.new(:a, "s")),
          f.(A.new(nil, 1.5)),
          f.(A.new(S.new(B.new, 2), D.new(3, B.new))),
          f.(A.new(Thread::Mutex.new, Thread::Queue.new)),
          f.(A.new(Monitor.new, Thread::ConditionVariable.new)),
          f.(A.new(1..2, /re/)),
          S.new(B.new, U.new).inspect,
          S.new(o).inspect,
          D.new(B.new, o).inspect,
          Struct.new(:a).new(B.new).inspect,
          Struct.new(:"a b", :"c?", :D, :_e).new(B.new, 2).inspect,
          Class.new(S).new(B.new).inspect,
          Data.define(:x).new(B.new).inspect.sub(/0x\h+/, "0x"),
          Struct.new(:a, keyword_init: true).new(a: B.new).inspect,
          [S.new(B.new)].inspect,
          {S.new(B.new) => D.new(B.new, 1)}.inspect,
          S.new(B.new).to_s,
        ]
    "#);
}

#[test]
fn what_inspect_answers() {
    run(r#"
        [
          t.() { A.new(R.new).inspect },
          t.() { S.new(R.new).inspect },
          t.() { D.new(R.new, 1).inspect },
          f.(A.new(N.new)),
          f.(S.new(N.new)),
          # The exception left nothing behind in the recursion guard.
          (a = A.new(R.new); t.() { a.inspect }; t.() { a.inspect }),
        ]
    "#);
}

#[test]
fn recursion() {
    run(r#"
        a = A.new; a.instance_variable_set(:@a, a)
        b = A.new; c = A.new(b); b.instance_variable_set(:@a, c)
        d = A.new; d.instance_variable_set(:@a, [d, {d => d}])
        s = S.new; s.a = s
        s2 = S.new; s2.a = [s2]
        s3 = S.new; a3 = A.new(s3); s3.a = a3
        dd = D.new([], 1); dd.a << dd
        [f.(a), f.(b), f.(d), f.(s), f.(s2), f.(s3), f.(a3), f.(dd),
         f.(S.new(S.new(1, D.new(2, S.new(3))), [S.new(B.new)]))]
    "#);
}

#[test]
fn encoding_of_the_rendering() {
    run(r#"
        class Cé; def initialize; @é = 1; @b = "é"; end; end
        Sé = Struct.new(:é)
        [
          A.new(1, 2).inspect.encoding.to_s, A.new.inspect.encoding.to_s, Object.new.inspect.encoding.to_s,
          S.new(1, 2).inspect.encoding.to_s, S.new.inspect.encoding.to_s, D.new(1, 2).inspect.encoding.to_s,
          f.(A.new(U.new, 1)), A.new(U.new, 1).inspect.encoding.to_s,
          f.(A.new(E.new, 1)).b, A.new(E.new, 1).inspect.encoding.to_s,
          S.new(B.new, U.new).inspect.encoding.to_s, D.new(U.new, 1).inspect.encoding.to_s,
          S.new(E.new, 1).inspect.encoding.to_s,
          f.(A.new("い")), A.new("い").inspect.encoding.to_s,
          S.new("い").inspect, S.new("い").inspect.encoding.to_s,
          f.(A.new("\xff")), S.new("\xff".b).inspect,
          f.(Cé.new), Cé.new.inspect.encoding.to_s,
          Sé.new(1).inspect, Sé.new(1).inspect.encoding.to_s,
          # A piece in another encoding than the result's is escaped by
          # `rb_inspect`, so the two never meet.
          f.(A.new(U.new, E.new)),
          S.new(U.new, E.new).inspect,
        ]
    "#);
}

/// Under a result encoding that cannot show a value's `#inspect`, that
/// value is escaped — it alone, not the rendering around it.
#[test]
fn values_are_escaped_for_the_result_encoding() {
    run(r#"
        ext, int = Encoding.default_external, Encoding.default_internal
        res = []
        begin
          Encoding.default_external = "US-ASCII"
          res << f.(A.new("い", U.new)) << A.new("い").inspect.encoding.to_s
          res << S.new("い", U.new).inspect << S.new("い").inspect.encoding.to_s
          res << D.new("い", U.new).inspect
          Encoding.default_external = "EUC-JP"
          res << f.(A.new("い", U.new)) << S.new("い", U.new).inspect
        ensure
          Encoding.default_external = ext
          Encoding.default_internal = int
        end
        res
    "#);
}

/// `instance_variables_to_inspect` selects; the order stays the
/// object's, and the values are the ones the object held when `inspect`
/// began.
#[test]
fn instance_variables_to_inspect() {
    run(r#"
        class X1; def initialize; @a = 1; end; def instance_variables_to_inspect = [:@a, :@zz]; end
        class X2; def initialize; @a = B.new; @b = 2; end; private def instance_variables_to_inspect = [:@b, :@a]; end
        class X3; def initialize; @a = 1; end; def instance_variables_to_inspect = []; end
        class X4; def initialize; @a = 1; end; def instance_variables_to_inspect = 5; end
        class X5; def initialize; @a = 1; end; def instance_variables_to_inspect = nil; end
        class Y; def initialize(o); @o = o; @z = 1; end; end
        class Z; def initialize(y); @y = y; end; def inspect; @y.remove_instance_variable(:@z) if @y.instance_variable_defined?(:@z); "Z"; end; end
        y = Y.new(nil); y.instance_variable_set(:@o, Z.new(y))
        [f.(X1.new), f.(X2.new), f.(X3.new), t.() { X4.new.inspect }, f.(X5.new), f.(y), f.(y)]
    "#);
}

#[test]
fn backtrace_location_inspect() {
    run(r#"
        l = caller_locations(0)[0]
        [l.inspect == l.to_s.inspect, l.inspect.start_with?('"'), l.instance_variables]
    "#);
}

/// #1947's program (the MatchData lines are #1907's own business).
#[test]
fn issue_1947() {
    run(r#"
        class Foo; def inspect = "FOO"; end
        class HoldsFoo; def initialize; @f = Foo.new; end; end
        [
          f.(HoldsFoo.new),
          S.new(Foo.new).inspect,
          D.new(Foo.new, 1).inspect,
          [Foo.new].inspect,
          {a: Foo.new}.inspect,
          Set[Foo.new].inspect,
        ]
    "#);
}

#[test]
fn set_inspect() {
    run(r#"
        class MySet < Set; end
        class LoudSet < Set; def inspect = "LOUD"; end
        rec = Set[1]; rec << rec
        myrec = MySet[1]; myrec << myrec
        shrink = Set[1, 2]
        k = Object.new
        k.define_singleton_method(:inspect) { shrink.delete(2); "K" }
        shrink << k
        [
          Set[B.new].inspect,
          Set[Set[B.new], [B.new]].inspect,
          Set[].inspect, Set[1, :a, "s", nil].inspect,
          Set[U.new, 1].inspect, Set[1, U.new].inspect,
          Set[1].inspect.encoding.to_s, Set[B.new].inspect.encoding.to_s,
          Set[].inspect.encoding.to_s,
          f.(Set[Thread.current, Thread::Mutex.new]),
          rec.inspect, rec.inspect.encoding.to_s,
          Set[LoudSet[1]].inspect, [LoudSet[1]].inspect,
          Set[1].to_s, Set[B.new].to_s,
          Set.instance_method(:to_s) == Set.instance_method(:inspect),
          t.() { Set[R.new].inspect },
          t.() { Set[N.new].inspect },
          Set[1, 2].compare_by_identity.inspect,
          [Set[1, 2].each.inspect, shrink.inspect, shrink.inspect],
          # A subclass answers in `Set::SubclassCompatible`'s shape.
          MySet[B.new, 1].inspect, MySet[].inspect, MySet[U.new, :a].inspect,
          MySet[1].inspect.encoding.to_s, MySet[1].to_s,
          myrec.inspect, myrec.inspect.encoding.to_s,
          f.(Class.new(Set)[1]),
        ]
    "#);
}

#[test]
fn enumerator_inspect() {
    run(r#"
        o = Object.new
        def o.inspect = "OBJ"
        def o.each; end
        a = []; self_ref = a.each; a << self_ref
        via_arg = [1].each_with_object([]); via_arg.each { |x, m| m << via_arg }
        class MyEnum < Enumerator; end
        [
          [B.new].each.inspect,
          [B.new].each_slice(1).inspect,
          [1].each_with_object(B.new).inspect,
          [B.new].each.with_index(1).inspect,
          ([B.new].each + [B.new]).inspect,
          o.to_enum.inspect, o.enum_for(:each, o, B.new).inspect,
          [1].to_enum(:foo, 1, a: B.new, b: nil, c: :sym).inspect.sub(/#<B:0x\h+>/, "<B>"),
          [1].to_enum(:foo, {1 => B.new}).inspect,
          [1].each_slice(1, **{}).inspect,
          Set[B.new].each.inspect,
          (1..3).each_cons(2).inspect,
          [U.new].each.inspect, [1].each_with_object(U.new).inspect,
          [1].each.inspect.encoding.to_s, [B.new].each.inspect.encoding.to_s,
          self_ref.inspect, via_arg.inspect, self_ref.inspect.encoding.to_s,
          Enumerator.allocate.inspect, MyEnum.allocate.inspect,
          MyEnum.new { |y| y << 1 }.inspect.sub(/0x\h+/, "0x").sub(/:\d+/, ""),
          f.([Thread.current].each),
          t.() { [R.new].each.inspect },
          t.() { [1].each_with_object(R.new).inspect },
          [B.new].each.to_s.sub(/0x\h+/, "0x"),
          [B.new].lazy.inspect, [B.new].lazy.map {}.inspect,
          Enumerator::Product.new([B.new]).inspect,
        ]
    "#);
}
