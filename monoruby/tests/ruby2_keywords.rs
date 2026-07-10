extern crate monoruby;
use monoruby::tests::*;

#[test]
fn hash_ruby2_keywords_hash_flag() {
    run_test_once(
        r#"
        h = Hash.ruby2_keywords_hash(a: 1)
        res = [Hash.ruby2_keywords_hash?(h), Hash.ruby2_keywords_hash?({a: 1}), h == {a: 1}]
        # positional pass keeps identity and flag
        def bar(a); a; end
        res << bar(h).equal?(h) << Hash.ruby2_keywords_hash?(h)
        # dup drops the flag; subclass and ivars are preserved
        res << Hash.ruby2_keywords_hash?(h.dup)
        klass = Class.new(Hash)
        sub = klass.new
        sub[:a] = 1
        sub.instance_variable_set(:@x, 42)
        marked = Hash.ruby2_keywords_hash(sub)
        res << marked.class.equal?(klass) << marked.instance_variable_get(:@x)
        res
        "#,
    );
}

#[test]
fn ruby2_keywords_delegation() {
    run_test_once(
        r#"
        def target(*args, **kw); [args, kw]; end
        class << self
          ruby2_keywords def m_call(*args); target(*args); end
        end
        res = []
        empty = {}
        res << m_call(**empty) << Hash.ruby2_keywords_hash?(empty)
        res << m_call(empty) << m_call(a: 1) << m_call({a: 1})

        class R2KSuperTgt; def t(*args, **kw); [args, kw]; end; end
        class R2KSuperSub < R2KSuperTgt
          ruby2_keywords def t(*args); super(*args); end
        end
        class R2KZSuperSub < R2KSuperTgt
          ruby2_keywords def t(*args); super; end
        end
        res << R2KSuperSub.new.t(a: 1) << R2KSuperSub.new.t({a: 1})
        res << R2KZSuperSub.new.t(a: 1) << R2KZSuperSub.new.t({a: 1})

        class << self
          def y(args, &blk); yield(*args); end
          ruby2_keywords def m_yield(*outer)
            y(outer, &-> *args, **kwargs { target(*args, **kwargs) })
          end
        end
        res << m_yield(a: 1) << m_yield({a: 1})
        res
        "#,
    );
}

#[test]
fn ruby2_keywords_flag_propagation_rules() {
    run_test_once(
        r#"
        h = Hash.ruby2_keywords_hash(a: 1)
        res = []
        # splat into an unmarked no-keyword callee: unflagged copy
        def take(*args); args; end
        r = take(*[1, h])
        res << Hash.ruby2_keywords_hash?(r.last) << r.last.equal?(h) << r.last
        def take2(a, b); [a, b]; end
        r2 = take2(*[1, h])
        res << Hash.ruby2_keywords_hash?(r2[1]) << r2[1].equal?(h)
        # splat into a marked callee: same object, flag kept
        ruby2_keywords def marked(*args); args; end
        r4 = marked(*[1, h])
        res << Hash.ruby2_keywords_hash?(r4.last) << r4.last.equal?(h)
        # splat into a keyword-accepting callee: becomes the keywords
        def kw_target(*args, **kw); [args, kw]; end
        res << kw_target(*[1, h])
        res
        "#,
    );
}

#[test]
fn proc_ruby2_keywords() {
    run_test_once(
        r#"
        res = []
        f = -> *a { a.last }
        res << f.ruby2_keywords.equal?(f)
        res << Hash.ruby2_keywords_hash?(f.call(1, 2, a: "a"))
        f1 = -> *a { a.last }
        f1.ruby2_keywords
        f2 = f1.dup
        res << Hash.ruby2_keywords_hash?(f2.call(1, a: "a"))
        # incompatible signature warns (message compared via capture)
        saved = $stderr
        buf = +""
        io = Object.new
        io.define_singleton_method(:write) { |*x| x.each { |v| buf << v.to_s }; nil }
        $stderr = io
        (-> a, b { }).ruby2_keywords
        $stderr = saved
        res << (buf =~ /Skipping set of ruby2_keywords flag for proc/ ? "warned" : "silent")
        # define_method + ruby2_keywords by name
        klass = Class.new do
          def bar(a, foo: nil); [a, foo]; end
          def self.setup
            send :define_method, :foo, &make_proc
            send :ruby2_keywords, :foo
          end
          def self.make_proc = proc { |a, *args| bar(a, *args) }
        end
        klass.setup
        res << klass.new.foo(1, foo: 2)
        res
        "#,
    );
}

#[test]
fn ruby2_keywords_jit_tier() {
    // Hot loop so the JIT compiles the marked method and the delegation
    // call; the flagged-hash promotion happens on the IC-miss/argument
    // marshaling path and must keep working under the JIT.
    run_test(
        r#"
        def target(*args, **kw); [args, kw]; end
        class << self
          ruby2_keywords def m(*args); target(*args); end
        end
        res = []
        40.times do |i|
          res << m(a: i) << m({b: i})
        end
        res.last(4)
        "#,
    );
}
