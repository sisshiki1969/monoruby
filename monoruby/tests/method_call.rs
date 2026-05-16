extern crate monoruby;
use monoruby::tests::*;

#[test]
fn test_method_optional() {
    run_test_with_prelude(
        r#"
        f(1,2)
        "#,
        r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(1,2,3)
        "#,
        r#"
        def f(x,y,z=42)
            [x,y,z]
        end
        "#,
    );
}

#[test]
fn method_rest() {
    run_test_with_prelude(
        r#"
        f(1,2)
        "#,
        r#"
        def f(x,y,a=42,b=55,*z)
            [x,y,a,b,z]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(1,2,3,4,5,6,7)
        "#,
        r#"
        def f(x,y,a=42,b=55,*z)
            [x,y,a,b,z]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(1,2,3,4,5,6,7,a:1,b:2)
        "#,
        r#"
        def f(x,y,a=42,b=55,*z)
            [x,y,a,b,z]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(1,2,3,4,5,6,7)
        "#,
        r#"
        def f(x,y,*z)
            [x,y,z]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f { |a,b,x=42,y=12,*c|
          [a,b,c,x,y]
        }
        "#,
        r#"
        def f
          yield [1,2,3]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f { |a,b,x=42,y=12,*c|
          [a,b,c,x,y]
        }
        "#,
        r#"
        def f
          yield [1,2,3,4,5,6]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(0,1,2,3,4,5,6,7,8)
        "#,
        r#"
        def f(*x)
          x
        end
        "#,
    );
}

#[test]
fn method_post() {
    run_test_with_prelude(
        r#"
        f(1,2,3,4)
        "#,
        r#"
        def f(x,y,a=42,b=55,*z,c,d)
            [x,y,a,b,c,d,z]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(1,2,3,4,5,6,7,8)
        "#,
        r#"
        def f(x,y,a=42,b=55,*z,c,d)
            [x,y,a,b,z]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(1,2,3,4,5,6,7)
        "#,
        r#"
        def f(x,y,*z,a)
            [x,y,z,a]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f { |a,b,x=42,y=12,*c,d,e|
          [a,b,c,d,e,x,y]
        }
        "#,
        r#"
        def f
          yield [1,2,3,4,5]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f { |a,b,x=42,y=12,*c,d,e|
          [a,b,c,d,e,x,y]
        }
        "#,
        r#"
        def f
          yield [1,2,3,4,5,6,7,8]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(0,1,2,3,4,5,6,7,8)
        "#,
        r#"
        def f(*x,a,b)
          [x,a,b]
        end
        "#,
    );
}

#[test]
fn keyword() {
    run_test_with_prelude(
        r#"
        x = []
        x << f(1,2)
        x << f(1,2,3,c:10,a:20)
        x << f(1,2,3,4,5,6,7,8)
        x << f(1,2,3,4,5,6,7,8,b:50)
        x
        "#,
        r#"
        def f(x,y,z=10,*r,a:1,b:2,c:3)
          [x, y, z, r, a, b, c]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        x = []
        x << f(1,2) { |x| x * 14 }
        x << f(1,2,3,c:10,a:20) { |x| x * 14 }
        x << f(1,2,3,4,5,6,7,8) { |x| x * 14 }
        x << f(1,2,3,4,5,6,7,8,b:50) { |x| x * 14 }
        x
        "#,
        r#"
        def f(x,y,z=10,*r,a:1,b:2,c:3,&p)
          [x, y, z, r, a, b, c, g(&p)]
        end
        def g
          yield 3
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        [f(c:7), f(b:8, a:10), f]
        "#,
        r#"
        def f(a:1,b:2,c:3)
          [a, b, c]
        end
        "#,
    );
    run_test_error(
        r#"
        def f(a:1)
        end
        f(b:1)
        "#,
    );
    run_test_error(
        r#"
        def f(a:1)
        end
        f(**{b:1})
        "#,
    );
}

#[test]
fn keyword_fatarrow() {
    run_test_with_prelude(
        r##"
        $res = ""
        C.new.foo(a:42, 100 => 100, c:5)
        $res
        "##,
        r##"
        class C
          def foo(*x)
            $res << x.inspect
          end
        end
        "##,
    );
    run_test_with_prelude(
        r##"
        $res = ""
        C.new.foo(a:42, 100 => 100, c:5)
        $res
        "##,
        r##"
        class C
          def foo(*x, **y)
            $res << x.inspect + " " + y.inspect
          end
        end
        "##,
    );
}

#[test]
fn keyword_rest() {
    run_test_with_prelude(
        r#"
        [f{1}, f(a:18, c:4, z:6){2}, f(b:2, **{:z=>7, 5=>2}){3}]
        "#,
        r#"
        def f(a:1,b:2,**c,&block)
          [a, b, c, block.call]
        end
        "#,
    );
    run_test_error("def f(**a, **b); end");
}

// Ruby 3.1+ shorthand keyword argument syntax: `foo(handled:)` is
// equivalent to `foo(handled: handled)`. Prism wraps the implicit value
// in an `ImplicitNode` that the lowerer must unwrap.
#[test]
fn keyword_shorthand() {
    run_test_with_prelude(
        r#"
        handled = 42
        f(handled:)
        "#,
        r#"
        def f(handled:)
          handled
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        a = 1
        b = 2
        f(a:, b:)
        "#,
        r#"
        def f(a:, b:)
          [a, b]
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        x = 10
        z = 30
        f(1, x:, y: 20, z:)
        "#,
        r#"
        def f(p, x:, y:, z:)
          [p, x, y, z]
        end
        "#,
    );
    // Shorthand referencing a method, not a local variable.
    run_test_with_prelude(
        r#"
        f(name:)
        "#,
        r#"
        def name; "ruby"; end
        def f(name:); name; end
        "#,
    );
    // Hash literal: `{x:, y:}` desugars to `{x: x, y: y}`.
    run_test(
        r#"
        x = "hello"
        y = 99
        {x:, y:}
        "#,
    );
}

#[test]
fn keyword_to_hash() {
    run_test_with_prelude(
        r##"
        f("a", 42, 1000, x:100, y:200)
        "##,
        r##"
        def f(a, b, *c)
          "a:#{a} b:#{b} c:#{c}"
        end
        "##,
    );
    run_test_with_prelude(
        r##"
        f("a", x:100, y:200)
        "##,
        r##"
        def f(a, b)
          "a:#{a} b:#{b}"
        end
        "##,
    );
    run_test_with_prelude(
        r##"
        f do |a,b,*c| "a:#{a} b:#{b} c:#{c}" end
        "##,
        r##"
        def f
          yield 1,2,3,x:100,y:200
        end
        "##,
    );
    run_test_with_prelude(
        r##"
        f do |a,b,c| "a:#{a} b:#{b} c:#{c}" end
        "##,
        r##"
        def f
          yield 1,x:100,y:200
        end
        "##,
    );
    run_test_error(
        r##"
        def f(a, b, c)
          "a:#{a} b:#{b} c:#{c}"
        end
        f("a", 42, 1000, x:100, y:200)
        "##,
    );
}

#[test]
fn splat() {
    run_test_with_prelude(
        r#"
        f(*[0,1,2,3,4,5,6,7,8])
        "#,
        r#"
        def f(*x)
          x
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f(*[0,1,2,3])
        "#,
        r#"
        def f(a,b,c=12,d=23)
          a+b+c+d
        end
        "#,
    );
    run_test_with_prelude(
        r#"
        f([1,2,3,4,5]) {|a,b,c,d,e| a+b+c+d+e}
        "#,
        r#"
        def f(x)
          yield *x
        end
        "#,
    );
    run_test_error(
        r##"
    class C
        def to_a
            1
        end
    end
    [*C.new]
    "##,
    );
    run_test_error(
        r##"
    class C
        def to_a
            1
        end
    end
    puts *C.new
    "##,
    );
}

#[test]
fn splat2() {
    run_test(
        r##"
        a = [1,2]
        b = []
        7.times do
            b.prepend(*a)
        end
        b
        "##,
    )
}

#[test]
fn hash_splat() {
    run_test_with_prelude(
        r##"
            f(**{a:1})
        "##,
        r##"
        def f(*x)
            x
        end
        "##,
    )
}

#[test]
fn hash_splat2() {
    run_test_with_prelude(
        r##"
            f()
        "##,
        r##"
        def f(*x)
            x
        end
        "##,
    )
}

#[test]
fn hash_splat3() {
    run_test_with_prelude(
        r##"
            f(a:1, **{b:2,c:3})
        "##,
        r##"
        def f(*x)
            x
        end
        "##,
    )
}

#[test]
fn hash_splat4() {
    run_test_with_prelude(
        r#"
            f(1,2,d:4,a:1,**{c:3,b:2})
        "#,
        r#"
            def f(x,y,a:100,b:200,c:300,d:400)
              [a,b,c,d,x,y]
            end
        "#,
    );
    run_test_with_prelude(
        r#"
            f(1,2,**{c:3,b:2})
        "#,
        r#"
            def f(x,y,a:100,b:200,c:300,d:400)
              [a,b,c,d,x,y]
            end
        "#,
    );
    run_test_with_prelude(
        r#"
            f(1,2,**{c:3,b:2})
        "#,
        r#"
            def f(x,y,b:200,c:300)
              [b,c,x,y]
            end
        "#,
    );
}

#[test]
fn invoker_ruby() {
    run_test_with_prelude(
        r#"
        C.new.send(:foo, 1, 2, a:3, b:5)
        "#,
        r##"
        class C
          def foo(*x, a:100, **y)
            x.inspect + a.inspect + y.inspect
          end
        end
        "##,
    );
    run_test_error(
        r##"
        class C
            def foo(*x, a:100)
                x.inspect + a.inspect
            end
        end

        C.new.send(:foo, 1, 2, a:3, b:5)
        "##,
    );
}

#[test]
fn forwarding1() {
    run_test_with_prelude(
        r#"
        $res = []
        D.new.f(1,*[2,3],e:70,**{f:80, g:90})
        $res
        "#,
        r##"
        class C
          def f(*rest, **kw)
            $res << [rest, kw]
          end
        end

        class D < C
          def f(a,...)
            a = 50
            super
          end
        end
        "##,
    );
}

#[test]
fn forwarding2() {
    run_test_with_prelude(
        r##"
        $res = []
        C.new.f(1,*[2,3],e:70,**{f:80, g:90})
        $res
        "##,
        r##"
        class C
          def g(*rest, **kw)
            $res << "#{rest} #{kw}"
          end
                
          def f(a,b,...)
            a = 50
            g(1,2,a:100)
            g(...)
            g(b,a,...)
          end
        end
        "##,
    );
}

#[test]
fn forwarding3() {
    run_test_with_prelude(
        r##"
        $res = []
        C.new.f(1,2,a:3)
        C.new.f(1,2,a:3) { 100 }
        $res
        "##,
        r##"
        class C
          def g(*rest, **kw)
            $res << "#{rest} #{kw}"
            $res << "#{yield}" if block_given?
          end
                
          def f(...)
            g(...)
            g(10,...)
          end
        end
        "##,
    );
}

#[test]
fn forwarding_super() {
    run_test_with_prelude(
        r##"
        $res = []
        C.new.f(1,2,a:3)
        C.new.f(1,2,a:3) { 100 }
        $res
        "##,
        r##"
        class S
          def f(*rest, **kw)
            $res << "#{rest} #{kw}"
            $res << "#{yield}" if block_given?
          end
        end

        class C < S     
          def f(...)
            super
            super(10,...)
          end
        end
        "##,
    );
}

#[test]
fn anonymous_block_forwarding1() {
    run_test(
        r##"
        def bar(&)
          yield 42
        end

        def foo(&)
          bar(&)
        end

        foo { |x| x * 2 }
        "##,
    );
}

#[test]
fn anonymous_block_forwarding2() {
    run_test(
        r##"
        def baz(&)
          if block_given?
            yield 10
          else
            "no block"
          end
        end

        [baz { |x| x + 1 }, baz]
        "##,
    );
}

#[test]
fn anonymous_block_forwarding3() {
    run_test_with_prelude(
        r##"
        $res = []
        C.new.f { |x| x * 3 }
        $res
        "##,
        r##"
        class C
          def g(&)
            $res << yield(7)
          end

          def f(&)
            g(&)
          end
        end
        "##,
    );
}

#[test]
fn anonymous_block_forwarding4() {
    run_test(
        r##"
        def inner(&)
          yield 1, 2, 3
        end

        def middle(&)
          inner(&)
        end

        def outer(&)
          middle(&)
        end

        outer { |a, b, c| a + b + c }
        "##,
    );
}

#[test]
fn destruct() {
    run_test_with_prelude(
        r#"
        f(1,[2,3,4],5,[6])
        "#,
        r#"
        def f(a,(b,c),d,(e,f))
          g = 42
          [a,b,c,d,e,f,g]
        end
        "#,
    );
}

#[test]
fn block_param() {
    run_test_with_prelude(
        r#"
        f do |a,b|
            a + b + h
        end
        "#,
        r#"
        def g
            yield 1,2
        end

        def h
            yield 1,3,5
        end
          
        def f(&p)
            [g(&p), h(&p)]
        end
        h = 39
        "#,
    );
}

#[test]
fn block_param2() {
    run_test_with_prelude(
        r#"
        f{}
        "#,
        r#"
        def f(&p)
            p.call
        end
        "#,
    );
}

#[test]
fn method_error() {
    run_test_error(
        r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        f(1)
        "#,
    );
    run_test_error(
        r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        f(1,2,3,4,5)
        "#,
    );
    run_test_error(
        r#"
        def f
        end
        f(1,2,3,4,5)
        "#,
    );
    run_test_error(
        r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        10.times {|x|
            if x == 9
                f(1)
            else
                f(1,2)
            end
        }
        "#,
    );
    run_test_error(
        r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        10.times {|x|
            if x == 9
                f(1,2,3,4,5)
            else
                f(1,2)
            end
        }
        "#,
    );
    run_test_error(
        r#"
        def f
        end
        10.times {|x|
            if x == 9
                f(1)
            else
                f
            end
        }
        "#,
    );
}

#[test]
fn block_call1() {
    run_test_with_prelude(
        r#"
        f {|a,b|
          e=42
          [a,b,e]
        }
        "#,
        r#"
        def f
          yield 1,2,3,4
        end
        "#,
    );
}

#[test]
fn block_call2() {
    run_test_with_prelude(
        r#"
        f {|a,b,c,d|
          e=42
          [a,b,c,d,e]
        }
        "#,
        r#"
        def f
          yield 1,2
        end
        "#,
    );
}

#[test]
fn block_nest1() {
    run_test_with_prelude(
        r#"
        f {|a,(b,c,d),e,f|
            [a,b,c,d,e,f]
        }
        "#,
        r#"
        def f
          yield 1,[2,3],4
        end
        "#,
    );
}

#[test]
fn block_nest2() {
    run_test_with_prelude(
        r#"
                f {|a,(b)|
                    [a,b]
                }
                "#,
        r#"
                def f
                  yield 1,[2,3],4
                end
                "#,
    );
}

#[test]
fn block_array_expand1() {
    run_test_with_prelude(
        r#"
            f { |a,(b,c),d|
                e = 100
                [a,b,c,d,e]
            }
            "#,
        r#"
            def f
                yield [1,[2,3],4]
            end
            "#,
    );
}

#[test]
fn block_array_expand2() {
    run_test_with_prelude(
        r#"
            f { |a,b|
                c = 42
                [a,b,c]
            }
            "#,
        r#"
            def f
              yield [1,[2,3],4]
            end
            "#,
    );
}

#[test]
fn block_array_expand3() {
    run_test_with_prelude(
        r#"
            f { |a, *b|
                [a, b]
            }
            "#,
        r#"
            def f
            	yield [1,2,3]
            end
            "#,
    );
}

#[test]
fn block_optional() {
    run_test_with_prelude(
        r#"
        f { |a,b,c=42|
          [a,b,c]
        }
        "#,
        r#"
        def f
          yield [1,2]
        end
        "#,
    );
}

#[test]
fn nested_blockargproxy() {
    run_test_with_prelude(
        r#"
        $x = 0
        g { 42 }
        $x
        "#,
        r#"
        def e
          10.times do
            $x += yield
          end
        end

        def f(&q)
          10.times do
            e(&q)
          end
        end

        def g(&p)
          10.times do
            10.times do
              f(&p)
            end
          end
        end
        "#,
    );
}

#[test]
fn block_arg() {
    run_test_with_prelude(
        r##"
        $x = []
        f { 100 }
        p = Proc.new { 200 }
        f(&p)
        $x
    "##,
        r##"
        def f(&p)
            g(&p)
        end
                
        def g(&p)
            $x << yield
        end
    "##,
    );
    run_test_with_prelude(
        r##"
        $x = []
        f { 100 }
        p = Proc.new { 200 }
        f(&p)
        $x
    "##,
        r##"
        def f(&p)
            g(&p)
        end
                
        def g(&p)
            $x << p.call
        end
    "##,
    );
    run_test_with_prelude(
        r##"
        $res = []
        a = 77
        1.times do
            $res << foo do |k,v|
              $res << [k, v, E, a]
            end
        end
        $res
        "##,
        r##"
        E = 100
        def foo(&block)
          a = 1
          loop do
            {a:1,b:2}.each &block
            a += 1
            if a > 50
              return a
            end
          end
        end
"##,
    );
}

#[test]
fn nested_call_opt() {
    run_test_with_prelude(
        r#"
            a = [1,2,3,4,5]
            [f(100), f(*a)]
        "#,
        r#"
            def f(*x); x; end
        "#,
    );
}

#[test]
fn rest_discard() {
    run_test_with_prelude(
        r#"
            [f(1,2), f(1,2,3,4,5)]
        "#,
        r#"
            def f(a,b,*)
              [a,b]
            end
        "#,
    );
}

#[test]
fn safe_nav_operator() {
    run_test(
        r#"
        a = [1,2,3,nil] * 5
        x = []
        for e in a
          x << e&.nil?
        end
        x
        "#,
    );
    run_test(
        r#"
        def f(x)
          x&.nil?
        end

        a = [1,2,3,nil] * 5
        x = []
        for e in a
          x << f(e)
        end
        x
        "#,
    );
}

#[test]
fn test_super() {
    run_test_with_prelude(
        r#"
            D.new.f(42, 100)
        "#,
        r#"
            class C
                def f(x,y,z,a:1000)
                    x+y+z+a
                end
            end

            class D < C
                def f(x,y,z=10,a:77)
                    super x,y,z,a:a
                end
            end
        "#,
    );

    run_test(
        r#"
        $res = []

        class S
            def f(x)
                $res << x
            end
        end

        class C < S
            def f(x)
                3.times do
                    super x
                end
            end
        end

        C.new.f(200)
        $res
        "#,
    );

    run_test(
        r#"
        class S
          def f(x,y,z)
            x + y + z
          end
        end
                
        class C < S
          def f(*x)
            super *x
          end
        end
                
        C.new.f(3,4,5)
        "#,
    )
}

#[test]
fn test_super2() {
    run_test_with_prelude(
        r##"
            $res = []
            D.new.f(1,[2,3],f:70)
            $res
                "##,
        r##"
            class C
              def f(a,(b,c),d,e:30,f:40)
                $res << [a,b,c,d,e,f]
              end
            end

            class D < C
              def f(a,(b,c),d=100,e:42,f:10)
                a = 100
                c = 50
                e = 200
                super
                1.times do
                    super
                end
              end
            end
            "##,
    );

    run_test_with_prelude(
        r#"
        $res = []
        C.new.f(200, 300, z:400)
        $res
        "#,
        r#" 
        class S
            def f(x, y, z:10)
                $res << x
                $res << y
                $res << z
            end
        end

        class C < S
            def f(x, y, z:50)
                super
                1.times do
                    super
                end
            end
        end
            "#,
    );

    run_test_with_prelude(
        r#"
        $res = []
        C.new.f(200, 300, a:250)
        $res
        "#,
        r#" 
        class S
            def f(x, y, a:10, b:20)
                $res << x
                $res << y
                $res << a
                $res << b
            end
        end

        class C < S
            def f(x, y, a:50, b:60)
                super
                1.times do
                    super
                end
            end
        end
            "#,
    );

    run_test_with_prelude(
        r#"
        $res = []
        C.new.f(100, 200, 300, a:250, c:400)
        $res
        "#,
        r#" 
        class S
            def f(*x, **y)
                $res << x
                $res << y.sort
            end
        end

        class C < S
            def f(x, *rest, a:50, **kw)
                super
                1.times do
                    super
                end
            end
        end
            "#,
    );

    run_test_with_prelude(
        r#"
        $res = []
        C.new.f(100, 200, 300, a:250, c:400)
        $res
        "#,
        r#" 
        class S
            def f(x, y, z, a:0, c:0)
                $res << x
                $res << y
                $res << z
                $res << a
                $res << c
            end
        end

        class C < S
            def f(*rest, **kw)
                super
                1.times do
                    super
                end   
            end
        end
            "#,
    );

    run_test_with_prelude(
        r#"
        $res = []
        C.new.f(200, 300, a:250)
        $res
        "#,
        r#" 
        class S
            def f(x, y, a:10, b:20)
                $res << x
                $res << y
                $res << a
                $res << b
            end
        end

        class C < S
            def f(x, y, a:50, b:60)
                super
                1.times do
                    super
                end
            end
        end
            "#,
    );

    run_test_with_prelude(
        r##"
        $res = []
        D.new.f(1,*[2,3],e:70,**{f:80, g:90})
        $res
        "##,
        r##"
        class C
          def f(*rest, **kw)
            $res << [rest, kw.sort]
          end
        end

        class D < C
          def f(a,b,...)
            a = 50
            super
            super(...)
            super(b,...)
          end
        end
        "##,
    );
}

// tests for JIT compiler

#[test]
fn polymorphic() {
    run_test_with_prelude(
        r##"
        res = []
                
        a = [C1.new, C1.new, C1.new, C1.new, C.new, C.new]
        for i in 0..a.length - 1
          res << a[i].f
        end
                
        a = [C.new, C.new, C.new, C.new, C1.new, C1.new]
        for i in 0..a.length - 1
          res << a[i].f
        end
                
        res
        "##,
        r##"
        class C
          attr_accessor :a
          def initialize
            @a=10
          end
          def f
            @a
          end
        end

        class C1 < C
          attr_accessor :a
          def initialize
            @a=20
          end
        end
        "##,
    );
}

#[test]
fn yield_test() {
    run_test(
        r##"
          def f(x,y)
            yield x,y
          end
          
          res = []
          for i in 0..10
            res << f(i,5) {|x,y| x+y}
            res << f(i,8) {|x,y| x+y}
          end
          res
        "##,
    );
}

#[test]
fn iterator() {
    run_test(
        r##"
        class Array
          def iich
            for i in 0...self.size
              yield(self[i])
            end
          end
        end

        a = []
        [2,5,7,10,2.2,7,9].iich do |x|
          a << x*2
        end
        a
        "##,
    );
}

#[test]
fn attr_accessor() {
    run_test_with_prelude(
        r##"
            x = [C.new, B.new, A.new]
            res = []
            for e in x
                e.a += 1000.0
                e.b += 1000.0
                e.c += 1000.0
                res << e.a
                res << e.b
                res << e.c
            end
            res
            "##,
        r##"
            class C
              def initialize
                @a = 1
                @b = 2
                @c = 3
              end
              attr_accessor :a, :b, :c
            end
            class B < C
              def initialize
                @b = 10
                @c = 20
                @a = 30
              end
              attr_accessor :a, :b, :c
            end
            class A < B
              def initialize
                @c = 100
                @a = 200
                @b = 300
              end
              attr_accessor :a, :b, :c
            end
        "##,
    );
}

#[test]
fn jit_attr_reader() {
    run_test_with_prelude(
        r###"
        x = C.new
        [x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h]
        "###,
        r###"
        class C
          attr_reader :a, :b, :c, :d, :e, :f, :g, :h
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
        end
        "###,
    );
    run_test_with_prelude(
        r###"
        x = C.new
        [x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h]
        "###,
        r###"
        class C < Array
          attr_reader :a, :b, :c, :d, :e, :f, :g, :h
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
        end
        "###,
    );
}

#[test]
fn deopt_method_recv_class() {
    run_test_error(
        r##"
          class A
            def w
              42
            end
          end
          class B
          end
          a = A.new
          res = []
          for i in 0..10
            if i == 8
              a = B.new
            end
            res << a.w
          end
          res
        "##,
    );
}

#[test]
fn deopt_reader_recv_class() {
    run_test(
        r##"
            class A
                attr_accessor :w
            end
            class B
              def w
                100
              end
            end
            a = A.new
            a.w = 42
            res = []
            for i in 0..10
              if i == 8
                a = B.new
              end
              res << a.w
            end
            res
        "##,
    );
}

#[test]
fn deopt_writer_recv_class() {
    run_test(
        r##"
            class A
              attr_accessor :w
            end
            class B
              attr_reader :w
              def w=(v)
                @w = v * 2
              end
            end
            a = A.new
            res = []
            for i in 0..10
              if i == 8
                a = B.new
              end
              a.w = 42
              res << a.w
            end
            res
        "##,
    );
}

#[test]
fn deopt_reader_class_version() {
    run_test(
        r##"
        class A
          attr_accessor :w
        end
        a = A.new
        a.w = 42
        res = []
        for i in 0..10
          if i == 8
            class A
              def w
                99
              end
            end
          end
          res << a.w
        end
        res
        "##,
    );
}

#[test]
fn deopt_writer_class_version() {
    run_test_once(
        r##"
        class A
          attr_accessor :w
        end
        a = A.new
        res = []
        for i in 0..10
          if i == 8
            class A
              def w=(v)
                @w = v * 2
              end
            end
          end
          a.w = 42
          res << a.w
        end
        res
        "##,
    );
}

#[test]
fn attr_reader_in_different_class() {
    run_test_with_prelude(
        r##"
            s = S.new
            c = C.new
            [s.a, s.b, s.c, s.d, s.e, s.f, s.g, s.h, c.a, c.b, c.c, c.d, c.e, c.f, c.g, c.h]
        "##,
        r##"
            class S
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                attr_reader :a, :b, :c, :d, :e, :f, :g, :h
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
                attr_reader :a, :b, :c, :c, :e, :f, :g, :h
            end
            
            "##,
    );
    run_test_with_prelude(
        r##"
            s = S.new
            c = C.new
            [s.a, s.b, s.c, s.d, s.e, s.f, s.g, s.h, c.a, c.b, c.c, c.d, c.e, c.f, c.g, c.h]
        "##,
        r##"
            class S < Array
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                attr_reader :a, :b, :c, :d, :e, :f, :g, :h
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
                attr_reader :a, :b, :c, :c, :e, :f, :g, :h
            end
            
            "##,
    );
}

#[test]
fn flow_control() {
    run_test_with_prelude(
        r##"
    $i = []

    foo do |i|
      $i << i
      next if i < 5
      $i << "break:#{i}"
      break
    end

    $i << "done: main"
    $i
    "##,
        r##"
    def bar(&b)
      10.times &b
      $i << "done: bar"
    end

    def foo(&b)
      bar &b
      $i << "done: foo"
    end
    "##,
    );
}

#[test]
fn define_unary() {
    run_test_with_prelude(
        r##"
        c = C.new
        [-c, +c, ~c, c.-@, c.+@, c.~]
        "##,
        r##"
        class C
          def -@
            "-"
          end
            
          def +@
            "+"
          end
            
          def ~
            "~"
          end
        end
        "##,
    );
}

#[test]
fn define_index() {
    run_test_with_prelude(
        r##"
        c = C.new
        [c[42], c[10] = 100]
        "##,
        r##"
        class C
          def [](idx)
            idx.to_s
          end
            
          def []=(idx, val)
            idx.to_s + val.to_s
          end
        end
        "##,
    );
}

#[test]
fn comment_in_method_chain() {
    run_test(
        r##"
        [1,2,3].
          # this is a comment
          map {|x| x * 2}.
          # another comment
          select {|x| x > 2}
        "##,
    );
}

#[test]
fn method_missing_basic() {
    run_test_with_prelude(
        r#"
        C.new.hello
        "#,
        r#"
        class C
          def method_missing(name, *args)
            "missing: #{name}"
          end
        end
        "#,
    );
}

#[test]
fn method_missing_with_args() {
    run_test_with_prelude(
        r#"
        C.new.foo(1, 2, 3)
        "#,
        r#"
        class C
          def method_missing(name, *args)
            [name, args]
          end
        end
        "#,
    );
}

#[test]
fn method_missing_with_splat() {
    run_test_with_prelude(
        r#"
        a = [2, 3]
        C.new.foo(1, *a, 4)
        "#,
        r#"
        class C
          def method_missing(name, *args)
            [name, args]
          end
        end
        "#,
    );
}

#[test]
fn method_missing_with_kwargs() {
    run_test_with_prelude(
        r#"
        C.new.foo(1, x: 10, y: 20)
        "#,
        r#"
        class C
          def method_missing(name, *args, **kwargs)
            [name, args, kwargs]
          end
        end
        "#,
    );
}

#[test]
fn method_missing_with_hash_splat() {
    run_test_with_prelude(
        r#"
        h = {a: 1, b: 2}
        C.new.bar(**h)
        "#,
        r#"
        class C
          def method_missing(name, *args, **kwargs)
            [name, args, kwargs]
          end
        end
        "#,
    );
}

#[test]
fn method_missing_via_undef() {
    run_test_with_prelude(
        r#"
        C.new.foo(10, 20)
        "#,
        r#"
        class C
          def foo(*args)
            args
          end
          undef_method :foo
          def method_missing(name, *args)
            "undef caught: #{name} #{args}"
          end
        end
        "#,
    );
}

#[test]
fn method_missing_hook_via_singleton_class() {
    run_test_once(
        r#"
        $result = []
        class C
          class << self
            undef_method :singleton_method_added
            def method_missing(name, *args)
              $result << [name, args]
              nil
            end
          end
          def self.foo; end
        end
        $result
        "#,
    );
}

#[test]
fn forwarding_specialized_inline() {
    // required-only callee, pure `...` forwarding: hits the
    // SetArgumentsForwarded fast path, inline-array branch (<=5 args).
    run_test_with_prelude(
        r#"
        $r = []
        10.times { $r << f(1, 2, 3) }
        $r << f(-7, 8, 99)
        $r
        "#,
        r#"
        def g(a, b, c) = a * 100 + b * 10 + c
        def f(...) = g(...)
        "#,
    );
}

#[test]
fn forwarding_specialized_heap() {
    // 7 required params > ARRAY_INLINE_CAPA(5): exercises the heap
    // (RVALUE_OFFSET_HEAP_PTR/HEAP_LEN) branch of the copy.
    run_test_with_prelude(
        r#"
        $r = []
        30.times { |i| $r << f(i, i+1, i+2, i+3, i+4, i+5, i+6) }
        $r
        "#,
        r#"
        def g(a, b, c, d, e, f, g) = [a, b, c, d, e, f, g].sum
        def f(...) = g(...)
        "#,
    );
}

#[test]
fn forwarding_specialized_zero_arity() {
    // g_arity == 0: empty forwarded array, copy loop skipped.
    run_test_with_prelude(
        r#"
        $r = []
        30.times { $r << f }
        $r
        "#,
        r#"
        def g = 12345
        def f(...) = g(...)
        "#,
    );
}

#[test]
fn forwarding_specialized_arity_mismatch_falls_back() {
    // Length guard miss must fall back to the generic path and raise
    // ArgumentError exactly like CRuby (no crash / no silent accept).
    run_test_with_prelude(
        r#"
        $r = []
        30.times do
          begin
            f(1, 2)
          rescue ArgumentError => e
            $r << e.class.name
          end
        end
        $r
        "#,
        r#"
        def g(a, b, c) = a + b + c
        def f(...) = g(...)
        "#,
    );
}

#[test]
fn forwarding_specialized_kwargs_falls_back() {
    // Non-nil forwarded kw-rest must take the fallback; CRuby raises
    // ArgumentError because `g` accepts no keywords.
    run_test_with_prelude(
        r#"
        $r = []
        30.times do
          $r << f(7)
          begin
            f(7, kw: 1)
          rescue ArgumentError, TypeError => e
            $r << e.class.name
          end
        end
        $r
        "#,
        r#"
        def g(a) = a * 3
        def f(...) = g(...)
        "#,
    );
}

#[test]
fn forwarding_specialized_block_passthrough() {
    // Forwarded block must still reach the callee on the fast path.
    run_test_with_prelude(
        r#"
        $r = []
        30.times { $r << f(5) { |x| x + 1 } }
        $r
        "#,
        r#"
        def g(a) = yield(a * 2)
        def f(...) = g(...)
        "#,
    );
}

#[test]
fn forwarding_leading_arg() {
    // `g(x, ...)` with required-only `g`: trailing single splat
    // (`splat_pos == [pos_num-1]`); hits the specialized path with
    // lead_num == 1.
    run_test_with_prelude(
        r#"
        $r = []
        30.times { $r << f(2, 3, 4) }
        $r
        "#,
        r#"
        def g(a, b, c, d) = [a, b, c, d]
        def f(...) = g(10, ...)
        "#,
    );
}

#[test]
fn forwarding_leading_args_multi_and_heap() {
    // 3 leading args + a 7-element `...` (> ARRAY_INLINE_CAPA(5), so the
    // heap branch of the rest copy is taken); g has 10 required params
    // (3 + 7 == 10).
    run_test_with_prelude(
        r#"
        $r = []
        30.times { |i| $r << f(i, i+1, i+2, i+3, i+4, i+5, i+6) }
        $r
        "#,
        r#"
        def g(a, b, c, d, e, f, g, h, i, j) = [a, b, c, d, e, f, g, h, i, j].sum
        def f(...) = g(100, 200, 300, ...)
        "#,
    );
}

#[test]
fn forwarding_leading_args_empty_rest() {
    // All required slots come from leading args; the `...` array is
    // empty (expected_len == 0, copy loop skipped).
    run_test_with_prelude(
        r#"
        $r = []
        30.times { $r << f }
        $r
        "#,
        r#"
        def g(a, b, c) = a * 100 + b * 10 + c
        def f(...) = g(7, 8, 9, ...)
        "#,
    );
}

#[test]
fn forwarding_leading_args_too_many_falls_back() {
    // More leading args than `g`'s arity (req_num+1 < pos_num): gate
    // must reject; CRuby raises ArgumentError, generic path must match.
    run_test_with_prelude(
        r#"
        $r = []
        30.times do
          begin
            f(1)
          rescue ArgumentError => e
            $r << e.class.name
          end
        end
        $r
        "#,
        r#"
        def g(a, b) = a + b
        def f(...) = g(10, 20, 30, ...)
        "#,
    );
}

#[test]
fn forwarding_leading_args_block_and_mismatch() {
    // Leading arg + forwarded block on the fast path, plus a
    // length-guard miss (wrong rest count) that must fall back and
    // raise ArgumentError exactly like CRuby.
    run_test_with_prelude(
        r#"
        $r = []
        30.times do
          $r << f(3, 4) { |x| x + 1 }
          begin
            f(3) { |x| x }
          rescue ArgumentError => e
            $r << e.class.name
          end
        end
        $r
        "#,
        r#"
        def g(a, b, c) = yield(a + b + c)
        def f(...) = g(1, ...)
        "#,
    );
}

#[test]
fn forwarding_specialized_chain_and_mutation() {
    // Deep forwarding chain + ensure the forwarded array elements are
    // copied by value (callee mutation must not corrupt the caller).
    run_test_with_prelude(
        r#"
        $r = []
        30.times do
          a = [1, 2, 3]
          $r << h(*a)
          $r << a
        end
        $r
        "#,
        r#"
        def g(x, y, z)
          x, y, z = z, x, y
          x * 100 + y * 10 + z
        end
        def f(...) = g(...)
        def h(...) = f(...)
        "#,
    );
}

#[test]
fn forwarding_rest_pure() {
    // `def g(a, *r)` via pure `g(...)`: rest-array alloc unavoidable,
    // handled by the specialized runtime helper.
    run_test_with_prelude(
        r##"
        $r = []
        30.times { $r << f(1, 2, 3, 4) }
        $r << f(9)
        $r
        "##,
        r##"
        def g(a, *r) = "#{a}|#{r.inspect}"
        def f(...) = g(...)
        "##,
    );
}

#[test]
fn forwarding_rest_only_and_leading() {
    // `def g(*r)` (no required) and leading-arg `k(100, 200, ...)`.
    run_test_with_prelude(
        r##"
        $r = []
        30.times { |i| $r << [g_all(i, i+1), h_lead(i)] }
        $r
        "##,
        r##"
        def g(*r) = r.sum
        def k(a, b, *r) = a * 1000 + b * 100 + r.sum
        def g_all(...) = g(...)
        def h_lead(...) = k(100, 200, ...)
        "##,
    );
}

#[test]
fn forwarding_opt_post_rest() {
    // Opt + post + rest callee through forwarding (fill_positional_args
    // generic shape, driven by the helper's direct buffer).
    run_test_with_prelude(
        r##"
        $r = []
        30.times do |i|
          $r << a(i, i+1)
          $r << a(i, i+1, i+2, i+3, i+4)
          $r << b(1, 2, 3, 4, 5, 6, 7)
        end
        $r
        "##,
        r##"
        def opt(x, y = 10, *rest, z) = "#{x},#{y},#{rest.inspect},#{z}"
        def post(p, *mid, q, r) = "#{p}|#{mid.inspect}|#{q}|#{r}"
        def a(...) = opt(...)
        def b(...) = post(50, ...)
        "##,
    );
}

#[test]
fn forwarding_rest_kwargs_delegates() {
    // Keywords actually forwarded into a `*rest` callee: the helper
    // delegates to the proven generic path; must match CRuby (kw hash
    // becomes a trailing rest element).
    run_test_with_prelude(
        r##"
        $r = []
        30.times do
          $r << g(1, 2)
          $r << g(1, k: 9, m: 8)
        end
        $r
        "##,
        r##"
        def real(a, *r) = "#{a}|#{r.inspect}"
        def g(...) = real(...)
        "##,
    );
}

#[test]
fn forwarding_rest_block_passthrough() {
    // Forwarded block must still reach a rest-bearing callee on the
    // helper path.
    run_test_with_prelude(
        r##"
        $r = []
        30.times { $r << g(2, 3, 4) { |x| x * 10 } }
        $r
        "##,
        r##"
        def real(a, *r) = yield(a + r.sum)
        def g(...) = real(...)
        "##,
    );
}

#[test]
fn forwarding_rest_chain_and_mutation() {
    // Deep chain + callee mutates its rest array; the forwarded source
    // array must be unaffected.
    run_test_with_prelude(
        r##"
        $r = []
        30.times do
          src = [10, 20, 30]
          $r << h(*src)
          $r << src
        end
        $r
        "##,
        r##"
        def real(a, *r)
          r << 999
          "#{a}:#{r.inspect}"
        end
        def f(...) = real(...)
        def h(...) = f(...)
        "##,
    );
}

#[test]
fn forwarding_super_rest_post() {
    // Implicit `super` of `def m(a, *r, z)`: single splat *before* a
    // post param (splat_pos == [rest_pos], not trailing). Previously
    // fell to the generic path; now the single-splat helper handles it.
    run_test_with_prelude(
        r##"
        $r = []
        o = B.new
        30.times { $r << o.m(1, 2, 3, 4, 5) }
        $r << o.m(7, 8)
        $r
        "##,
        r##"
        class A
          def m(a, *r, z) = "A #{a}|#{r.inspect}|#{z}"
        end
        class B < A
          def m(a, *r, z) = "B->" + super
        end
        "##,
    );
}

#[test]
fn forwarding_super_rest_last() {
    // Implicit `super` of `def m(a, *r)` (rest is the last positional):
    // trailing single splat, already specialized — regression guard.
    run_test_with_prelude(
        r##"
        $r = []
        o = B.new
        30.times { $r << o.m(1, 2, 3, 4) }
        $r << o.m(9)
        $r
        "##,
        r##"
        class A
          def m(a, *r) = "A #{a}|#{r.inspect}"
        end
        class B < A
          def m(a, *r) = "B->" + super
        end
        "##,
    );
}

#[test]
fn forwarding_super_opt_rest_post() {
    // Implicit `super` of `def m(a, b=10, *r, y, z)`: opt + rest + post,
    // single splat in the middle, exercised at several arities.
    run_test_with_prelude(
        r##"
        $r = []
        o = B.new
        30.times do |i|
          $r << o.m(i, i+1, i+2, i+3)
          $r << o.m(1, 2, 3, 4, 5, 6, 7, 8)
        end
        $r
        "##,
        r##"
        class A
          def m(a, b = 10, *r, y, z) = "#{a},#{b},#{r.inspect},#{y},#{z}"
        end
        class B < A
          def m(a, b = 10, *r, y, z) = "B[" + super + "]"
        end
        "##,
    );
}

#[test]
fn forwarding_super_no_rest_passthrough() {
    // Implicit `super` of a fixed-arity method (no splat) must keep
    // working (is_simple path) — regression guard.
    run_test_with_prelude(
        r##"
        $r = []
        o = B.new
        30.times { $r << o.m(3, 4) }
        $r
        "##,
        r##"
        class A
          def m(a, b) = a * 10 + b
        end
        class B < A
          def m(a, b) = super + 1000
        end
        "##,
    );
}

#[test]
fn forwarding_super_block_passthrough() {
    // A block given to the subclass method must reach `super`.
    run_test_with_prelude(
        r##"
        $r = []
        o = B.new
        30.times { $r << o.m(2, 3, 4) { |x| x * 100 } }
        $r
        "##,
        r##"
        class A
          def m(a, *r) = yield(a + r.sum)
        end
        class B < A
          def m(a, *r) = super
        end
        "##,
    );
}
