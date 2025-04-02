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
        foo do |k,v|
          $res << [k, v, E]
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
              break
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
