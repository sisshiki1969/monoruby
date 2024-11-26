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
fn delegate1() {
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
fn delegate2() {
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
