#[cfg(test)]
mod test {
    use crate::tests::*;

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
    fn test_block_param() {
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
    fn test_block_param2() {
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
    fn test_block_call1() {
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
    fn test_block_call2() {
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
    fn test_block_nest1() {
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
    fn test_block_nest2() {
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
    fn test_block_array_expand1() {
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
    fn test_block_array_expand2() {
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
    fn test_block_array_expand3() {
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
    fn test_block_optional() {
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
}
