extern crate monoruby;
use monoruby::tests::*;

// Regression for a stage-1'' loop-entry adoption bug (#1196): the
// `adopt_deferred` arm read `FPReg >= PHYS_FPR_POOL` as "this id is a
// raw-f64 *home* the ledger issued", and force-bound it as `F` at the
// loop head. But that predicate says only "spill-resident", and the
// ordinary FP allocator spills into the same file once the pool runs
// out — so under pressure an ordinary loop-carried float was adopted as
// a home, and the loop body then read a slot the entry never
// established, carrying the previous iteration's values into the next.
//
// The shape below is so_mandelbrot's inner escape loop: a nested loop
// whose body carries five floats, re-initialized on every outer
// iteration. Under `--features stress-spill-pool` (a pool of 2, which
// CI runs) the miscompile counted `d == 0` for points that need two
// iterations to escape.
#[test]
fn nested_float_loop_carries_its_own_locals() {
    run_test(
        r#"
        def row
          res = []
          x = 0
          while x < 45
            a = 0.0
            b = 0.0
            c = x * 0.001 - 1.5
            d = 0
            while d <= 49
              t1 = a*a - b*b + c
              t2 = 2.0*a*b - 1.0
              a = t1
              b = t2
              break if (a*a+b*b) > 4.0
              d += 1
            end
            res << d
            x += 1
          end
          res
        end
        row
        "#,
    );
}

// The same shape with the escape recorded in a boolean the outer body
// re-initializes, which is how `benchmark/so_mandelbrot.rb` writes it.
#[test]
fn nested_float_loop_with_a_reset_flag() {
    run_test(
        r#"
        def row
          res = []
          x = 0
          while x < 60
            zr = 0.0
            zi = 0.0
            cr = x * 0.002 - 1.5
            ci = -1.0
            escape = false
            d = 0
            while d <= 49
              tr = zr*zr - zi*zi + cr
              ti = 2.0*zr*zi + ci
              zr = tr
              zi = ti
              if (zr*zr+zi*zi) > 4.0
                escape = true
                break
              end
              d += 1
            end
            res << [d, escape]
            x += 1
          end
          res
        end
        row
        "#,
    );
}
