//! A whole-compiled block body whose receiver-class guard goes polymorphic
//! after the compile.
//!
//! `recv_miss_recompile_target` used to return `None` for block ROOTs
//! (#1127-era: an early recompile path rebuilt a block under the method
//! argument convention), leaving a receiver-class-guard miss on a plain
//! deopt with no healing — a block compiled while a receiver was still
//! `nil` then deopted on every later execution, forever. dewasm DOOM's
//! terminal renderer (`prev_row[cx] == key`: nil on the first frame,
//! Integer after) paid ~296k such deopts a minute. Block ROOTs now take
//! the whole-recompile route; these tests pin the semantics of exactly
//! that shape — the block's own argument must survive the recompile (the
//! historical breakage), and the values must stay correct across the
//! heal.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn block_root_heals_after_nil_warmup() {
    run_test(
        r##"
        class R
          def initialize(n)
            @n = n
            @prev = Array.new(n)
          end
          def render(key)
            s = 0
            @n.times do |i|
              # nil == key during warmup, Integer == key forever after:
              # the block is whole-compiled with a NilClass guard that
              # must heal via recompile, and `i` (the block argument) is
              # live across the deopt site.
              next if @prev[i] == key
              @prev[i] = key
              s += i + 1
            end
            s
          end
        end
        r = R.new(40)
        t = 0
        120.times { |f| t += r.render(f / 13) }
        t
        "##,
    );
}

#[test]
fn block_argument_survives_recompile() {
    // The #1127 breakage shape: a block forwarding its arguments to a
    // constructor, with a receiver-class flip inside forcing the block
    // ROOT's whole recompile mid-run.
    run_test(
        r##"
        class Loc
          attr_reader :a, :b
          def initialize(a, b) = (@a = a; @b = b)
        end
        def probe(xs, key)
          out = []
          xs.each_with_index do |x, i|
            flag = x == key
            out << Loc.new(i, flag)
          end
          out
        end
        acc = 0
        src = Array.new(30)
        120.times do |f|
          src = Array.new(30, f) if f == 40
          probe(src, f).each { |l| acc += l.a + (l.b ? 1 : 0) }
        end
        acc
        "##,
    );
}
