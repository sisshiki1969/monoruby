//! A float local that starts as a literal and is accumulated into by a
//! block the callee inlines: `s = 0.0; arr.each { |x| s += x }`.
//!
//! The literal gets a float home at the call, so the callee's loop
//! carries `s` unboxed (see `doc/fixpoint_engine_plan.md`). The slot of
//! `s` is then stale while the callee runs, and every way out of the loop
//! has to make it current again: the normal return, `break`, `return` and
//! `throw` from the block, an exception, a guard that fails in the middle.

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn accumulate_in_iterators() {
    run_test(
        r##"
        def sum_each(arr, k); s = 0.0; arr.each { |x| s += x * k }; s; end
        def sum_times(n); s = 0.0; h = 0.5; n.times { |i| s += i * h }; s; end
        def sum_upto(n); s = 0.0; 1.upto(n) { |k| x = (k - 0.5) / n; s += 4.0 / (1.0 + x * x) }; s / n; end
        def dot(a, b); s = 0.0; a.each_with_index { |x, i| s += x * b[i] }; s; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        r = nil
        40.times { r = [sum_each(a, 1.5), sum_times(30), sum_upto(30), dot(a, a)] }
        r + [sum_times(0), sum_each([], 1.5), sum_each([1, 2, 3], 2), sum_each([1.5, 2.5], 2r)]
        "##,
    );
}

#[test]
fn several_accumulators_and_nested_blocks() {
    run_test(
        r##"
        def nested(m); s = 0.0; w = 0.25; m.each { |row| row.each { |x| s += x * w } }; s; end
        def two(fs); pos = 0.0; vel = 1.0; dt = 0.01; fs.each { |f| vel += f * dt; pos += vel * dt }; [pos, vel]; end
        def minmax(arr); lo = 1.0e9; hi = -1.0e9; arr.each { |x| lo = x if x < lo; hi = x if x > hi }; [lo, hi]; end
        def inner(arr); s = 0.0; arr.each { |x| t = 0.0; 3.times { |i| t += x * i }; s += t }; s; end
        def mean(arr); s = 0.0; c = 0; arr.each { |x| s += x; c += 1 }; s / c; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        m = Array.new(6) { |i| Array.new(6) { |j| (i + j) * 0.1 } }
        r = nil
        40.times { r = [nested(m), two(a), minmax(a), inner(a), mean(a)] }
        r + [nested([[1.5], [], [2.5, 3.5]]), two([]), minmax([])]
        "##,
    );
}

#[test]
fn value_used_after_the_call() {
    run_test(
        r##"
        def after_use(arr); s = 0.0; t = 1.0; arr.each { |x| s += x; t *= 1.5 }; [s, t, s + t, s.to_s]; end
        def then_loop(arr); s = 0.0; arr.each { |x| s += x }; i = 0; while i < 10; s += 0.5; i += 1; end; s; end
        def in_loop(arr); r = 0.0; j = 0; while j < 3; s = 0.0; arr.each { |x| s += x }; r += s; j += 1; end; r; end
        def zero_trip; s = 0.0; [].each { |x| s += x }; s; end
        def huge(arr); s = 0.0; arr.each { |x| s += x * 1.0e300 }; s; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        r = nil
        40.times { r = [after_use(a), then_loop(a), in_loop(a), zero_trip, huge(a)] }
        r
        "##,
    );
}

#[test]
fn leaving_the_block_early() {
    run_test(
        r##"
        def brk(arr); s = 0.0; arr.each { |x| s += x; break if s > 3.0 }; s; end
        def brk_val(arr); s = 0.0; r = arr.each { |x| s += x; break s * 2.0 if s > 3.0 }; [s, r]; end
        def nxt(arr); s = 0.0; arr.each { |x| next if x < 1.0; s += x }; s; end
        def ret_in_block(arr); s = 0.0; arr.each { |x| s += x; return s if x > 2.0 }; -s; end
        def throw_out(arr); s = 0.0; catch(:done) { arr.each { |x| s += x; throw :done if x > 2.0 } }; s; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        r = nil
        40.times { r = [brk(a), brk_val(a), nxt(a), ret_in_block(a), throw_out(a)] }
        r + [brk([0.5, 5.0, 100.0]), ret_in_block([0.5]), throw_out([])]
        "##,
    );
}

#[test]
fn exception_from_the_block() {
    run_test(
        r##"
        def raise_rescue(arr)
          s = 0.0
          begin
            arr.each { |x| s += x; raise "stop" if x > 2.0 }
          rescue => e
            return [s, e.message]
          end
          [s, nil]
        end
        def raise_outer(arr); s = 0.0; arr.each { |x| s += x; raise "stop" if x > 2.0 }; s; end
        def str_err(arr); s = 0.0; arr.each { |x| s += x }; s; rescue => e; [s, e.class]; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        r = nil
        40.times { r = [raise_rescue(a), (raise_outer(a) rescue :raised), str_err(a)] }
        r + [raise_rescue([0.5, 1.0]), str_err([1.5, "x", 2.5]), str_err([1.5, nil])]
        "##,
    );
}

#[test]
fn operand_class_changes_after_warm_up() {
    run_test(
        r##"
        def sum(arr); s = 0.0; arr.each { |x| s += x }; s; end
        def to_other(arr); s = 0.0; arr.each { |x| s += x; s = s.to_i if s > 100.0 }; s; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        r = nil
        40.times { r = [sum(a), to_other(a)] }
        r + [sum([1.5, 2, 3.25, 4]), sum([1.5, 2r, 3.25]), sum([1.5, 2**70, 3.25]), sum(a), to_other([1.5, 2.5])]
        "##,
    );
}

#[test]
fn frame_captured_after_the_call() {
    run_test(
        r##"
        def capture(arr); s = 0.0; arr.each { |x| s += x }; pr = proc { s }; s += 1.0; pr.call; end
        def bind(arr); s = 0.0; arr.each { |x| s += x }; binding.local_variable_get(:s); end
        def in_block_proc(arr); s = 0.0; ps = []; arr.each { |x| s += x; ps << proc { s } if x > 14.0 }; ps.map(&:call) << s; end
        a = Array.new(30) { |i| i * 0.5 + 0.25 }
        r = nil
        40.times { r = [capture(a), bind(a), in_block_proc(a)] }
        r
        "##,
    );
}

/// The block's store reaches the accumulator's float home from a register
/// that is itself spilled (enough floats live in the block for the pool to
/// run out, which `stress-spill-pool` makes two). The aarch64 lowering of
/// that store materialized the home's address in the scratch register the
/// spill read then reused, so the home was never written and every `+=`
/// started over from the accumulator's first value.
#[test]
fn accumulator_written_from_a_spilled_register() {
    run_test(
        r##"
        def occ_literal(n, k)
          occ = 0.0
          p0 = k + 0.0001 * k
          n.times do |j|
            n.times do |i|
              r = (i * 0.37 + j * 0.11) % 1.0
              x = 1.0 - r
              y = r * 2.0
              z = r * 3.0
              rx = x * p0 + y * 0.5 + z * 0.25
              occ += 1.0 if rx > 0.3
            end
          end
          (n.to_f * n.to_f - occ) / (n.to_f * n.to_f)
        end
        def occ_computed(n, k)
          occ = k * 0.0
          p0 = k + 0.0001 * k
          n.times do |j|
            n.times do |i|
              r = (i * 0.37 + j * 0.11) % 1.0
              x = 1.0 - r
              y = r * 2.0
              z = r * 3.0
              rx = x * p0 + y * 0.5 + z * 0.25
              occ += rx if rx > 0.3
            end
          end
          occ / (n.to_f * n.to_f)
        end
        s = 0.0
        t = 0.0
        100.times { |i| s += occ_literal(6, 0.5 + i * 0.0001); t += occ_computed(6, 0.5 + i * 0.0001) }
        [s, t]
        "##,
    );
}
