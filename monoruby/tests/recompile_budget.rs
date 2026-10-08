extern crate monoruby;
use monoruby::tests::*;

// The shared, size-scaled recompile budget of a compilation unit
// (`unit_recompile_budget`): every not-yet-profiled site of a unit draws on
// one budget, so a large unit whose branches come alive one by one is
// recompiled a few times rather than once per branch.

/// A 64-way `case` (a toy opcode dispatcher) whose arms are first taken at
/// different times: each recompile must pick up the arms the interpreter has
/// profiled since, and every arm must keep computing the right value.
fn dispatcher() -> String {
    let mut arms = String::new();
    for i in 0..64 {
        arms += &format!(
            "      when {i} then @acc = (@acc * {m} + x + {i}) & 0xffff; helper{h}(x)\n",
            m = i % 7 + 1,
            h = i % 4
        );
    }
    format!(
        r#"
    class Cpu
      def initialize
        @acc = 1
        @log = 0
      end
      attr_reader :acc, :log
      def helper0(x) = @log += x
      def helper1(x) = @log ^= x
      def helper2(x) = @log = (@log + x * 3) & 0xffff
      def helper3(x) = @log -= 1
      def exec(op, x)
        case op
{arms}        else
          @acc += 1
        end
      end
    end
    "#
    )
}

#[test]
fn recompile_budget_method_unit() {
    run_test_once(&format!(
        r#"
        {}
        cpu = Cpu.new
        res = []
        # Widen the set of live opcodes step by step.
        (1..64).each do |live|
          200.times do |i|
            cpu.exec((i * 7) % live, i)
          end
          res << cpu.acc << cpu.log
        end
        res
        "#,
        dispatcher()
    ));
}

#[test]
fn recompile_budget_loop_unit() {
    run_test_once(&format!(
        r#"
        {}
        cpu = Cpu.new
        res = []
        live = 1
        i = 0
        # The loop itself is the unit: its body grows new live arms while it
        # runs.
        while i < 20000
          case i % 4
          when 0 then cpu.exec(i % live, i)
          when 1 then res << cpu.acc if i % 997 == 1
          when 2 then live += 1 if i % 300 == 2 && live < 64
          else cpu.exec(live - 1, -i)
          end
          i += 1
        end
        res << cpu.acc << cpu.log
        "#,
        dispatcher()
    ));
}
