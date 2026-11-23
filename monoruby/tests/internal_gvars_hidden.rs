//! The interpreter keeps some state of its own in the global variable
//! table: the `$!` save of a protected region, and the per-site state of
//! a flip-flop, an `END { }` and a `/.../o`. Those entries are named
//! without the `$` (`/errinfo`, `/flip_flop_N`, `/end_block_N`,
//! `/regexp_once_N`), so nothing a program can write reaches them
//! (#2158).
//!
//! They used to be `$(errinfo)` and `$__flip_flop_N` etc. — `$` names
//! like any other. `global_variables` listed them, a program could
//! assign a flip-flop's state, and a `trace_var(:"$(errinfo)")` hook ran
//! on every protected region's exit in the interpreter but not in JIT
//! code, which stores `$!`'s save without asking for hooks.
extern crate monoruby;
use monoruby::tests::*;

/// `global_variables` lists `$` names only, whatever the program ran.
#[test]
fn global_variables_lists_no_internal_entry() {
    run_test(
        r#"
        def region
          begin
            :body
          ensure
            nil
          end
        end
        30.times { region }
        ff = (1..10).select { |i| true if (i == 3)..(i == 5) }
        once = %w[a b].map { |s| s =~ /#{s}/o }
        END { }
        [
          ff, once,
          global_variables.include?(:"$(errinfo)"),
          global_variables.include?(:"/errinfo"),
          global_variables.reject { |n| n.start_with?("$") },
          global_variables.grep(/errinfo|flip_flop|end_block|regexp_once/),
        ]
        "#,
    );
}

/// The handoff's script: a hook on the old spelling of the `$!` save is
/// a hook on a variable nobody assigns, in the interpreter and in JIT
/// code alike. (It ran once per `defined?` probe in the interpreter.)
#[test]
fn a_hook_on_the_old_errinfo_name_never_fires() {
    run_test_once(
        r#"
        $errinfo_hook = 0
        trace_var(:"$(errinfo)") { $errinfo_hook += 1 }
        def probe(o) = defined?(o.foo)
        def region
          begin
            :body
          ensure
            nil
          end
        end
        def rescued
          raise "x"
        rescue
          $!.message
        end
        cold = [probe(1), region, rescued, $errinfo_hook]
        200.times { probe(1); region; rescued }
        [cold, $errinfo_hook, untrace_var(:"$(errinfo)").size]
        "#,
    );
}

/// The new spelling is not a name `trace_var` takes, so there is no hook
/// to disagree about either. (CRuby accepts any name and makes it a
/// variable nobody assigns, #2159: the count is 0 both ways.)
#[test]
fn the_internal_names_take_no_hook() {
    run_test_once(
        r#"
        $internal_hook = 0
        %w[/errinfo /flip_flop_0 /end_block_0 /regexp_once_0].each do |name|
          begin
            trace_var(name.to_sym) { $internal_hook += 1 }
          rescue NameError
          end
        end
        def region
          begin
            :body
          ensure
            nil
          end
        end
        res = []
        200.times do |i|
          region
          res << i if (i == 3)..(i == 5)
          res << (("a" =~ /#{"a"}/o) || :miss) if i == 0
        end
        [res, $internal_hook]
        "#,
    );
}

/// A flip-flop's, an `END`'s and a `/o`'s state is not a variable the
/// program can assign: `$__flip_flop_0` and friends are ordinary
/// globals now, unset ones.
#[test]
fn the_old_state_names_are_ordinary_globals() {
    run_test(
        r#"
        def once(s) = (s =~ /#{s}/o)
        64.times do |n|
          eval("$__flip_flop_#{n} = true; $__regexp_once_#{n} = /zzz/; $__end_block_#{n} = true")
        end
        res = [(0..5).select { |i| true if (i == 2)..(i == 3) }, once("a"), once("b")]
        64.times do |n|
          eval("$__flip_flop_#{n} = nil; $__regexp_once_#{n} = nil; $__end_block_#{n} = nil")
        end
        res
        "#,
    );
}

/// With the state variables out of the `$` namespace, a `$__` name is
/// no longer left out of the verbose-mode warning for an unset global.
#[test]
fn an_unset_dunder_global_warns_in_verbose_mode() {
    run_test_once(
        r#"
        log = []
        cap = Object.new
        cap.define_singleton_method(:write) do |*s|
          log.concat(s.join.scan(/global variable '\S+' not initialized/))
          0
        end
        verbose, $VERBOSE = $VERBOSE, true
        stderr, $stderr = $stderr, cap
        begin
          $__never_set_2158
          $_never_set_2158
          x = (1..4).select { |i| true if (i == 2)..(i == 3) }
          y = "ab" =~ /#{"a"}/o
        ensure
          $stderr = stderr
          $VERBOSE = verbose
        end
        [log, x, y]
        "#,
    );
}
