//! A fused method-call branch whose `method_call` compiles an inlined
//! callee under it — one with fused sites of its own.
//!
//! The pending `FusedBr` used to live in a single `JitContext` cell for
//! the whole compile, so the nested unit's fused `block_given?` clobbered
//! the outer callsite's pending branch and its own consumption cleared
//! the cell; the outer arm then read "taken" and emitted no truthiness
//! test at all, falling through unconditionally. optparse's
//! `make_switch` lost every long option to exactly this (`next if
//! search(:atype, o) do .. end` always took the `next`), which is how
//! rubocop's option parsing broke. The cell is now bracketed per
//! callsite and parked in a leftover cell before anything nested
//! compiles.
//!
//! Spawns the real binary: the in-process harness compiles this shape
//! without the nested inlining that triggers the clobber, so only the
//! production binary reproduces it.

use std::process::Command;

#[test]
fn fused_branch_survives_nested_inline_compile() {
    let script = r#"
        class FuseList
          def initialize; @atype = {}; end
          def atype; @atype; end
          def search(id, key)
            if dic = __send__(id)
              val = dic.fetch(key) { return nil }
              block_given? ? yield(val) : val
            end
          end
        end
        class FuseOP
          def initialize
            @stack = [FuseList.new, FuseList.new]
            @stack[0].atype[String] = [1, 2]
          end
          def visit(id, *args, &block)
            @stack.reverse_each do |el|
              v = el.__send__(id, *args, &block)
              return v if v
            end
            nil
          end
          def search(typ, opt)
            bg = block_given?
            visit(:search, typ, opt) do |k|
              return bg ? yield(k) : k
            end
          end
          def make(opts)
            long = []
            opts.each do |o|
              next if search(:atype, o) do |pat, c| end
              long << o if o.is_a?(String)
            end
            long
          end
        end
        op = FuseOP.new
        200.times do |i|
          r = op.make([String, "alpha", "beta"])
          raise "iteration #{i}: #{r.inspect}" unless r == ["alpha", "beta"]
        end
        puts "ok"
    "#;
    run(script);
}

/// A fused predicate on a polymorphic receiver: the callsite compiles as
/// a class dispatch (PIC arms, or a fast arm over a generic residual),
/// and a predicate generator firing *inside one arm* must not take the
/// fused branch — the sibling arms would join and fall through with no
/// truthiness test at all. `zero?` over {Integer, Float} and `nil?` over
/// a six-class mix both counted wrongly (591 / 594 instead of 400)
/// before the dispatch-arm gate parked the branch.
#[test]
fn fused_branch_not_taken_inside_dispatch_arms() {
    let script = r#"
        vals = [0, 1, 0.0, 2.5]
        c = 0
        200.times { vals.each { |x| c += 1 if x.zero? } }
        raise "zero?: #{c}" unless c == 400

        mixed = [1, nil, "s", nil, :sym, 2.5]
        c = 0
        d = 0
        200.times do
          mixed.each do |x|
            c += 1 if x.nil?
            d += 1 unless x.nil?
          end
        end
        raise "nil? each: #{c} #{d}" unless c == 400 && d == 800

        c = 0
        300.times { |i| x = mixed[i % 6]; c += 1 if x.nil? }
        raise "nil? index: #{c}" unless c == 100
        puts "ok"
    "#;
    run(script);
}

fn run(script: &str) {
    let out = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .args(["--disable=gems", "-e", script])
        .output()
        .expect("spawn monoruby");
    assert!(
        out.status.success(),
        "monoruby failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "ok");
}
