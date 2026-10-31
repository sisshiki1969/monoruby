use super::*;
use crate::bytecodegen::BinOpK;
use crate::executor::inline::InlineFuncInfo;

//
// Boolean methods (internal): `BOOL_CLASS` is *not* a Ruby-visible class.
// `true` and `false` keep their normal `TrueClass` / `FalseClass`
// identities at the user level. The constant only exists as an inline-
// cache tag so a polymorphic-on-true/false call site does not deopt on
// every flip.
//
// To make that tag safe, the methods that are otherwise defined twice
// (once on TrueClass and once on FalseClass with subtly different bodies)
// are registered **once** in Rust and `add_method` is called on **both**
// classes with the same `FuncId`. Subsequent JIT lookups under either
// class therefore resolve to the same `FuncId`, which is the invariant
// the IC relies on.
//

pub(super) fn init(globals: &mut Globals) {
    let and_fid = define_bool_method(globals, "&", and_, 1, 1, false);
    let or_fid = define_bool_method(globals, "|", or_, 1, 1, false);
    let xor_fid = define_bool_method(globals, "^", xor_, 1, 1, false);
    for (fid, kind) in [
        (and_fid, BinOpK::BitAnd),
        (or_fid, BinOpK::BitOr),
        (xor_fid, BinOpK::BitXor),
    ] {
        globals
            .store
            .inline_info
            .add_inline(fid, InlineFuncInfo::new_inline_gen(bool_binop(kind)));
    }
}

///
/// JIT-inline `TrueClass#&` / `#|` / `#^` (shared with `FalseClass`). The
/// call site's guard has proven the receiver a bool; the argument is
/// reduced to its truthiness (`!!arg`) and combined with the receiver on
/// the raw words (see `AsmInst::BoolBinOp`). Emulator flag updates
/// (`@halted &= cond`) run this per instruction.
///
fn bool_binop(kind: BinOpK) -> Box<InlineGen> {
    Box::new(move |state, ir, _, store, callid, _, _| {
        let callsite = &store[callid];
        if !callsite.is_simple() || callsite.pos_num != 1 {
            return false;
        }
        let CallSiteInfo {
            dst, recv, args, ..
        } = *callsite;
        // No side effects: an unused result needs no code.
        let Some(dst) = dst else {
            return true;
        };
        // A receiver that alone decides the answer: `false & x`, `true | x`.
        match kind {
            BinOpK::BitAnd if state.is_falsy(recv) => {
                state.def_C(dst, Immediate::bool(false));
                return true;
            }
            BinOpK::BitOr if state.is_truthy(recv) => {
                state.def_C(dst, Immediate::bool(true));
                return true;
            }
            _ => {}
        }
        state.load(ir, args, GP::Rdi);
        state.load(ir, recv, GP::Rsi);
        ir.bool_binop(kind, GP::Rax, GP::Rsi, GP::Rdi);
        state.def_reg2acc_class(ir, GP::Rax, dst, BOOL_CLASS);
        true
    })
}

/// Register `address` once and add it as `name` to both `TRUE_CLASS` and
/// `FALSE_CLASS` with the **same** `FuncId`.
fn define_bool_method(
    globals: &mut Globals,
    name: &str,
    address: BuiltinFn,
    min: usize,
    max: usize,
    rest: bool,
) -> FuncId {
    let func_id = globals
        .store
        .new_builtin_func(name, address, min, max, rest, &[], false);
    globals.gen_wrapper(func_id);
    let method_name = IdentId::get_id(name);
    globals
        .store
        .add_method(TRUE_CLASS, method_name, func_id, Visibility::Public);
    globals
        .store
        .add_method(FALSE_CLASS, method_name, func_id, Visibility::Public);
    func_id
}

///
/// ### TrueClass#& / FalseClass#&
/// - self & other -> bool
///
/// Shared between `true` and `false` so the JIT inline cache observes a
/// single `FuncId` regardless of which boolean the receiver happens to be.
///
#[monoruby_builtin]
fn and_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if lfp.self_val().as_bool() {
        Ok(Value::bool(lfp.arg(0).as_bool()))
    } else {
        Ok(Value::bool(false))
    }
}

///
/// ### TrueClass#| / FalseClass#|
/// - self | other -> bool
///
#[monoruby_builtin]
fn or_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if lfp.self_val().as_bool() {
        Ok(Value::bool(true))
    } else {
        Ok(Value::bool(lfp.arg(0).as_bool()))
    }
}

///
/// ### TrueClass#^ / FalseClass#^
/// - self ^ other -> bool
///
#[monoruby_builtin]
fn xor_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let other = lfp.arg(0).as_bool();
    if lfp.self_val().as_bool() {
        Ok(Value::bool(!other))
    } else {
        Ok(Value::bool(other))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// `true` / `false` `&`, `|`, `^` in JIT code, against every kind of
    /// argument (the argument is reduced to its truthiness), and the
    /// accumulator idiom `flag &= cond`.
    #[test]
    fn bool_binop_jit_inline() {
        run_test(
            r##"
        def ops(a, b) = [a & b, a | b, a ^ b]
        def lit(b) = [true & b, false & b, true | b, false | b, true ^ b, false ^ b]
        def acc(xs)
          h = true
          xs.each { |x| h &= x == 0 }
          h
        end
        def acc_or(xs)
          h = false
          xs.each { |x| h |= x.nil? }
          h
        end
        res = []
        30.times do
          [true, false, nil, 0, 1, :x, "s", []].each do |b|
            res << ops(true, b) + ops(false, b) + lit(b)
          end
          res << acc([0, 0]) << acc([0, 1, 0]) << acc_or([1, 2]) << acc_or([1, nil])
        end
        res
        "##,
        );
    }

    #[test]
    fn bool_binop_redefined_after_jit() {
        run_test_once(
            r##"
        def ops(a, b) = [a & b, a | b, a ^ b]
        res = []
        30.times { res << ops(true, false) << ops(false, true) << ops(false, nil) }
        # Each stage runs enough calls for the body to recompile under the
        # redefinition.
        class TrueClass
          def &(other) = :t_and
        end
        10.times { res << ops(true, false) << ops(false, true) }
        class FalseClass
          def ^(other) = :f_xor
        end
        10.times { res << ops(true, false) << ops(false, true) }
        class FalseClass
          def |(other) = :f_or
        end
        10.times { res << ops(true, false) << ops(false, true) }
        # An operator neither boolean class has: no inline target at all.
        def no_op(a, b) = (a - b rescue :no_method)
        10.times { res << no_op(true, false) << no_op(false, true) }
        res
        "##,
        );
    }
}
