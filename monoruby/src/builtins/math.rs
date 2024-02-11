use super::*;
use num::ToPrimitive;

//
// Math class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_module("Math").id();
    let standarderr = globals
        .get_constant(OBJECT_CLASS, IdentId::get_id("StandardError"))
        .unwrap()
        .as_class();
    globals.define_class_by_str("DomainError", standarderr, klass);
    globals.set_constant_by_str(klass, "PI", Value::float(std::f64::consts::PI));
    globals.define_builtin_module_inline_func(klass, "sqrt", sqrt, math_sqrt, analysis::f_v_f, 1);
    globals.define_builtin_module_inline_func(klass, "cos", cos, math_cos, analysis::f_v_f, 1);
    globals.define_builtin_module_inline_func(klass, "sin", sin, math_sin, analysis::f_v_f, 1);
}

/// ### Math.#sqrt
/// - sqrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sqrt.html]
#[monoruby_builtin]
fn sqrt(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(arg0));
        }
    };
    Ok(Value::float(f.sqrt()))
}

/// ### Math.#sin
/// - sin(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sin.html]
#[monoruby_builtin]
fn sin(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(arg0));
        }
    };
    Ok(Value::float(f.sin()))
}

/// ### Math.#cos
/// - cos(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/cos.html]
#[monoruby_builtin]
fn cos(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(arg0));
        }
    };
    Ok(Value::float(f.cos()))
}

fn math_sqrt(
    ir: &mut AsmIr,
    _store: &Store,
    bb: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
) {
    let CallSiteInfo {
        recv,
        args,
        dst: ret,
        ..
    } = *callsite;
    let deopt = ir.new_deopt(bb, pc);
    if !recv.is_self() {
        ir.guard_class(bb, recv, GP::Rdi, pc.cached_class1().unwrap(), deopt);
    }
    let fsrc = ir.fetch_float_assume_float(bb, args, deopt).enc();
    if let Some(ret) = ret {
        let fret = ir.xmm_write_enc(bb, ret);
        ir.inline(move |gen, _| {
            monoasm!( &mut gen.jit,
                sqrtsd xmm(fret), xmm(fsrc);
            );
        });
    }
}

fn math_cos(ir: &mut AsmIr, _store: &Store, bb: &mut BBContext, callsite: &CallSiteInfo, pc: BcPc) {
    let CallSiteInfo {
        recv,
        args,
        dst: ret,
        ..
    } = *callsite;
    let deopt = ir.new_deopt(bb, pc);
    if !recv.is_self() {
        ir.guard_class(bb, recv, GP::Rdi, pc.cached_class1().unwrap(), deopt);
    }
    let fsrc = ir.fetch_float_assume_float(bb, args, deopt).enc();
    if let Some(ret) = ret {
        let fret = ir.xmm_write_enc(bb, ret);
        let using_xmm = bb.get_using_xmm();
        ir.inline(move |gen, _| {
            gen.xmm_save(using_xmm);
            monoasm!( &mut gen.jit,
                movq xmm0, xmm(fsrc);
                movq rax, (extern_cos);
                call rax;
            );
            gen.xmm_restore(using_xmm);
            monoasm!( &mut gen.jit,
                movq xmm(fret), xmm0;
            );
        });
    }
}

fn math_sin(ir: &mut AsmIr, _store: &Store, bb: &mut BBContext, callsite: &CallSiteInfo, pc: BcPc) {
    let CallSiteInfo {
        recv,
        args,
        dst: ret,
        ..
    } = *callsite;
    let deopt = ir.new_deopt(bb, pc);
    if !recv.is_self() {
        ir.guard_class(bb, recv, GP::Rdi, pc.cached_class1().unwrap(), deopt);
    }
    let fsrc = ir.fetch_float_assume_float(bb, args, deopt).enc();
    if let Some(ret) = ret {
        let fret = ir.xmm_write_enc(bb, ret);
        let using_xmm = bb.get_using_xmm();
        ir.inline(move |gen, _| {
            gen.xmm_save(using_xmm);
            monoasm! { &mut gen.jit,
                movq xmm0, xmm(fsrc);
                movq rax, (extern_sin);
                call rax;
            }
            gen.xmm_restore(using_xmm);
            monoasm! { &mut gen.jit,
                movq xmm(fret), xmm0;
            }
        });
    }
}

extern "C" fn extern_cos(f: f64) -> f64 {
    f.cos()
}

extern "C" fn extern_sin(f: f64) -> f64 {
    f.sin()
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn sqrt() {
        run_test("Math.sqrt 128");
        run_test("Math.sqrt 2192.56818");
        run_test(
            r#"
        class C
          include Math
          def f
            sqrt 2192.56818
          end
        end
        C.new.f
        "#,
        );
    }

    #[test]
    fn torigonometric() {
        run_test("Math.cos 149");
        run_test("Math.cos -14.97522");
        run_test("Math.sin 149");
        run_test("Math.sin -14.97522");
    }
}
