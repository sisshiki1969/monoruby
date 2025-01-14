use super::*;
use num::ToPrimitive;

//
// Math class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Math").id();
    let standarderr = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("StandardError"))
        .unwrap()
        .as_class();
    globals.define_class("DomainError", standarderr, klass);
    globals.set_constant_by_str(klass, "PI", Value::float(std::f64::consts::PI));
    globals.define_builtin_module_inline_func(klass, "sqrt", sqrt, Box::new(math_sqrt), 1);
    globals.define_builtin_module_inline_func(klass, "cos", cos, Box::new(math_cos), 1);
    globals.define_builtin_module_inline_func(klass, "sin", sin, Box::new(math_sin), 1);
    globals.define_builtin_module_func(klass, "log10", log10, 1);
}

/// ### Math.#sqrt
/// - sqrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sqrt.html]
#[monoruby_builtin]
fn sqrt(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn sin(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn cos(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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

/// Math.#log10
/// - log10(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/log10.html]
#[monoruby_builtin]
fn log10(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(arg0));
        }
    };
    if f.is_sign_negative() {
        return Err(MonorubyErr::rangeerr(
            "Numerical argument is out of domain - log10",
        ));
    }
    Ok(Value::float(f.log10()))
}

fn math_sqrt(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { args, dst, .. } = *callsite;
    let deopt = bb.new_deopt(ir);
    let fsrc = bb.fetch_float_for_xmm(ir, args, deopt).enc();
    if let Some(dst) = dst {
        let fret = bb.xmm_write_enc(dst);
        ir.inline(move |gen, _, _| {
            monoasm!( &mut gen.jit,
                sqrtsd xmm(fret), xmm(fsrc);
            );
        });
    }
    true
}

fn math_cos(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { args, dst, .. } = *callsite;
    let deopt = bb.new_deopt(ir);
    let fsrc = bb.fetch_float_for_xmm(ir, args, deopt).enc();
    if let Some(ret) = dst {
        let fret = bb.xmm_write_enc(ret);
        let using_xmm = bb.get_using_xmm();
        ir.inline(move |gen, _, _| {
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
    true
}

fn math_sin(
    bb: &mut BBContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { args, dst: ret, .. } = *callsite;
    let deopt = bb.new_deopt(ir);
    let fsrc = bb.fetch_float_for_xmm(ir, args, deopt).enc();
    if let Some(ret) = ret {
        let fret = bb.xmm_write_enc(ret);
        let using_xmm = bb.get_using_xmm();
        ir.inline(move |gen, _, _| {
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
    true
}

extern "C" fn extern_cos(f: f64) -> f64 {
    f.cos()
}

extern "C" fn extern_sin(f: f64) -> f64 {
    f.sin()
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn torigonometric() {
        run_test("Math.cos 149");
        run_test("Math.cos -14.97522");
        run_test("Math.sin 149");
        run_test("Math.sin -14.97522");
    }

    #[test]
    fn log() {
        run_test("Math.log10 149");
        run_test("Math.log10 14.9");
        run_test("Math.log10 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
    }

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
}
