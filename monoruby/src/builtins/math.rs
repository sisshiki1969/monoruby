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
    globals.set_constant_by_str(klass, "PI", Value::float(3.141592653589793));
    globals.define_builtin_module_inline_func(klass, "sqrt", sqrt, math_sqrt, analysis::f_v_f);
    globals.define_builtin_module_inline_func(klass, "cos", cos, math_cos, analysis::f_v_f);
    globals.define_builtin_module_inline_func(klass, "sin", sin, math_sin, analysis::f_v_f);
}

/// ### Math.#sqrt
/// - sqrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sqrt.html]
#[monoruby_builtin]
fn sqrt(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(globals, arg0));
        }
    };
    Ok(Value::float(f.sqrt()))
}

/// ### Math.#sin
/// - sin(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sin.html]
#[monoruby_builtin]
fn sin(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(globals, arg0));
        }
    };
    Ok(Value::float(f.sin()))
}

/// ### Math.#cos
/// - cos(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/cos.html]
#[monoruby_builtin]
fn cos(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Fixnum(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(globals, arg0));
        }
    };
    Ok(Value::float(f.cos()))
}

fn math_sqrt(
    gen: &mut Codegen,
    store: &Store,
    ctx: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
    deopt: DestLabel,
) {
    let CallSiteInfo {
        recv,
        args,
        dst: ret,
        ..
    } = *callsite;
    gen.fetch_slots(store, ctx, &[recv]);
    gen.load_rdi(recv);
    if !recv.is_zero() {
        gen.guard_class_rdi(pc.cached_class1().unwrap(), deopt);
    }
    let fsrc = gen.fetch_float_assume_float(store, ctx, args, pc).enc();
    if let Some(ret) = ret {
        let fret = ctx.xmm_write_enc(ret);
        monoasm!( &mut gen.jit,
            sqrtsd xmm(fret), xmm(fsrc);
        );
    }
}

fn math_cos(
    gen: &mut Codegen,
    store: &Store,
    ctx: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
    deopt: DestLabel,
) {
    let CallSiteInfo {
        recv,
        args,
        dst: ret,
        ..
    } = *callsite;
    gen.fetch_slots(store, ctx, &[recv]);
    gen.load_rdi(recv);
    if !recv.is_zero() {
        gen.guard_class_rdi(pc.cached_class1().unwrap(), deopt);
    }
    let fsrc = gen.fetch_float_assume_float(store, ctx, args, pc).enc();
    if let Some(ret) = ret {
        let fret = ctx.xmm_write_enc(ret);
        let using_xmm = ctx.get_using_xmm();
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
    }
}

fn math_sin(
    gen: &mut Codegen,
    store: &Store,
    ctx: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
    deopt: DestLabel,
) {
    let CallSiteInfo {
        recv,
        args,
        dst: ret,
        ..
    } = *callsite;
    gen.fetch_slots(store, ctx, &[recv]);
    gen.load_rdi(recv);
    if !recv.is_zero() {
        gen.guard_class_rdi(pc.cached_class1().unwrap(), deopt);
    }
    let fsrc = gen.fetch_float_assume_float(store, ctx, args, pc).enc();
    if let Some(ret) = ret {
        let fret = ctx.xmm_write_enc(ret);
        let using_xmm = ctx.get_using_xmm();
        gen.xmm_save(using_xmm);
        monoasm!( &mut gen.jit,
            movq xmm0, xmm(fsrc);
            movq rax, (extern_sin);
            call rax;
        );
        gen.xmm_restore(using_xmm);
        monoasm!( &mut gen.jit,
            movq xmm(fret), xmm0;
        );
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
