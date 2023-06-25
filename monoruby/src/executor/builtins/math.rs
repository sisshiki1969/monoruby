use super::*;
use num::ToPrimitive;

//
// Math class
//

pub(super) fn init(globals: &mut Globals, class_id: ClassId) {
    globals.set_constant_by_str(class_id, "PI", Value::float(3.141592653589793));
    globals.define_builtin_module_func_inlinable(
        class_id,
        "sqrt",
        sqrt,
        1,
        math_sqrt,
        analysis_sqrt,
    );
    globals.define_builtin_module_func_inlinable(class_id, "cos", cos, 1, math_cos, analysis_cos);
    globals.define_builtin_module_func_inlinable(class_id, "sin", sin, 1, math_sin, analysis_sin);
}

/// ### Math.#sqrt
/// - sqrt(x) -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Math/m/sqrt.html]
#[monoruby_builtin]
fn sqrt(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Integer(i) => i as f64,
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
fn sin(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Integer(i) => i as f64,
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
fn cos(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arg0 = arg[0];
    let f = match arg0.unpack() {
        RV::Float(f) => f,
        RV::Integer(i) => i as f64,
        RV::BigInt(b) => b.to_f64().unwrap(),
        _ => {
            return Err(MonorubyErr::cant_convert_into_float(globals, arg0));
        }
    };
    Ok(Value::float(f.cos()))
}

fn math_sqrt(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    method_info: &MethodInfo,
    ret: SlotId,
    pc: BcPc,
    deopt: DestLabel,
) {
    let MethodInfo { recv, args, .. } = method_info;
    gen.load_rdi(*recv);
    if !recv.is_zero() {
        gen.guard_class(pc.class_version().0, deopt);
    }
    let fsrc = gen.fetch_float_assume_float_enc(ctx, *args, pc);
    let fret = ctx.xmm_write_enc(ret);
    monoasm!( &mut gen.jit,
        sqrtsd xmm(fret), xmm(fsrc);
    );
}

fn analysis_sqrt(info: &mut SlotInfo, method_info: &MethodInfo, ret: SlotId) {
    info.use_non_float(method_info.recv);
    info.use_as(method_info.args, true, FLOAT_CLASS);
    info.def_as(ret, true);
}

fn math_cos(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    method_info: &MethodInfo,
    ret: SlotId,
    pc: BcPc,
    deopt: DestLabel,
) {
    let MethodInfo { recv, args, .. } = method_info;
    gen.load_rdi(*recv);
    if !recv.is_zero() {
        gen.guard_class(pc.class_version().0, deopt);
    }
    let fsrc = gen.fetch_float_assume_float_enc(ctx, *args, pc);
    let fret = ctx.xmm_write_enc(ret);
    let xmm_using = ctx.get_xmm_using();
    gen.xmm_save(&xmm_using);
    monoasm!( &mut gen.jit,
        movq xmm0, xmm(fsrc);
        movq rax, (extern_cos);
        call rax;
    );
    gen.xmm_restore(&xmm_using);
    monoasm!( &mut gen.jit,
        movq xmm(fret), xmm0;
    );
}

fn analysis_cos(info: &mut SlotInfo, method_info: &MethodInfo, ret: SlotId) {
    info.use_non_float(method_info.recv);
    info.use_as(method_info.args, true, FLOAT_CLASS);
    info.def_as(ret, true);
}

fn math_sin(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    method_info: &MethodInfo,
    ret: SlotId,
    pc: BcPc,
    deopt: DestLabel,
) {
    let MethodInfo { recv, args, .. } = method_info;
    gen.load_rdi(*recv);
    if !recv.is_zero() {
        gen.guard_class(pc.class_version().0, deopt);
    }
    let fsrc = gen.fetch_float_assume_float_enc(ctx, *args, pc);
    let fret = ctx.xmm_write_enc(ret);
    let xmm_using = ctx.get_xmm_using();
    gen.xmm_save(&xmm_using);
    monoasm!( &mut gen.jit,
        movq xmm0, xmm(fsrc);
        movq rax, (extern_sin);
        call rax;
    );
    gen.xmm_restore(&xmm_using);
    monoasm!( &mut gen.jit,
        movq xmm(fret), xmm0;
    );
}

fn analysis_sin(info: &mut SlotInfo, method_info: &MethodInfo, ret: SlotId) {
    info.use_non_float(method_info.recv);
    info.use_as(method_info.args, true, FLOAT_CLASS);
    info.def_as(ret, true);
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
