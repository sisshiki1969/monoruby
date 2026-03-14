use super::*;

//
// Marshal module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Marshal").id();
    globals.define_builtin_module_func(klass, "dump", dump, 1);
}

/// ### Marshal.dump
/// - dump(obj) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Marshal/m/dump.html]
#[monoruby_builtin]
fn dump(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _pc: BytecodePtr) -> Result<Value> {
    let obj = lfp.arg(0);
    let mut buf: Vec<u8> = Vec::new();
    // Marshal version header
    buf.push(0x04);
    buf.push(0x08);
    marshal_dump_value(&mut buf, obj, globals)?;
    Ok(Value::bytes(buf))
}

fn marshal_dump_value(buf: &mut Vec<u8>, obj: Value, globals: &Globals) -> Result<()> {
    match obj.unpack() {
        RV::Nil => {
            buf.push(b'0'); // 0x30
            Ok(())
        }
        RV::Bool(true) => {
            buf.push(b'T'); // 0x54
            Ok(())
        }
        RV::Bool(false) => {
            buf.push(b'F'); // 0x46
            Ok(())
        }
        _ => Err(MonorubyErr::typeerr(format!(
            "no _dump_data is defined for class {}",
            globals.get_class_name(obj.class())
        ))),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn marshal_dump_nil() {
        run_test("Marshal.dump(nil)");
    }

    #[test]
    fn marshal_dump_true() {
        run_test("Marshal.dump(true)");
    }

    #[test]
    fn marshal_dump_false() {
        run_test("Marshal.dump(false)");
    }
}
