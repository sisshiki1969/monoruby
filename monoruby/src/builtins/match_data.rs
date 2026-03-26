use super::*;

//
// MatchData class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("MatchData", MATCHDATA_CLASS, ObjTy::MATCHDATA);
    globals.define_builtin_class_func(MATCHDATA_CLASS, "allocate", super::class::undef_allocate, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "captures", captures, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "[]", index, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "begin", match_begin, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "end", match_end, 1);
    globals.define_builtin_func(MATCHDATA_CLASS, "named_captures", named_captures, 0);
}

///
/// ### MatchData#captures
///
/// - captures -> [String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/captures.html]
#[monoruby_builtin]
fn captures(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::array_from_iter(
        lfp.self_val().as_match_data().captures().skip(1).map(|s| {
            if let Some(s) = s {
                Value::string_from_str(s)
            } else {
                Value::nil()
            }
        }),
    ))
}

///
/// ### MatchData#[]
///
/// - self[n] -> String | nil
/// - self[name] -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/=5b=5d.html]
#[monoruby_builtin]
fn index(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    if let Some(idx) = lfp.arg(0).try_fixnum() {
        let idx = if idx >= 0 {
            idx as usize
        } else {
            let idx = idx + m.len() as i64;
            if idx < 0 {
                return Ok(Value::nil());
            }
            idx as usize
        };
        if idx >= m.len() {
            return Ok(Value::nil());
        }
        Ok(m.at(idx as usize)
            .map(|s| Value::string_from_str(s))
            .unwrap_or_default())
    } else if let Some(sym) = lfp.arg(0).try_symbol_or_string() {
        if let Some(i) = m.regexp().get_group_members(&format!("{sym}")).last() {
            if let Some(s) = m.at(*i as usize) {
                Ok(Value::string_from_str(s))
            } else {
                Ok(Value::nil())
            }
        } else {
            Err(MonorubyErr::indexerr(format!(
                "undefined group name reference: {sym}"
            )))
        }
    } else {
        Err(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into Integer",
            lfp.arg(0).get_real_class_name(globals)
        )))
    }
}

///
/// ### MatchData#begin
///
/// - begin(n) -> Integer | nil
///
/// Returns the offset of the start of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/begin.html]
#[monoruby_builtin]
fn match_begin(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = lfp.arg(0).coerce_to_i64(globals)? as usize;
    if idx >= m.len() {
        return Err(MonorubyErr::indexerr(format!(
            "index {idx} out of matches"
        )));
    }
    match m.pos(idx) {
        Some((start, _)) => {
            let char_offset = m.string()[..start].chars().count();
            Ok(Value::integer(char_offset as i64))
        }
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#end
///
/// - end(n) -> Integer | nil
///
/// Returns the offset of the character just past the end of the nth match.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/end.html]
#[monoruby_builtin]
fn match_end(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let idx = lfp.arg(0).coerce_to_i64(globals)? as usize;
    if idx >= m.len() {
        return Err(MonorubyErr::indexerr(format!(
            "index {idx} out of matches"
        )));
    }
    match m.pos(idx) {
        Some((_, end_pos)) => {
            let char_offset = m.string()[..end_pos].chars().count();
            Ok(Value::integer(char_offset as i64))
        }
        None => Ok(Value::nil()),
    }
}

///
/// ### MatchData#named_captures
///
/// - named_captures -> Hash
///
/// Returns a Hash of named captures.
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/named_captures.html]
#[monoruby_builtin]
fn named_captures(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let m = self_.as_match_data();
    let names = m.regexp().capture_names().unwrap();
    let mut map = RubyMap::default();
    for (i, name) in names.iter().enumerate() {
        let key = Value::string_from_str(name);
        let val = m.at(i + 1)
            .map(|s| Value::string_from_str(s))
            .unwrap_or_default();
        map.insert(key, val, vm, globals)?;
    }
    Ok(Value::hash(map))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn match_data() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").to_s"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").inspect"##);
        run_test(r##"/(?<a>foo)(?<b>bar)(?<c>BAZ)?/.match("foobarbaz").inspect"##);
        run_test(r##"/(?<a>foo)(?<b>bar)(BAZ)?/.match("foobarbaz").inspect"##);
        run_test(r##"/(?<a>foo)(?<b>bar)(?<a>BAZ)?/.match("foobarbaz").inspect"##);

        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz").captures"##);
    }

    #[test]
    fn match_data_index() {
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[-100]"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[-1]"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[0]"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")[100]"##);
        run_test(r##"/(foo)(?<abc>bar)(BAZ)?/.match("foobarbaz")["abc"]"##);
        run_test(r##"/(foo)(?<abc>xxx)?(BAZ)?/.match("foobarbaz")["abc"]"##);
        run_test_error(r##"/(foo)(bar)(BAZ)?/.match("foobarbaz")["abc"]"##);
    }

    #[test]
    fn match_data_begin_end() {
        run_test(r##"/(foo)(bar)/.match("foobar").begin(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").begin(1)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").begin(2)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").end(0)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").end(1)"##);
        run_test(r##"/(foo)(bar)/.match("foobar").end(2)"##);
        // nil group
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").begin(3)"##);
        run_test(r##"/(foo)(bar)(BAZ)?/.match("foobar").end(3)"##);
    }

    #[test]
    fn match_data_named_captures() {
        run_test(r##"/(?<a>foo)(?<b>bar)/.match("foobar").named_captures"##);
    }

    #[test]
    fn match_data_begin_end_out_of_range() {
        // Index out of range -> IndexError
        run_test_error(r##"/(foo)(bar)/.match("foobar").begin(5)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").end(5)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").begin(-1)"##);
        run_test_error(r##"/(foo)(bar)/.match("foobar").end(-1)"##);
    }
}
