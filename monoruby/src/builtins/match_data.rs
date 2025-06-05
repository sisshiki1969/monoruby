use super::*;

//
// MatchData class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("MatchData", MATCHDATA_CLASS, ObjTy::MATCHDATA);
    globals.define_builtin_func(MATCHDATA_CLASS, "captures", captures, 0);
    globals.define_builtin_func(MATCHDATA_CLASS, "[]", index, 1);
}

///
/// ### MatchData#captures
///
/// - captures -> [String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/MatchData/i/captures.html]
#[monoruby_builtin]
fn captures(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn index(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
}
