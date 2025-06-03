use super::*;

//
// MatchData class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("MatchData", MATCHDATA_CLASS, ObjTy::MATCHDATA);
    globals.define_builtin_func(MATCHDATA_CLASS, "captures", captures, 0);
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
