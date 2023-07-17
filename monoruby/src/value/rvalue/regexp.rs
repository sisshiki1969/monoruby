use crate::*;
use fancy_regex::{Captures, Match, Regex};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct RegexpInner(Rc<Regex>);

impl PartialEq for RegexpInner {
    fn eq(&self, other: &Self) -> bool {
        if Rc::ptr_eq(&self.0, &other.0) {
            return true;
        }
        self.as_str() == other.as_str()
    }
}

impl std::ops::Deref for RegexpInner {
    type Target = Regex;
    fn deref(&self) -> &Regex {
        &self.0
    }
}

impl RegexpInner {
    /// Create `RegexpInfo` from `escaped_str` escaping all meta characters.
    pub(crate) fn from_escaped(globals: &mut Globals, escaped_str: &str) -> Result<Self> {
        let string = regex::escape(escaped_str);
        RegexpInner::from_string(globals, string)
    }

    /// Create `RegexpInfo` from `reg_str`.
    /// The first `\\Z\z` in `reg_str` is replaced by '\z' for compatibility issue
    /// between fancy_regex crate and Regexp class of Ruby.
    pub(crate) fn from_string(globals: &mut Globals, mut reg_str: String) -> Result<Self> {
        let conv = Regex::new(r"\\Z\z").unwrap();
        if let Some(mat) = conv.find(&reg_str).unwrap() {
            reg_str.replace_range(mat.range(), r"\z");
        };
        match globals.regexp_cache.get(&reg_str) {
            Some(re) => Ok(RegexpInner(re.clone())),
            None => match Regex::new(&reg_str) {
                Ok(regexp) => {
                    let regex = Rc::new(regexp);
                    globals.regexp_cache.insert(reg_str, regex.clone());
                    Ok(RegexpInner(regex))
                }
                Err(err) => Err(MonorubyErr::regexerr(err.to_string())),
            },
        }
    }

    pub(crate) fn new(mut reg_str: String) -> std::result::Result<Self, String> {
        let conv = Regex::new(r"\\Z\z").unwrap();
        if let Some(mat) = conv.find(&reg_str).unwrap() {
            reg_str.replace_range(mat.range(), r"\z");
        };
        match Regex::new(&reg_str) {
            Ok(regexp) => {
                let regex = Rc::new(regexp);
                Ok(RegexpInner(regex))
            }
            Err(err) => Err(err.to_string()),
        }
    }
}

// Utility methods

impl RegexpInner {
    /// Replaces the leftmost-first match with `replace`.
    pub(crate) fn replace_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        if let Some(s) = re_val.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            re.replace_once(vm, given, replace)
                .map(|(s, c)| (s, c.is_some()))
        } else if let Some(re) = re_val.is_regex() {
            re.replace_once(vm, given, replace)
                .map(|(s, c)| (s, c.is_some()))
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.".to_string(),
            ))
        }
    }

    pub(crate) fn replace_one_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        block_handler: BlockHandler,
    ) -> Result<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInner,
            given: &str,
            block_handler: BlockHandler,
        ) -> Result<(String, bool)> {
            let (start, end, matched_str) = match re.captures_from_pos(given, 0) {
                Ok(None) => return Ok((given.to_string(), false)),
                Ok(Some(captures)) => {
                    let m = captures.get(0).unwrap();
                    vm.save_captures(&captures, given);
                    (m.start(), m.end(), m.as_str())
                }
                Err(err) => {
                    return Err(MonorubyErr::internalerr(format!(
                        "Capture failed. {:?}",
                        err
                    )));
                }
            };

            let mut res = given.to_string();
            let matched = Value::string_from_str(matched_str);
            let result = vm.invoke_block_once(globals, block_handler, &[matched])?;
            let s = globals.to_s(result);
            res.replace_range(start..end, &s);
            Ok((res, true))
        }

        if let Some(s) = re_val.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            replace_(vm, globals, &re, given, block_handler)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, re, given, block_handler)
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.".to_string(),
            ))
        }
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all(
        vm: &mut Executor,
        globals: &mut Globals,
        regexp: Value,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        if let Some(s) = regexp.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            re.replace_repeat(vm, given, replace)
        } else if let Some(re) = regexp.is_regex() {
            re.replace_repeat(vm, given, replace)
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.".to_string(),
            ))
        }
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        block_handler: BlockHandler,
    ) -> Result<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInner,
            given: &str,
            block_handler: BlockHandler,
        ) -> Result<(String, bool)> {
            let mut range = vec![];
            let mut i = 0;
            let data = globals.get_block_data(vm.cfp());
            loop {
                let (start, end, matched_str) = match re.captures_from_pos(given, i) {
                    Ok(None) => break,
                    Ok(Some(captures)) => {
                        let m = captures.get(0).unwrap();
                        i = m.end() + usize::from(m.start() == m.end());
                        vm.save_captures(&captures, given);
                        (m.start(), m.end(), m.as_str())
                    }
                    Err(err) => {
                        return Err(MonorubyErr::internalerr(format!(
                            "Capture failed. {:?}",
                            err
                        )));
                    }
                };
                let matched = Value::string_from_str(matched_str);
                let result = vm.invoke_block(globals, &data, &[matched])?;
                let replace = globals.to_s(result);
                range.push((start, end, replace));
            }

            let mut res = given.to_string();
            for (start, end, replace) in range.iter().rev() {
                res.replace_range(start..end, replace);
            }
            Ok((res, !range.is_empty()))
        }

        if let Some(s) = re_val.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            replace_(vm, globals, &re, given, block_handler)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, re, given, block_handler)
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.".to_string(),
            ))
        }
    }

    pub(crate) fn match_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re: &Regex,
        given: &str,
        block: Option<BlockHandler>,
        char_pos: usize,
    ) -> Result<Value> {
        let byte_pos = match given.char_indices().nth(char_pos) {
            Some((pos, _)) => pos,
            None => return Ok(Value::nil()),
        };
        match re.captures_from_pos(given, byte_pos) {
            Ok(None) => Ok(Value::nil()),
            Ok(Some(captures)) => {
                vm.save_captures(&captures, given);
                if let Some(block_handler) = block {
                    let matched = Value::string_from_str(captures.get(0).unwrap().as_str());
                    vm.invoke_block_once(globals, block_handler, &[matched])
                } else {
                    let mut ary = ArrayInner::new();
                    for i in 0..captures.len() {
                        ary.push(Value::string_from_str(captures.get(i).unwrap().as_str()));
                    }
                    Ok(Value::array(ary))
                }
            }
            Err(err) => Err(MonorubyErr::internalerr(format!(
                "Capture failed. {:?}",
                err
            ))),
        }
    }

    /// Find the leftmost-first match for `given`.
    /// Returns `Match`s.
    pub(crate) fn find_one<'a>(
        vm: &mut Executor,
        re: &Regex,
        given: &'a str,
    ) -> Result<Option<Match<'a>>> {
        match re.captures(given) {
            Ok(None) => Ok(None),
            Ok(Some(captures)) => {
                vm.save_captures(&captures, given);
                Ok(captures.get(0))
            }
            Err(err) => Err(MonorubyErr::internalerr(format!(
                "Capture failed. {:?}",
                err
            ))),
        }
    }

    /*pub(crate) fn find_all(
        vm: &mut Executor,
        globals: &mut Globals,
        re: &Regex,
        given: &str,
    ) -> Option<Vec<Value>> {
        let mut ary = vec![];
        let mut idx = 0;
        let mut last_captures = None;
        loop {
            match re.captures_from_pos(given, idx) {
                Ok(None) => break,
                Ok(Some(captures)) => {
                    let m = captures.get(0).unwrap();
                    idx = m.end();
                    match captures.len() {
                        1 => {
                            let val = Value::new_string_from_str(&given[m.start()..m.end()]);
                            ary.push(val);
                        }
                        len => {
                            let mut vec = vec![];
                            for i in 1..len {
                                match captures.get(i) {
                                    Some(m) => {
                                        vec.push(Value::new_string_from_str(
                                            &given[m.start()..m.end()],
                                        ));
                                    }
                                    None => vec.push(Value::nil()),
                                }
                            }
                            let val = Value::new_array_from_vec(vec);
                            ary.push(val);
                        }
                    }
                    last_captures = Some(captures);
                }
                Err(err) => {
                    globals.err_internal(format!("Capture failed. {:?}", err));
                    return None;
                }
            };
        }
        match last_captures {
            Some(c) => vm.get_captures(&c, given),
            None => {}
        }
        Some(ary)
    }*/
}

impl RegexpInner {
    /// Replace all matches for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// (replaced:String, is_replaced?:bool)
    pub(crate) fn replace_repeat(
        &self,
        vm: &mut Executor,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        let mut range = vec![];
        let mut i = 0;
        let mut last_captures = None;
        loop {
            if i >= given.len() {
                break;
            }
            match self.captures_from_pos(given, i) {
                Ok(None) => break,
                Ok(Some(captures)) => {
                    let m = captures.get(0).unwrap();
                    // the length of matched string can be 0.
                    // this is neccesary to avoid infinite loop.
                    i = if m.end() == m.start() {
                        m.end() + 1
                    } else {
                        m.end()
                    };
                    range.push((m.start(), m.end()));
                    last_captures = Some(captures);
                }
                Err(err) => {
                    return Err(MonorubyErr::internalerr(format!(
                        "Capture failed. {:?}",
                        err
                    )));
                }
            };
        }
        let mut res = given.to_string();
        for (start, end) in range.iter().rev() {
            res.replace_range(start..end, replace);
        }

        if let Some(c) = last_captures {
            vm.save_captures(&c, given)
        }

        Ok((res, !range.is_empty()))
    }

    /// Replaces the leftmost-first match for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// replaced:String
    pub(crate) fn replace_once<'a>(
        &'a self,
        vm: &mut Executor,
        given: &'a str,
        replace: &str,
    ) -> Result<(String, Option<Captures>)> {
        match self.captures(given) {
            Ok(None) => Ok((given.to_string(), None)),
            Ok(Some(captures)) => {
                let mut res = given.to_string();
                let m = captures.get(0).unwrap();
                vm.save_captures(&captures, given);
                let mut rep = "".to_string();
                let mut escape = false;
                for ch in replace.chars() {
                    if escape {
                        match ch {
                            '0'..='9' => {
                                let i = ch as usize - '0' as usize;
                                if let Some(m) = captures.get(i) {
                                    rep += m.as_str()
                                }
                            }
                            _ => rep.push(ch),
                        };
                        escape = false;
                    } else if ch != '\\' {
                        rep.push(ch);
                    } else {
                        escape = true;
                    }
                }
                res.replace_range(m.start()..m.end(), &rep);
                Ok((res, Some(captures)))
            }
            Err(err) => Err(MonorubyErr::internalerr(format!(
                "Capture failed. {:?}",
                err
            ))),
        }
    }
}
