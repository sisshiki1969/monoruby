use crate::*;
use fancy_regex::{Captures, Regex};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct RegexpInfo(Rc<Regex>);

impl PartialEq for RegexpInfo {
    fn eq(&self, other: &Self) -> bool {
        if Rc::ptr_eq(&self.0, &other.0) {
            return true;
        }
        self.as_str() == other.as_str()
    }
}

impl std::ops::Deref for RegexpInfo {
    type Target = Regex;
    fn deref(&self) -> &Regex {
        &self.0
    }
}

impl RegexpInfo {
    /// Create `RegexpInfo` from `escaped_str` escaping all meta characters.
    pub(crate) fn from_escaped(globals: &mut Globals, escaped_str: &str) -> Option<Self> {
        let string = regex::escape(escaped_str);
        RegexpInfo::from_string(globals, string)
    }

    /// Create `RegexpInfo` from `reg_str`.
    /// The first `\\Z\z` in `reg_str` is replaced by '\z' for compatibility issue
    /// between fancy_regex crate and Regexp class of Ruby.
    pub(crate) fn from_string(globals: &mut Globals, mut reg_str: String) -> Option<Self> {
        let conv = Regex::new(r"\\Z\z").unwrap();
        if let Some(mat) = conv.find(&reg_str).unwrap() {
            reg_str.replace_range(mat.range(), r"\z");
        };
        match globals.regexp_cache.get(&reg_str) {
            Some(re) => Some(RegexpInfo(re.clone())),
            None => match Regex::new(&reg_str) {
                Ok(regexp) => {
                    let regex = Rc::new(regexp);
                    globals.regexp_cache.insert(reg_str, regex.clone());
                    Some(RegexpInfo(regex))
                }
                Err(err) => {
                    globals.err_regex(err.to_string());
                    None
                }
            },
        }
    }

    pub(crate) fn new(mut reg_str: String) -> Result<Self, String> {
        let conv = Regex::new(r"\\Z\z").unwrap();
        if let Some(mat) = conv.find(&reg_str).unwrap() {
            reg_str.replace_range(mat.range(), r"\z");
        };
        match Regex::new(&reg_str) {
            Ok(regexp) => {
                let regex = Rc::new(regexp);
                Ok(RegexpInfo(regex))
            }
            Err(err) => Err(err.to_string()),
        }
    }
}

// Utility methods

impl RegexpInfo {
    /// Replaces the leftmost-first match with `replace`.
    pub(crate) fn replace_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        replace: &str,
    ) -> Option<(String, bool)> {
        if let Some(s) = re_val.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            re.replace_once(vm, globals, given, replace)
                .map(|(s, c)| (s, c.is_some()))
        } else if let Some(re) = re_val.is_regex() {
            re.replace_once(vm, globals, given, replace)
                .map(|(s, c)| (s, c.is_some()))
        } else {
            globals.err_argument("1st arg must be RegExp or String.");
            None
        }
    }

    pub(crate) fn replace_one_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        block_handler: BlockHandler,
    ) -> Option<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInfo,
            given: &str,
            block_handler: BlockHandler,
        ) -> Option<(String, bool)> {
            let (start, end, matched_str) = match re.captures_from_pos(given, 0) {
                Ok(None) => return Some((given.to_string(), false)),
                Ok(Some(captures)) => {
                    let m = captures.get(0).unwrap();
                    vm.get_captures(&captures, given);
                    (m.start(), m.end(), m.as_str())
                }
                Err(err) => {
                    globals.err_internal(format!("Capture failed. {:?}", err));
                    return None;
                }
            };

            let mut res = given.to_string();
            let matched = Value::new_string_from_str(matched_str);
            let result = vm.invoke_block(globals, block_handler, &[matched])?;
            let s = result.to_s(globals);
            res.replace_range(start..end, &s);
            Some((res, true))
        }

        if let Some(s) = re_val.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            replace_(vm, globals, &re, given, block_handler)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, re, given, block_handler)
        } else {
            globals.err_argument("1st arg must be RegExp or String.");
            None
        }
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all(
        vm: &mut Executor,
        globals: &mut Globals,
        regexp: Value,
        given: &str,
        replace: &str,
    ) -> Option<(String, bool)> {
        if let Some(s) = regexp.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            re.replace_repeat(vm, globals, given, replace)
        } else if let Some(re) = regexp.is_regex() {
            re.replace_repeat(vm, globals, given, replace)
        } else {
            globals.err_argument("1st arg must be RegExp or String.");
            None
        }
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        block_handler: BlockHandler,
    ) -> Option<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInfo,
            given: &str,
            block_handler: BlockHandler,
        ) -> Option<(String, bool)> {
            let mut range = vec![];
            let mut i = 0;
            loop {
                let (start, end, matched_str) = match re.captures_from_pos(given, i) {
                    Ok(None) => break,
                    Ok(Some(captures)) => {
                        let m = captures.get(0).unwrap();
                        i = m.end() + usize::from(m.start() == m.end());
                        vm.get_captures(&captures, given);
                        (m.start(), m.end(), m.as_str())
                    }
                    Err(err) => {
                        globals.err_internal(format!("Capture failed. {:?}", err));
                        return None;
                    }
                };
                let matched = Value::new_string_from_str(matched_str);
                let result: Value = vm.invoke_block(globals, block_handler, &[matched])?;
                let replace = result.to_s(globals);
                range.push((start, end, replace));
            }

            let mut res = given.to_string();
            for (start, end, replace) in range.iter().rev() {
                res.replace_range(start..end, replace);
            }
            Some((res, !range.is_empty()))
        }

        if let Some(s) = re_val.is_string() {
            let re = Self::from_escaped(globals, &s)?;
            replace_(vm, globals, &re, given, block_handler)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, re, given, block_handler)
        } else {
            globals.err_argument("1st arg must be RegExp or String.");
            None
        }
    }

    pub(crate) fn match_one<'a>(
        vm: &mut Executor,
        globals: &mut Globals,
        re: &Regex,
        given: &'a str,
        block: Option<BlockHandler>,
        pos: usize,
    ) -> Option<Value> {
        let pos = match given.char_indices().nth(pos) {
            Some((pos, _)) => pos,
            None => return Some(Value::nil()),
        };
        match re.captures_from_pos(given, pos) {
            Ok(None) => Some(Value::nil()),
            Ok(Some(captures)) => {
                vm.get_captures(&captures, given);
                if let Some(block_handler) = block {
                    let matched = Value::new_string_from_str(captures.get(0).unwrap().as_str());
                    vm.invoke_block(globals, block_handler, &[matched])
                } else {
                    let mut v = vec![];
                    for i in 0..captures.len() {
                        v.push(Value::new_string_from_str(
                            captures.get(i).unwrap().as_str(),
                        ));
                    }
                    Some(Value::new_array_from_vec(v))
                }
            }
            Err(err) => {
                globals.err_internal(format!("Capture failed. {:?}", err));
                None
            }
        }
    }

    /*
    /// Find the leftmost-first match for `given`.
    /// Returns `Match`s.
    pub(crate) fn find_one<'a>(
        vm: &mut Executor,
        globals: &mut Globals,
        re: &Regex,
        given: &'a str,
    ) -> Option<Option<Match<'a>>> {
        match re.captures(given) {
            Ok(None) => Some(None),
            Ok(Some(captures)) => {
                vm.get_captures(&captures, given);
                Some(captures.get(0))
            }
            Err(err) => {
                globals.err_internal(format!("Capture failed. {:?}", err));
                None
            }
        }
    }

    pub(crate) fn find_all(
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

impl RegexpInfo {
    /// Replace all matches for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// (replaced:String, is_replaced?:bool)
    pub(crate) fn replace_repeat(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
        given: &str,
        replace: &str,
    ) -> Option<(String, bool)> {
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
                    globals.err_internal(format!("Capture failed. {:?}", err));
                    return None;
                }
            };
        }
        let mut res = given.to_string();
        for (start, end) in range.iter().rev() {
            res.replace_range(start..end, replace);
        }

        if let Some(c) = last_captures {
            vm.get_captures(&c, given)
        }

        Some((res, !range.is_empty()))
    }

    /// Replaces the leftmost-first match for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// replaced:String
    pub(crate) fn replace_once<'a>(
        &'a self,
        vm: &mut Executor,
        globals: &mut Globals,
        given: &'a str,
        replace: &str,
    ) -> Option<(String, Option<Captures>)> {
        match self.captures(given) {
            Ok(None) => Some((given.to_string(), None)),
            Ok(Some(captures)) => {
                let mut res = given.to_string();
                let m = captures.get(0).unwrap();
                vm.get_captures(&captures, given);
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
                Some((res, Some(captures)))
            }
            Err(err) => {
                globals.err_internal(format!("Capture failed. {:?}", err));
                None
            }
        }
    }
}
