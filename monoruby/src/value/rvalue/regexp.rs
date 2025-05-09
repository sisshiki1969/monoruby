use super::*;
use fancy_regex::{CaptureMatches, Captures, Match, Regex};
use std::sync::Arc;
use std::sync::{LazyLock, RwLock};

static HASHMAP_CACHE: LazyLock<RwLock<HashMapCache>> =
    LazyLock::new(|| RwLock::new(HashMapCache::new()));

#[derive(Debug, Default)]
struct HashMapCache(HashMap<String, Arc<Regex>>);

impl HashMapCache {
    fn new() -> Self {
        Self(HashMap::default())
    }
}

#[derive(Clone, Debug)]
pub struct RegexpInner(Arc<Regex>);

impl PartialEq for RegexpInner {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.0, &other.0) {
            return true;
        }
        self.as_str() == other.as_str()
    }
}

impl RegexpInner {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn union(v: &[String]) -> Result<Self> {
        let s = v
            .iter()
            .map(|re| format!("(?:{})", re))
            .collect::<Vec<_>>()
            .join("|");
        Self::from_string(s)
    }

    pub fn escape(text: &str) -> String {
        regex::escape(text)
    }

    /// Create `RegexpInfo` from `escaped_str` escaping all meta characters.
    pub fn from_escaped(text: &str) -> Result<Self> {
        RegexpInner::from_string(Self::escape(text))
    }

    /// Create `RegexpInfo` from `reg_str`.
    /// The first `\\Z\z` in `reg_str` is replaced by '\z' for compatibility issue
    /// between fancy_regex crate and Regexp class of Ruby.
    pub fn from_string(reg_str: impl Into<String>) -> Result<Self> {
        let mut reg_str: String = reg_str.into();
        let conv = Regex::new(r"\\Z\z").unwrap();
        if let Some(mat) = conv.find(&reg_str).unwrap() {
            reg_str.replace_range(mat.range(), r"\z");
        };
        match HASHMAP_CACHE.write().unwrap().0.entry(reg_str.clone()) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                Ok(RegexpInner(entry.get().clone()))
            }
            std::collections::hash_map::Entry::Vacant(entry) => match Regex::new(&reg_str) {
                Ok(regexp) => {
                    let regex = Arc::new(regexp);
                    entry.insert(regex.clone());
                    Ok(RegexpInner(regex))
                }
                Err(err) => Err(MonorubyErr::regexerr(err)),
            },
        }
    }

    pub fn new(mut reg_str: String) -> std::result::Result<Self, String> {
        let conv = Regex::new(r"\\Z\z").unwrap();
        if let Some(mat) = conv.find(&reg_str).unwrap() {
            reg_str.replace_range(mat.range(), r"\z");
        };
        match Regex::new(&reg_str) {
            Ok(regexp) => {
                let regex = Arc::new(regexp);
                Ok(RegexpInner(regex))
            }
            Err(err) => Err(err.to_string()),
        }
    }

    pub fn captures<'a>(&self, given: &'a str, vm: &mut Executor) -> Result<Option<Captures<'a>>> {
        self.captures_from_pos(given, 0, vm)
    }

    pub fn captures_from_pos<'a>(
        &self,
        given: &'a str,
        pos: usize,
        vm: &mut Executor,
    ) -> Result<Option<Captures<'a>>> {
        match self.0.captures_from_pos(given, pos) {
            Ok(res) => {
                if let Some(captures) = &res {
                    vm.save_capture_special_variables(captures, given)
                } else {
                    vm.clear_capture_special_variables();
                }
                Ok(res)
            }
            Err(err) => Err(MonorubyErr::internalerr(format!(
                "Capture failed. {:?}",
                err
            ))),
        }
    }

    pub fn captures_from_pos_no_save<'a>(
        &self,
        given: &'a str,
        pos: usize,
    ) -> Result<Option<Captures<'a>>> {
        match self.0.captures_from_pos(given, pos) {
            Ok(res) => Ok(res),
            Err(err) => Err(MonorubyErr::internalerr(format!(
                "Capture failed. {:?}",
                err
            ))),
        }
    }

    pub fn captures_iter<'a>(&self, given: &'a str) -> CaptureMatches<'_, 'a> {
        self.0.captures_iter(given)
    }

    /// Find the leftmost-first match for `given`.
    /// Returns `Match`s.
    pub fn find_one<'a>(&self, vm: &mut Executor, given: &'a str) -> Result<Option<Match<'a>>> {
        match self.captures(given, vm)? {
            None => Ok(None),
            Some(captures) => Ok(captures.get(0)),
        }
    }
}

// Utility methods

impl RegexpInner {
    /// Replaces the leftmost-first match with `replace`.
    pub(crate) fn replace_one(
        vm: &mut Executor,
        re_val: Value,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        if let Some(s) = re_val.is_str() {
            let re = Self::from_escaped(s)?;
            re.replace_once(vm, given, replace)
                .map(|(s, c)| (s, c.is_some()))
        } else if let Some(re) = re_val.is_regex() {
            re.replace_once(vm, given, replace)
                .map(|(s, c)| (s, c.is_some()))
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.",
            ))
        }
    }

    pub(crate) fn replace_one_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        bh: BlockHandler,
    ) -> Result<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInner,
            given: &str,
            bh: BlockHandler,
        ) -> Result<(String, bool)> {
            let (start, end, matched_str) = match re.captures(given, vm)? {
                None => {
                    return Ok((given.to_string(), false));
                }
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    (m.start(), m.end(), m.as_str())
                }
            };

            let mut res = given.to_string();
            let matched = Value::string_from_str(matched_str);
            let result = vm.invoke_block_once(globals, bh, &[matched])?;
            let s = result.to_s(&globals.store);
            res.replace_range(start..end, &s);
            Ok((res, true))
        }

        if let Some(s) = re_val.is_str() {
            let re = Self::from_escaped(s)?;
            replace_(vm, globals, &re, given, bh)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, re, given, bh)
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.",
            ))
        }
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all(
        vm: &mut Executor,
        regexp: Value,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        if let Some(s) = regexp.is_str() {
            let re = Self::from_escaped(s)?;
            re.replace_repeat(vm, given, replace)
        } else if let Some(re) = regexp.is_regex() {
            re.replace_repeat(vm, given, replace)
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.",
            ))
        }
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        bh: BlockHandler,
    ) -> Result<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInner,
            given: &str,
            bh: BlockHandler,
        ) -> Result<(String, bool)> {
            let mut range = vec![];
            let mut i = 0;
            let data = vm.get_block_data(globals, bh)?;
            loop {
                let (start, end, matched_str) = match re.captures_from_pos_no_save(given, i)? {
                    None => break,
                    Some(captures) => {
                        let m = captures.get(0).unwrap();
                        i = m.end() + usize::from(m.start() == m.end());
                        vm.save_capture_special_variables(&captures, given);
                        (m.start(), m.end(), m.as_str())
                    }
                };
                let matched = Value::string_from_str(matched_str);
                let result = vm.invoke_block(globals, &data, &[matched])?;
                let replace = result.to_s(&globals.store);
                range.push((start, end, replace));
            }

            let mut res = given.to_string();
            for (start, end, replace) in range.iter().rev() {
                res.replace_range(start..end, replace);
            }
            Ok((res, !range.is_empty()))
        }

        if let Some(s) = re_val.is_str() {
            let re = Self::from_escaped(s)?;
            replace_(vm, globals, &re, given, bh)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, re, given, bh)
        } else {
            Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.",
            ))
        }
    }

    pub(crate) fn match_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re: &RegexpInner,
        given: &str,
        block: Option<BlockHandler>,
        char_pos: usize,
    ) -> Result<Value> {
        let byte_pos = match given.char_indices().nth(char_pos) {
            Some((pos, _)) => pos,
            None => return Ok(Value::nil()),
        };
        match re.captures_from_pos(given, byte_pos, vm)? {
            None => Ok(Value::nil()),
            Some(captures) => {
                if let Some(bh) = block {
                    let matched = Value::string_from_str(captures.get(0).unwrap().as_str());
                    vm.invoke_block_once(globals, bh, &[matched])
                } else {
                    let mut ary = Array::new_empty();
                    for i in 0..captures.len() {
                        ary.push(Value::string_from_str(captures.get(i).unwrap().as_str()));
                    }
                    Ok(ary.into())
                }
            }
        }
    }

    pub(crate) fn find_all(&self, vm: &mut Executor, given: &str) -> Result<Vec<Value>> {
        let mut ary = vec![];
        let mut idx = 0;
        let mut last_captures = None;
        vm.clear_capture_special_variables();
        loop {
            match self.captures_from_pos_no_save(given, idx)? {
                None => break,
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    idx = m.end();
                    match captures.len() {
                        1 => {
                            let val = Value::string_from_str(&given[m.start()..m.end()]);
                            ary.push(val);
                        }
                        len => {
                            let mut vec = vec![];
                            for i in 1..len {
                                match captures.get(i) {
                                    Some(m) => {
                                        vec.push(Value::string_from_str(
                                            &given[m.start()..m.end()],
                                        ));
                                    }
                                    None => vec.push(Value::nil()),
                                }
                            }
                            let val = Value::array_from_vec(vec);
                            ary.push(val);
                        }
                    }
                    last_captures = Some(captures);
                }
            };
        }
        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c, given)
        }
        Ok(ary)
    }
}

impl RegexpInner {
    /// Replace all matches for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// (replaced:String, is_replaced?:bool)
    fn replace_repeat(
        &self,
        vm: &mut Executor,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        let mut range = vec![];
        let mut i = 0;
        vm.clear_capture_special_variables();
        let mut last_captures = None;
        loop {
            if i >= given.len() {
                break;
            }
            match self.captures_from_pos_no_save(given, i)? {
                None => break,
                Some(captures) => {
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
            };
        }
        let mut res = given.to_string();
        for (start, end) in range.iter().rev() {
            res.replace_range(start..end, replace);
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c, given)
        }

        Ok((res, !range.is_empty()))
    }

    /// Replaces the leftmost-first match for `self` in `given` string with `replace`.
    ///
    /// ### return
    /// replaced:String
    fn replace_once<'a>(
        &self,
        vm: &mut Executor,
        given: &'a str,
        replace: &str,
    ) -> Result<(String, Option<Captures<'a>>)> {
        match self.captures(given, vm)? {
            None => Ok((given.to_string(), None)),
            Some(captures) => {
                let mut res = given.to_string();
                let m = captures.get(0).unwrap();
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
        }
    }
}

#[test]
fn test_regexp() {
    let re = Regex::new(r#"(?:(?m)\A(?:(?m)/)?\z)"#).unwrap();
    assert!(re.is_match("").unwrap());
    assert!(re.is_match("/").unwrap());
    assert!(!re.is_match("a").unwrap());
}
