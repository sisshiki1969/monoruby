use super::*;
use onigmo_regex::{Captures, FindCaptures, OnigmoEncoding, Regex};
use std::sync::Arc;
use std::sync::{LazyLock, RwLock};

static REGEX_CACHE: LazyLock<RwLock<RegexCache>> = LazyLock::new(|| RwLock::new(RegexCache::new()));

#[derive(Debug, Default)]
struct RegexCache(HashMap<(String, u32), Arc<Regex>>);

impl RegexCache {
    fn new() -> Self {
        Self(HashMap::default())
    }
}

#[monoruby_object]
pub struct Regexp(Value);

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

    pub fn option(&self) -> u32 {
        self.0.option()
    }

    pub fn option_string(&self) -> String {
        let mut res = String::new();
        let option = self.option();
        if option & onigmo_regex::ONIG_OPTION_MULTILINE != 0 {
            res.push('m');
        }
        if option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0 {
            res.push('i');
        }
        if option & onigmo_regex::ONIG_OPTION_EXTEND != 0 {
            res.push('x');
        }
        res
    }

    pub fn escape(text: &str) -> String {
        regex::escape(text)
    }

    /// Create `RegexpInfo` from `escaped_str` escaping all meta characters.
    pub fn from_escaped(text: &str) -> Result<Self> {
        RegexpInner::with_option_and_encoding(Self::escape(text), 0, OnigmoEncoding::UTF8)
    }

    /// Create `RegexpInfo` from `reg_str` with `option`.
    pub fn with_option(reg_str: impl Into<String>, option: u32) -> Result<Self> {
        Self::with_option_and_encoding(reg_str, option, OnigmoEncoding::UTF8)
    }

    /// Create `RegexpInfo` from `reg_str` with `option`.
    pub fn with_option_and_encoding(
        reg_str: impl Into<String>,
        option: u32,
        encoding: OnigmoEncoding,
    ) -> Result<Self> {
        let reg_str: String = reg_str.into();
        match REGEX_CACHE
            .write()
            .unwrap()
            .0
            .entry((reg_str.clone(), option))
        {
            std::collections::hash_map::Entry::Occupied(entry) => {
                Ok(RegexpInner(entry.get().clone()))
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                match Regex::new_with_option_and_encoding(&reg_str, option, encoding) {
                    Ok(regexp) => {
                        let regex = Arc::new(regexp);
                        entry.insert(regex.clone());
                        Ok(RegexpInner(regex))
                    }
                    Err(err) => Err(MonorubyErr::regexerr(err)),
                }
            }
        }
    }

    pub fn get_group_members(&self, name: &str) -> Vec<i32> {
        self.0.get_group_nembers(name)
    }

    pub fn capture_names(&self) -> Result<Vec<String>> {
        self.0
            .capture_names()
            .map_err(|err| MonorubyErr::regexerr(err.message()))
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
                    vm.save_capture_special_variables(captures)
                } else {
                    vm.clear_capture_special_variables();
                }
                Ok(res)
            }
            Err(err) => Err(MonorubyErr::regexerr(format!("Capture failed. {:?}", err))),
        }
    }

    pub fn captures_iter<'a>(&self, given: &'a str) -> FindCaptures<'_, 'a> {
        self.0.captures_iter(given)
    }

    /// Find the leftmost-first match for `given`.
    /// Returns `Match`s.
    pub fn find_one<'a>(
        &self,
        vm: &mut Executor,
        given: &'a str,
    ) -> Result<Option<std::ops::Range<usize>>> {
        match self.captures(given, vm)? {
            None => Ok(None),
            Some(captures) => Ok(captures.get(0).map(|m| m.range())),
        }
    }

    pub fn tos(&self) -> String {
        let option = self.option();
        let m = option & onigmo_regex::ONIG_OPTION_MULTILINE != 0;
        let i = option & onigmo_regex::ONIG_OPTION_IGNORECASE != 0;
        let x = option & onigmo_regex::ONIG_OPTION_EXTEND != 0;
        format!(
            "(?{}{}{}{}{}{}{}:{})",
            if m { "m" } else { "" },
            if i { "i" } else { "" },
            if x { "x" } else { "" },
            if m && i && x { "" } else { "-" },
            if !m { "m" } else { "" },
            if !i { "i" } else { "" },
            if !x { "x" } else { "" },
            self.as_str()
        )
    }

    pub fn inspect(&self) -> String {
        format!("/{}/{}", self.as_str(), self.option_string())
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
        } else if let Some(re) = re_val.is_regex() {
            re.replace_once(vm, given, replace)
        } else {
            return Err(MonorubyErr::argumenterr(
                "1st arg must be RegExp or String.",
            ));
        }
        .map(|(s, c)| (s, c.is_some()))
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
            match re.captures(given, vm)? {
                None => Ok((given.to_string(), false)),
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    let (start, end, matched_str) = (m.start(), m.end(), m.as_str());
                    let mut res = given.to_string();
                    let matched = Value::string_from_str(matched_str);
                    let result = vm.invoke_block_once(globals, bh, &[matched])?;
                    let s = result.to_s(&globals.store);
                    res.replace_range(start..end, &s);
                    Ok((res, true))
                }
            }
        }

        if let Some(s) = re_val.is_str() {
            let re = Self::from_escaped(s)?;
            replace_(vm, globals, &re, given, bh)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, &re, given, bh)
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
            let data = vm.get_block_data(globals, bh)?;

            vm.clear_capture_special_variables();
            for cap in re.captures_iter(given) {
                let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
                let m = cap.get(0).unwrap();

                let matched_str = m.as_str();
                let matched = Value::string_from_str(matched_str);
                vm.save_capture_special_variables(&cap);
                let result = vm.invoke_block(globals, &data, &[matched])?;
                let replace = result.to_s(&globals.store);

                range.push((m.range(), replace));
            }

            let mut res = given.to_string();
            let is_empty = range.is_empty();

            for (range, replace) in range.into_iter().rev() {
                res.replace_range(range, &replace);
            }

            Ok((res, !is_empty))
        }

        if let Some(s) = re_val.is_str() {
            let re = Self::from_escaped(s)?;
            replace_(vm, globals, &re, given, bh)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, &re, given, bh)
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

    pub(crate) fn scan(&self, vm: &mut Executor, given: &str) -> Result<Vec<Value>> {
        let mut ary = vec![];
        let mut last_captures = None;
        vm.clear_capture_special_variables();
        for cap in self.0.captures_iter(given) {
            let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
            match cap.len() {
                0 => unreachable!(),
                1 => {
                    let val = Value::string(cap.get(0).unwrap().to_string());
                    ary.push(val);
                }
                len => {
                    let mut vec = vec![];
                    for i in 1..len {
                        match cap.get(i) {
                            Some(m) => {
                                vec.push(Value::string(m.to_string()));
                            }
                            None => vec.push(Value::nil()),
                        }
                    }
                    let val = Value::array_from_vec(vec);
                    ary.push(val);
                }
            }
            last_captures = Some(cap);
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c)
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
        vm.clear_capture_special_variables();
        let mut last_captures = None;
        for cap in self.captures_iter(given) {
            let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
            let m = cap.get(0).unwrap();
            range.push(m.range());
            last_captures = Some(cap);
        }
        let mut res = given.to_string();
        let is_empty = range.is_empty();
        for r in range.into_iter().rev() {
            res.replace_range(r, replace);
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c)
        }

        Ok((res, !is_empty))
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
    assert!(re.find("").unwrap().is_some());
    assert!(re.find("/").unwrap().is_some());
    assert!(!re.find("a").unwrap().is_some());
}
