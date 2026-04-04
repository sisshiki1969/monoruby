use super::*;
use onigmo_regex::{Captures, FindCaptures, OnigmoEncoding, Regex};
use std::sync::Arc;
use std::sync::{LazyLock, RwLock};

static REGEX_CACHE: LazyLock<RwLock<RegexCache>> = LazyLock::new(|| RwLock::new(RegexCache::new()));

#[derive(Debug, Default)]
struct RegexCache(HashMap<(String, u32, OnigmoEncoding), Arc<Regex>>);

impl RegexCache {
    fn new() -> Self {
        Self(HashMap::default())
    }
}

#[monoruby_object]
pub struct Regexp(Value);

#[derive(Clone, Debug)]
pub struct RegexpInner {
    regex: Arc<Regex>,
    encoding: OnigmoEncoding,
}

impl PartialEq for RegexpInner {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.regex, &other.regex) {
            return true;
        }
        self.as_str() == other.as_str() && self.encoding == other.encoding
    }
}

impl RegexpInner {
    /// Ruby's Regexp::NOENCODING constant (value 32).
    /// When set in options, the regexp uses ASCII-8BIT (binary) encoding.
    pub const NOENCODING: u32 = 32;

    /// Ruby's Regexp::FIXEDENCODING constant (value 16).
    pub const FIXEDENCODING: u32 = 16;

    pub fn as_str(&self) -> &str {
        self.regex.as_str()
    }

    pub fn encoding(&self) -> OnigmoEncoding {
        self.encoding
    }

    pub fn option(&self) -> u32 {
        let mut opt = self.regex.option();
        if self.encoding == OnigmoEncoding::ASCII {
            opt |= Self::NOENCODING;
        }
        opt
    }

    /// Returns the raw onigmo option without Ruby encoding flags.
    pub fn raw_option(&self) -> u32 {
        self.regex.option()
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
            .entry((reg_str.clone(), option, encoding))
        {
            std::collections::hash_map::Entry::Occupied(entry) => Ok(RegexpInner {
                regex: entry.get().clone(),
                encoding,
            }),
            std::collections::hash_map::Entry::Vacant(entry) => {
                match Regex::new_with_option_and_encoding(&reg_str, option, encoding) {
                    Ok(regexp) => {
                        let regex = Arc::new(regexp);
                        entry.insert(regex.clone());
                        Ok(RegexpInner { regex, encoding })
                    }
                    Err(err) => Err(MonorubyErr::regexerr(err)),
                }
            }
        }
    }

    pub fn get_group_members(&self, name: &str) -> Vec<i32> {
        self.regex.get_group_nembers(name)
    }

    pub fn capture_names(&self) -> Result<Vec<String>> {
        self.regex
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
        match self.regex.captures_from_pos(given, pos) {
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
        self.regex.captures_iter(given)
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
        globals: &mut Globals,
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
            // Try to_str coercion
            let coerced = re_val.coerce_to_str(vm, globals)?;
            let re = Self::from_escaped(&coerced)?;
            re.replace_once(vm, given, replace)
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
            // Try to_str coercion
            let coerced = re_val.coerce_to_str(vm, globals)?;
            let re = Self::from_escaped(&coerced)?;
            replace_(vm, globals, &re, given, bh)
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
        if let Some(s) = regexp.is_str() {
            let re = Self::from_escaped(s)?;
            re.replace_repeat(vm, given, replace)
        } else if let Some(re) = regexp.is_regex() {
            re.replace_repeat(vm, given, replace)
        } else {
            // Try to_str coercion
            let coerced = regexp.coerce_to_str(vm, globals)?;
            let re = Self::from_escaped(&coerced)?;
            re.replace_repeat(vm, given, replace)
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
            // Try to_str coercion
            let coerced = re_val.coerce_to_str(vm, globals)?;
            let re = Self::from_escaped(&coerced)?;
            replace_(vm, globals, &re, given, bh)
        }
    }

    /// Replaces the first match in `given` string using hash lookup.
    /// For each match, the matched text is looked up as a key in the hash.
    /// If found, the corresponding value (converted to string) is used as replacement.
    /// If not found, the match is removed (replaced with empty string).
    pub(crate) fn replace_one_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        hash_val: Value,
    ) -> Result<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInner,
            given: &str,
            hash_val: Value,
        ) -> Result<(String, bool)> {
            match re.captures(given, vm)? {
                None => Ok((given.to_string(), false)),
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    let (start, end, matched_str) = (m.start(), m.end(), m.as_str());
                    let mut res = given.to_string();
                    let key = Value::string_from_str(matched_str);
                    let hash = hash_val.as_hash();
                    let replacement = if let Some(v) = hash.get(key, vm, globals)? {
                        v.to_s(&globals.store)
                    } else {
                        String::new()
                    };
                    res.replace_range(start..end, &replacement);
                    Ok((res, true))
                }
            }
        }

        if let Some(s) = re_val.is_str() {
            let re = Self::from_escaped(s)?;
            replace_(vm, globals, &re, given, hash_val)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, &re, given, hash_val)
        } else {
            let coerced = re_val.coerce_to_str(vm, globals)?;
            let re = Self::from_escaped(&coerced)?;
            replace_(vm, globals, &re, given, hash_val)
        }
    }

    /// Replaces all non-overlapping matches in `given` string using hash lookup.
    pub(crate) fn replace_all_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        hash_val: Value,
    ) -> Result<(String, bool)> {
        fn replace_(
            vm: &mut Executor,
            globals: &mut Globals,
            re: &RegexpInner,
            given: &str,
            hash_val: Value,
        ) -> Result<(String, bool)> {
            let mut range = vec![];
            let hash = hash_val.as_hash();

            vm.clear_capture_special_variables();
            for cap in re.captures_iter(given) {
                let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
                let m = cap.get(0).unwrap();

                let matched_str = m.as_str();
                let key = Value::string_from_str(matched_str);
                vm.save_capture_special_variables(&cap);
                let replacement = if let Some(v) = hash.get(key, vm, globals)? {
                    v.to_s(&globals.store)
                } else {
                    String::new()
                };

                range.push((m.range(), replacement));
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
            replace_(vm, globals, &re, given, hash_val)
        } else if let Some(re) = re_val.is_regex() {
            replace_(vm, globals, &re, given, hash_val)
        } else {
            let coerced = re_val.coerce_to_str(vm, globals)?;
            let re = Self::from_escaped(&coerced)?;
            replace_(vm, globals, &re, given, hash_val)
        }
    }

    pub(crate) fn match_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re: Regexp,
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
                let match_data = Value::new_matchdata(captures, given, re);
                if let Some(bh) = block {
                    vm.invoke_block_once(globals, bh, &[match_data])
                } else {
                    Ok(match_data)
                }
            }
        }
    }

    /// Like `match_one` but returns only a boolean and does NOT set `$~`.
    pub(crate) fn match_pred(
        re: &RegexpInner,
        given: &str,
        char_pos: usize,
    ) -> Result<bool> {
        let byte_pos = match given.char_indices().nth(char_pos) {
            Some((pos, _)) => pos,
            None => return Ok(false),
        };
        match re.regex.captures_from_pos(given, byte_pos) {
            Ok(res) => Ok(res.is_some()),
            Err(err) => Err(MonorubyErr::regexerr(format!("Capture failed. {:?}", err))),
        }
    }

    pub(crate) fn scan(&self, vm: &mut Executor, given: &str) -> Result<Vec<Value>> {
        let mut ary = vec![];
        let mut last_captures = None;
        vm.clear_capture_special_variables();
        for cap in self.regex.captures_iter(given) {
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
        let mut replacements = vec![];
        vm.clear_capture_special_variables();
        let mut last_captures = None;
        for cap in self.captures_iter(given) {
            let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
            let m = cap.get(0).unwrap();
            let rep = Self::expand_backref(replace, &cap);
            replacements.push((m.range(), rep));
            last_captures = Some(cap);
        }
        let mut res = given.to_string();
        let is_empty = replacements.is_empty();
        for (r, rep) in replacements.into_iter().rev() {
            res.replace_range(r, &rep);
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c)
        }

        Ok((res, !is_empty))
    }

    /// Expand backreference sequences (`\1`, `\2`, etc.) in `replace` using `captures`.
    fn expand_backref(replace: &str, captures: &Captures) -> String {
        let mut rep = String::new();
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
                    '\\' => rep.push('\\'),
                    _ => {
                        rep.push('\\');
                        rep.push(ch);
                    }
                };
                escape = false;
            } else if ch != '\\' {
                rep.push(ch);
            } else {
                escape = true;
            }
        }
        rep
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
                let rep = Self::expand_backref(replace, &captures);
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
