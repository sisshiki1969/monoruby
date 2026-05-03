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
        // CRuby's `Regexp#inspect` includes the `n` (NOENCODING /
        // ASCII-8BIT) flag but not `u`/`e`/`s` (which are normalised
        // away). Order is m-i-x-n.
        if option & Self::NOENCODING != 0 {
            res.push('n');
        }
        res
    }

    pub fn escape(text: &str) -> String {
        regex::escape(text)
    }
}

/// Expand Ruby's `\u{XXXX}` / `\u{XX YY ZZ}` regex-literal escapes into the
/// forms Onigmo understands (`\uHHHH` for BMP, raw UTF-8 for supplementary).
///
/// Onigmo's `\u` handler only accepts exactly four hex digits, so the
/// Ruby-level brace form must be normalized before the source is handed off.
/// Leaves every other escape (including `\\u{...}`) untouched.
fn expand_unicode_braces(src: &str) -> Result<String> {
    if !src.contains("\\u{") {
        return Ok(src.to_string());
    }
    let bytes = src.as_bytes();
    let mut out = String::with_capacity(src.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            if i + 2 < bytes.len() && bytes[i + 1] == b'u' && bytes[i + 2] == b'{' {
                let content_start = i + 3;
                if let Some(rel_end) = bytes[content_start..].iter().position(|&b| b == b'}') {
                    let content = std::str::from_utf8(&bytes[content_start..content_start + rel_end])
                        .map_err(|_| MonorubyErr::regexerr("invalid utf-8 in \\u{...}"))?;
                    let mut buf = String::new();
                    let mut ok = true;
                    let mut empty = true;
                    for tok in content.split_ascii_whitespace() {
                        empty = false;
                        if tok.is_empty() || tok.len() > 6 || !tok.bytes().all(|b| b.is_ascii_hexdigit()) {
                            ok = false;
                            break;
                        }
                        match u32::from_str_radix(tok, 16) {
                            Ok(cp) if cp <= 0x10FFFF => {
                                if cp <= 0xFFFF {
                                    use std::fmt::Write;
                                    let _ = write!(buf, "\\u{:04X}", cp);
                                } else if let Some(ch) = char::from_u32(cp) {
                                    buf.push(ch);
                                } else {
                                    ok = false;
                                    break;
                                }
                            }
                            _ => {
                                ok = false;
                                break;
                            }
                        }
                    }
                    if ok && !empty {
                        out.push_str(&buf);
                        i = content_start + rel_end + 1;
                        continue;
                    }
                    return Err(MonorubyErr::regexerr("invalid Unicode escape \\u{...}"));
                }
            }
            // Copy the backslash and the following character (if any) verbatim
            // so escapes like `\\`, `\u0041`, `\x{...}` are preserved.
            out.push('\\');
            i += 1;
            if i < bytes.len() {
                let ch_len = utf8_char_len(bytes[i]);
                out.push_str(&src[i..i + ch_len]);
                i += ch_len;
            }
            continue;
        }
        // Copy one UTF-8 scalar verbatim.
        let ch_len = utf8_char_len(bytes[i]);
        out.push_str(&src[i..i + ch_len]);
        i += ch_len;
    }
    Ok(out)
}

fn utf8_char_len(b: u8) -> usize {
    if b < 0x80 {
        1
    } else if b < 0xC0 {
        1 // continuation byte (should not be seen as a leader, but avoid panic)
    } else if b < 0xE0 {
        2
    } else if b < 0xF0 {
        3
    } else {
        4
    }
}

impl RegexpInner {

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
        let reg_str = expand_unicode_braces(&reg_str)?;
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
                    vm.save_capture_special_variables(captures, given)
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
        format!(
            "/{}/{}",
            escape_unescaped_slashes(self.as_str()),
            self.option_string()
        )
    }
}

/// Escape forward slashes that aren't already escaped, leaving every
/// other backslash sequence intact. Used by `Regexp#inspect` so that
/// `Regexp.new("/foo/bar").inspect` is `"/\\/foo\\/bar/"` (matching
/// CRuby) without double-escaping `Regexp.new('\\\/')` to
/// `"/\\\\\\\\\\//"` etc.
fn escape_unescaped_slashes(src: &str) -> String {
    let mut out = String::with_capacity(src.len() + 4);
    let mut chars = src.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                // Pass through the backslash and whatever it escapes
                // (single char) verbatim, so existing `\/`/`\\`/`\n`/
                // ... aren't double-escaped.
                out.push('\\');
                if let Some(next) = chars.next() {
                    out.push(next);
                }
            }
            '/' => {
                out.push('\\');
                out.push('/');
            }
            other => out.push(other),
        }
    }
    out
}

// Utility methods

impl RegexpInner {
    /// Resolve `re_val` to a `RegexpInner` and run `f` on it. Accepts:
    ///   - an existing `Regexp` (used in place);
    ///   - a `String` (treated as a regexp source, escaped via
    ///     `from_escaped`);
    ///   - any other object that responds to `to_str` (the returned
    ///     String is then treated as a regexp source).
    /// All `replace_*` entry points share this dispatch — keep it in
    /// one place so the to_str-coercion semantics stay consistent.
    fn with_coerced_regexp<R>(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        f: impl FnOnce(&RegexpInner, &mut Executor, &mut Globals) -> Result<R>,
    ) -> Result<R> {
        if let Some(re) = re_val.is_regex() {
            return f(&re, vm, globals);
        }
        let s_owned;
        let s: &str = if let Some(s) = re_val.is_str() {
            s
        } else {
            s_owned = re_val.coerce_to_str(vm, globals)?;
            &s_owned
        };
        let re = Self::from_escaped(s)?;
        f(&re, vm, globals)
    }

    /// Replaces the leftmost-first match with `replace`.
    pub(crate) fn replace_one(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, _| {
            re.replace_once(vm, given, replace)
        })
        .map(|(s, c)| (s, c.is_some()))
    }

    pub(crate) fn replace_one_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        bh: BlockHandler,
    ) -> Result<(String, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            match re.captures(given, vm)? {
                None => Ok((given.to_string(), false)),
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    let (start, end, matched_str) = (m.start(), m.end(), m.as_str());
                    let mut res = given.to_string();
                    let matched = Value::string_from_str(matched_str);
                    let result = vm.invoke_block_once(globals, bh, &[matched])?;
                    let s = block_result_to_string(vm, globals, result)?;
                    res.replace_range(start..end, &s);
                    Ok((res, true))
                }
            }
        })
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all(
        vm: &mut Executor,
        globals: &mut Globals,
        regexp: Value,
        given: &str,
        replace: &str,
    ) -> Result<(String, bool)> {
        Self::with_coerced_regexp(vm, globals, regexp, |re, vm, _| {
            re.replace_repeat(vm, given, replace)
        })
    }

    /// Replaces all non-overlapping matches in `given` string with `replace`.
    pub(crate) fn replace_all_block(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        bh: BlockHandler,
        self_enc: Option<crate::value::Encoding>,
    ) -> Result<(String, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            let mut range = vec![];
            let data = vm.get_block_data(globals, bh)?;

            vm.clear_capture_special_variables();
            for cap in re.captures_iter(given) {
                let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
                let m = cap.get(0).unwrap();

                let matched_str = m.as_str();
                let matched = Value::string_from_str(matched_str);
                vm.save_capture_special_variables(&cap, given);
                let result = vm.invoke_block(globals, &data, &[matched])?;
                // CRuby raises Encoding::CompatibilityError if the
                // block returned a String whose encoding can't merge
                // with self's. Check before stringifying.
                if let Some(enc) = self_enc {
                    if let Some(repl_inner) = result.is_rstring_inner() {
                        let dummy = crate::value::RStringInner::from_encoding(b"", enc);
                        if dummy.compatible_encoding(&repl_inner).is_none() {
                            return Err(MonorubyErr::incompatible_encoding(
                                &globals.store,
                                enc,
                                repl_inner.encoding(),
                            ));
                        }
                    }
                }
                let replace = block_result_to_string(vm, globals, result)?;

                range.push((m.range(), replace));
            }

            let mut res = given.to_string();
            let is_empty = range.is_empty();

            for (range, replace) in range.into_iter().rev() {
                res.replace_range(range, &replace);
            }

            Ok((res, !is_empty))
        })
    }

    /// Replaces the first match in `given` string using hash lookup.
    /// For each match, the matched text is looked up as a key in the
    /// hash via `Hash#[]` so that any user-defined `default` / `default_proc`
    /// fires; values are coerced via `to_s`.
    pub(crate) fn replace_one_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        hash_val: Value,
    ) -> Result<(String, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            match re.captures(given, vm)? {
                None => Ok((given.to_string(), false)),
                Some(captures) => {
                    let m = captures.get(0).unwrap();
                    let (start, end, matched_str) = (m.start(), m.end(), m.as_str());
                    let mut res = given.to_string();
                    let key = Value::string_from_str(matched_str);
                    let replacement = lookup_hash_replacement(vm, globals, hash_val, key)?;
                    res.replace_range(start..end, &replacement);
                    Ok((res, true))
                }
            }
        })
    }

    /// Replaces all non-overlapping matches in `given` string using hash lookup.
    pub(crate) fn replace_all_hash(
        vm: &mut Executor,
        globals: &mut Globals,
        re_val: Value,
        given: &str,
        hash_val: Value,
    ) -> Result<(String, bool)> {
        Self::with_coerced_regexp(vm, globals, re_val, |re, vm, globals| {
            let mut range = vec![];

            vm.clear_capture_special_variables();
            for cap in re.captures_iter(given) {
                let cap = cap.map_err(|err| MonorubyErr::regexerr(format!("{err}")))?;
                let m = cap.get(0).unwrap();

                let matched_str = m.as_str();
                let key = Value::string_from_str(matched_str);
                vm.save_capture_special_variables(&cap, given);
                let replacement = lookup_hash_replacement(vm, globals, hash_val, key)?;

                range.push((m.range(), replacement));
            }

            let mut res = given.to_string();
            let is_empty = range.is_empty();

            for (range, replace) in range.into_iter().rev() {
                res.replace_range(range, &replace);
            }

            Ok((res, !is_empty))
        })
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
        // Walk the haystack manually rather than relying on
        // `captures_iter`, which can skip the zero-width match that
        // sits between two non-empty matches (e.g.
        // `"¿por qué?".gsub(/([a-z\d]*)/, "*")` — the empty position
        // immediately after `"por"` is observable in CRuby but the
        // bundled iterator collapses it). For empty matches we
        // advance by one Unicode scalar so the loop terminates.
        let mut replacements = vec![];
        vm.clear_capture_special_variables();
        let mut last_captures: Option<Captures> = None;
        let mut pos = 0usize;
        while pos <= given.len() {
            let cap = match self.captures_from_pos(given, pos, vm)? {
                Some(c) => c,
                None => break,
            };
            let m = cap.get(0).unwrap();
            let (start, end) = (m.start(), m.end());
            let rep = self.expand_backref(replace, given, &cap);
            replacements.push((start..end, rep));
            last_captures = Some(cap);
            pos = if end > start {
                end
            } else if start >= given.len() {
                given.len() + 1
            } else {
                let mut next = start + 1;
                while next < given.len() && !given.is_char_boundary(next) {
                    next += 1;
                }
                next
            };
        }
        let mut res = given.to_string();
        let is_empty = replacements.is_empty();
        for (r, rep) in replacements.into_iter().rev() {
            res.replace_range(r, &rep);
        }

        if let Some(c) = last_captures {
            vm.save_capture_special_variables(&c, given)
        }

        Ok((res, !is_empty))
    }

    /// Expand backreference sequences in `replace` using `captures`
    /// against `given` (the original haystack). Recognises:
    ///
    /// - `\0`, `\1`-`\9`: numbered captures (`\0` is the full match).
    /// - `\&`: same as `\0` (full match).
    /// - `` \` ``: pre-match (everything before the match).
    /// - `\'`: post-match (everything after the match).
    /// - `\+`: highest-numbered participating capture.
    /// - `\k<name>`: named capture by `<name>`.
    /// - `\\`: literal backslash.
    /// - Trailing `\` is left as a literal backslash.
    /// - Other `\X` sequences are passed through verbatim.
    fn expand_backref(
        &self,
        replace: &str,
        given: &str,
        captures: &Captures,
    ) -> String {
        let bytes = replace.as_bytes();
        let mut rep = String::new();
        let mut i = 0;
        while i < bytes.len() {
            let ch = bytes[i];
            if ch != b'\\' {
                // copy one UTF-8 scalar
                let len = utf8_char_len(ch);
                rep.push_str(&replace[i..i + len]);
                i += len;
                continue;
            }
            // ch == '\\'
            if i + 1 >= bytes.len() {
                // Trailing backslash: copy verbatim (CRuby leaves it).
                rep.push('\\');
                i += 1;
                continue;
            }
            let next = bytes[i + 1];
            match next {
                b'0'..=b'9' => {
                    let idx = (next - b'0') as usize;
                    if let Some(m) = captures.get(idx) {
                        rep.push_str(m.as_str());
                    }
                    i += 2;
                }
                b'&' => {
                    if let Some(m) = captures.get(0) {
                        rep.push_str(m.as_str());
                    }
                    i += 2;
                }
                b'`' => {
                    if let Some(m) = captures.get(0) {
                        rep.push_str(&given[..m.start()]);
                    }
                    i += 2;
                }
                b'\'' => {
                    if let Some(m) = captures.get(0) {
                        rep.push_str(&given[m.end()..]);
                    }
                    i += 2;
                }
                b'+' => {
                    // Highest-numbered participating capture *group*
                    // (1..). If the regex has no capture groups —
                    // even when there's a full match — `\+` expands
                    // to the empty string, matching CRuby.
                    let mut idx = captures.len();
                    while idx > 1 {
                        idx -= 1;
                        if let Some(m) = captures.get(idx) {
                            rep.push_str(m.as_str());
                            break;
                        }
                    }
                    i += 2;
                }
                b'\\' => {
                    rep.push('\\');
                    i += 2;
                }
                b'k' => {
                    // `\k<name>` — named backreference.
                    if i + 2 < bytes.len() && bytes[i + 2] == b'<' {
                        if let Some(end_off) = bytes[i + 3..].iter().position(|&b| b == b'>') {
                            let name_start = i + 3;
                            let name_end = name_start + end_off;
                            let name = &replace[name_start..name_end];
                            // Onigmo stores capture names; look up the
                            // rightmost group with this name (CRuby
                            // chooses the last participating one).
                            // Onigmo allows multiple groups to share
                            // a name; pick the highest-numbered one
                            // that participated, matching CRuby.
                            let members = self.get_group_members(name);
                            let mut chosen: Option<usize> = None;
                            for &m_idx in members.iter() {
                                if let Some(m) = captures.get(m_idx as usize) {
                                    let _ = m;
                                    chosen = Some(m_idx as usize);
                                }
                            }
                            if let Some(idx) = chosen {
                                if let Some(m) = captures.get(idx) {
                                    rep.push_str(m.as_str());
                                }
                            }
                            i = name_end + 1;
                            continue;
                        }
                    }
                    // Malformed `\k…`: copy verbatim.
                    rep.push('\\');
                    rep.push('k');
                    i += 2;
                }
                _ => {
                    // Unknown `\X`: keep as-is (preserves e.g. `\d`).
                    let len = utf8_char_len(next);
                    rep.push('\\');
                    rep.push_str(&replace[i + 1..i + 1 + len]);
                    i += 1 + len;
                }
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
                let rep = self.expand_backref(replace, given, &captures);
                res.replace_range(m.start()..m.end(), &rep);
                Ok((res, Some(captures)))
            }
        }
    }
}

/// Coerce the result of a `String#sub`/`#gsub` block to a String,
/// calling user-defined `to_s` so a mock returning a non-String
/// from `to_s` is exercised. Falls back to monoruby's intrinsic
/// `to_s` rendering when the object's `to_s` doesn't return a String.
fn block_result_to_string(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<String> {
    if let Some(s) = v.is_str() {
        return Ok(s.to_string());
    }
    let coerced = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
    if let Some(s) = coerced.is_str() {
        Ok(s.to_string())
    } else {
        Ok(coerced.to_s(&globals.store))
    }
}

/// Look up the replacement string for a `String#sub`/`#gsub` match
/// when the second argument is a `Hash`. Calls `Hash#[]` so that any
/// user-defined `default` / `default_proc` fires; missing keys
/// (where `[]` returns `nil` because no default is set) are replaced
/// with the empty string. Values are coerced via `Object#to_s` per
/// CRuby.
fn lookup_hash_replacement(
    vm: &mut Executor,
    globals: &mut Globals,
    hash: Value,
    key: Value,
) -> Result<String> {
    let v =
        vm.invoke_method_inner(globals, IdentId::get_id("[]"), hash, &[key], None, None)?;
    if v.is_nil() {
        Ok(String::new())
    } else if let Some(s) = v.is_str() {
        Ok(s.to_string())
    } else {
        // CRuby coerces non-String hash values via `Object#to_s`.
        let s = vm.invoke_method_inner(globals, IdentId::TO_S, v, &[], None, None)?;
        match s.is_str() {
            Some(s) => Ok(s.to_string()),
            None => Ok(s.to_s(&globals.store)),
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

#[cfg(test)]
mod expand_unicode_braces_tests {
    use super::expand_unicode_braces;

    fn ok(input: &str) -> String {
        expand_unicode_braces(input).expect("expected success")
    }

    #[test]
    fn passthrough_when_no_brace() {
        assert_eq!(ok(""), "");
        assert_eq!(ok("abc"), "abc");
        assert_eq!(ok("\\uFFFF"), "\\uFFFF");
        assert_eq!(ok("\\x{20}"), "\\x{20}");
        assert_eq!(ok("\\n\\t"), "\\n\\t");
    }

    #[test]
    fn bmp_codepoint_is_zero_padded() {
        assert_eq!(ok("\\u{20}"), "\\u0020");
        assert_eq!(ok("\\u{7e}"), "\\u007E");
        assert_eq!(ok("\\u{0041}"), "\\u0041");
        assert_eq!(ok("\\u{FFFF}"), "\\uFFFF");
    }

    #[test]
    fn range_with_braces() {
        assert_eq!(ok("[\\u{20}-\\u{7e}]"), "[\\u0020-\\u007E]");
    }

    #[test]
    fn supplementary_plane_emits_raw_utf8() {
        // U+1F600 😀 => 4-byte UTF-8 sequence
        assert_eq!(ok("\\u{1F600}"), "😀");
    }

    #[test]
    fn multi_codepoint_brace() {
        assert_eq!(ok("\\u{20 7e}"), "\\u0020\\u007E");
        assert_eq!(ok("\\u{41 42 43}"), "\\u0041\\u0042\\u0043");
    }

    #[test]
    fn escaped_backslash_is_preserved() {
        // `\\u{20}` => literal backslash followed by `u{20}` characters
        assert_eq!(ok("\\\\u{20}"), "\\\\u{20}");
    }

    #[test]
    fn mixed_with_surrounding_regex_syntax() {
        assert_eq!(ok("^(\\u{41})+$"), "^(\\u0041)+$");
    }

    #[test]
    fn rejects_invalid_hex() {
        assert!(expand_unicode_braces("\\u{xyz}").is_err());
    }

    #[test]
    fn rejects_empty_braces() {
        assert!(expand_unicode_braces("\\u{}").is_err());
    }

    #[test]
    fn rejects_out_of_range() {
        assert!(expand_unicode_braces("\\u{110000}").is_err());
    }

    #[test]
    fn bmp_surrogate_passes_through_as_four_digits() {
        // Surrogate values are in the BMP range; we emit them as \uHHHH and
        // leave validation to Onigmo (same behavior as CRuby for regex literals).
        assert_eq!(ok("\\u{D800}"), "\\uD800");
    }

    #[test]
    fn preserves_non_ascii_after_backslash() {
        assert_eq!(ok("\\あ"), "\\あ");
    }

    #[test]
    fn unterminated_brace_is_left_alone() {
        // No closing `}` — fall through and let Onigmo surface its own error.
        assert_eq!(ok("\\u{20"), "\\u{20");
    }
}
