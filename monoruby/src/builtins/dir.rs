use std::os::unix::fs::DirBuilderExt;
use std::path::PathBuf;

use super::*;

//
// Dir class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Dir").id();
    globals.define_builtin_class_func_with_kw(
        klass,
        "glob",
        glob,
        1,
        2,
        false,
        &["base", "sort"],
        false,
    );
    globals.define_builtin_class_func_with_kw(
        klass,
        "[]",
        glob2,
        0,
        0,
        true,
        &["base", "sort"],
        false,
    );
    globals.define_builtin_class_func_with(klass, "home", home, 0, 1, false);
    globals.define_builtin_class_funcs(klass, "pwd", &["getwd"], pwd, 0);
    globals.define_builtin_class_func_with(klass, "chdir", chdir, 0, 1, false);
    globals.define_builtin_class_func(klass, "exist?", exist, 1);
    globals.define_builtin_class_func_with(klass, "mkdir", mkdir, 1, 2, false);
    globals.define_builtin_class_func_with(klass, "entries", entries, 1, 2, false);
    globals.define_builtin_class_func_with(klass, "foreach", foreach, 1, 2, false);
    globals.define_builtin_class_funcs(klass, "rmdir", &["delete", "unlink"], rmdir, 1);

    // Methods that need libc syscalls or external state. The rest of Dir's
    // surface (initialize/open/read/close/pos/each/children/empty? …) lives
    // in monoruby/builtins/builtins.rb so the iterator state can be plain
    // Ruby ivars.
    globals.define_builtin_class_func(klass, "fchdir", fchdir, 1);
    globals.define_builtin_class_func(klass, "chroot", chroot, 1);
    globals.define_builtin_func(klass, "fileno", dir_fileno, 0);
    globals.define_builtin_func_with(klass, "chdir", dir_inst_chdir, 0, 0, false);
}

///
/// ### Dir.children
/// - children(path) -> [String]
///
/// Same as `Dir.entries` but excludes the `"."` and `".."` entries.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/children.html]
#[monoruby_builtin]
fn children(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_path_rstring(vm, globals)?.to_str()?.to_string();
    let mut result = vec![];
    for entry in std::fs::read_dir(&path)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?
    {
        let entry =
            entry.map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?;
        result.push(Value::string(
            entry.file_name().to_string_lossy().to_string(),
        ));
    }
    Ok(Value::array_from_vec(result))
}

///
/// ### Dir.foreach
/// - foreach(path) {|name| ... } -> nil
/// - foreach(path) -> Enumerator
///
/// Yields each entry name (including `"."` and `".."`) under `path` to the
/// block. Without a block, returns an Array (Enumerator is not yet
/// supported).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/foreach.html]
#[monoruby_builtin]
fn foreach(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_path_rstring(vm, globals)?.to_str()?.to_string();
    let mut names = vec![".".to_string(), "..".to_string()];
    for entry in std::fs::read_dir(&path)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?
    {
        let entry =
            entry.map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?;
        names.push(entry.file_name().to_string_lossy().to_string());
    }
    if let Some(bh) = lfp.block() {
        let p = vm.get_block_data(globals, bh)?;
        for name in names {
            vm.invoke_block(globals, &p, &[Value::string(name)])?;
        }
        Ok(Value::nil())
    } else {
        Ok(Value::array_from_vec(
            names.into_iter().map(Value::string).collect(),
        ))
    }
}

///
/// ### Dir.rmdir / Dir.delete / Dir.unlink
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/rmdir.html]
#[monoruby_builtin]
fn rmdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    std::fs::remove_dir(&path).map_err(|e| {
        let desc = errno_description(&e);
        MonorubyErr::from_io_err(globals, &e, format!("{} @ dir_s_rmdir - {}", desc, path))
    })?;
    Ok(Value::integer(0))
}

///
/// ### Dir.mkdir
///
/// - mkdir(path, mode = 0777) -> 0
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/mkdir.html]
#[monoruby_builtin]
fn mkdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    let mode = if let Some(m) = lfp.try_arg(1) {
        m.coerce_to_int_i64(vm, globals)? as u32
    } else {
        0o777
    };
    match std::fs::DirBuilder::new().mode(mode).create(&path) {
        Ok(()) => Ok(Value::integer(0)),
        Err(e) => {
            let desc = errno_description(&e);
            Err(MonorubyErr::from_io_err(
                globals,
                &e,
                format!("{} @ dir_s_mkdir - {}", desc, path),
            ))
        }
    }
}

/// File::FNM_DOTMATCH: wildcards match dotfiles too.
const FNM_DOTMATCH: i64 = 4;

#[derive(Debug, Clone)]
struct PathPair {
    path: PathBuf,
    full: PathBuf,
}

impl PathPair {
    fn new(path: PathBuf, full: PathBuf) -> Self {
        Self { path, full }
    }

    fn push(&mut self, name: &str) {
        self.path.push(name);
        self.full.push(name);
    }

    fn parent(&mut self) {
        self.path.pop();
        self.full.push("..");
    }

    fn current(&mut self) {
        self.full.push(".");
    }
}

#[derive(Debug, Clone)]
enum PathComponent {
    /// A literal name or single-level glob pattern (e.g. `*.rs`).
    Name(globset::Glob),
    /// `..` — go up one directory.
    Parent,
    /// `.` — stay in current directory.
    Current,
    /// Trailing separator: emit the current path as a match.
    None,
    /// `**` — match zero or more directory levels recursively.
    Globstar,
}

///
/// ### Dir.glob
///
/// - glob(pattern, flags = 0, base: nil, sort: true) -> [String]
/// - glob(pattern, flags = 0, base: nil, sort: true) {|file| ...} -> nil
///
/// `pattern` may be a String or an Array of Strings.
/// Supported flags: `File::FNM_DOTMATCH` (4) — include dot-files in wildcards.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/glob.html]
#[monoruby_builtin]
fn glob(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let pat_val = lfp.arg(0);
    let flags = lfp.try_arg(1).and_then(|v| v.try_fixnum()).unwrap_or(0);
    let base = if let Some(base) = lfp.try_arg(2)
        && !base.is_nil()
    {
        Some(
            base.coerce_to_path_rstring(vm, globals)?
                .to_str()?
                .to_string(),
        )
    } else {
        None
    };
    let sort = lfp.try_arg(3).map(|v| v.as_bool()).unwrap_or(true);

    // Accept a single String pattern or an Array of String patterns.
    let patterns: Vec<String> = if pat_val.is_array_ty() {
        pat_val
            .as_array_inner()
            .iter()
            .map(|v| Ok(v.coerce_to_path_rstring(vm, globals)?.to_str()?.to_string()))
            .collect::<Result<_>>()?
    } else {
        vec![pat_val
            .coerce_to_path_rstring(vm, globals)?
            .to_str()?
            .to_string()]
    };

    let all_matches = glob_impl(patterns, flags, base, sort)?;

    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        for m in all_matches {
            vm.invoke_block(globals, &data, &[Value::string_from_inner(m)])?;
        }
        Ok(Value::nil())
    } else {
        Ok(Value::array_from_iter(
            all_matches.into_iter().map(Value::string_from_inner),
        ))
    }
}

///
/// ### Dir.[]
///
/// - self[*pattern, base: nil, sort: true] -> [String]
///
/// Supported flags: `File::FNM_DOTMATCH` (4) — include dot-files in wildcards.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/glob.html]
#[monoruby_builtin]
fn glob2(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let pat_val = lfp.arg(0).as_array();
    let flags = 0;
    let base = if let Some(base) = lfp.try_arg(1)
        && !base.is_nil()
    {
        Some(
            base.coerce_to_path_rstring(vm, globals)?
                .to_str()?
                .to_string(),
        )
    } else {
        None
    };
    let sort = lfp.try_arg(2).map(|v| v.as_bool()).unwrap_or(true);

    // Accept a single String pattern or an Array of String patterns.
    let patterns: Vec<String> = pat_val
        .iter()
        .map(|v| Ok(v.coerce_to_path_rstring(vm, globals)?.to_str()?.to_string()))
        .collect::<Result<_>>()?;

    let all_matches: Vec<RStringInner> = glob_impl(patterns, flags, base, sort)?;

    lfp.expect_no_block()?;
    Ok(Value::array_from_iter(
        all_matches.into_iter().map(|s| Value::string_from_inner(s)),
    ))
}

fn glob_impl(
    patterns: Vec<String>,
    flags: i64,
    base: Option<String>,
    sort: bool,
) -> Result<Vec<RStringInner>> {
    let dotmatch = (flags & FNM_DOTMATCH) != 0;

    let mut all_matches = vec![];
    for pattern_str in &patterns {
        let mut matches = vec![];
        process_glob_pattern(pattern_str, base.as_deref(), dotmatch, &mut matches)?;
        matches.dedup();
        if sort {
            matches.sort_by(|a, b| a.cmp(b));
        }
        all_matches.extend(matches);
    }

    let all_matches: Vec<RStringInner> = all_matches
        .into_iter()
        .map(|s| RStringInner::from_string(s))
        .collect();
    Ok(all_matches)
}

/// Split `s` at top-level commas, ignoring commas inside nested `{...}`.
/// Backslash-escaped `\{`, `\}`, and `\,` are treated as literal characters
/// (CRuby glob semantics) and do not affect brace depth or splitting.
fn split_by_top_level_comma(s: &str) -> Vec<&str> {
    let mut result = vec![];
    let mut depth = 0usize;
    let mut start = 0;
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'\\' && i + 1 < bytes.len() {
            // Skip the escape and the next byte verbatim.
            i += 2;
            continue;
        }
        match c {
            b'{' => depth += 1,
            b'}' => depth = depth.saturating_sub(1),
            b',' if depth == 0 => {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    result.push(&s[start..]);
    result
}

/// Expand brace alternations in `pattern`, returning all expanded strings.
///
/// Examples:
/// - `{,*,*/*}.gemspec` → `[".gemspec", "*.gemspec", "*/*.gemspec"]`
/// - `d{a,c}*`          → `["da*", "dc*"]`
/// - `{a,b}{c,d}`       → `["ac", "ad", "bc", "bd"]`
///
/// Backslash escapes (`\{`, `\}`) are treated as literal characters and do
/// not start or close a brace group — matching CRuby's glob semantics.
/// Unmatched braces are returned as-is.
fn expand_braces(pattern: &str) -> Vec<String> {
    // Find the first top-level `{`, skipping escaped braces.
    let mut depth = 0usize;
    let mut open = None;
    let bytes = pattern.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'\\' && i + 1 < bytes.len() {
            // Escape sequence: skip both bytes.
            i += 2;
            continue;
        }
        match c {
            b'{' => {
                if depth == 0 {
                    open = Some(i);
                }
                depth += 1;
            }
            b'}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    if let Some(start) = open {
                        let prefix = &pattern[..start];
                        let inside = &pattern[start + 1..i];
                        let suffix = &pattern[i + 1..];
                        let mut results = vec![];
                        for alt in split_by_top_level_comma(inside) {
                            let expanded = format!("{}{}{}", prefix, alt, suffix);
                            results.extend(expand_braces(&expanded));
                        }
                        return results;
                    }
                    break;
                }
            }
            _ => {}
        }
        i += 1;
    }
    // No complete brace group found — return as-is.
    vec![pattern.to_string()]
}

/// Translate a single glob path segment from CRuby conventions into the
/// dialect that `globset::Glob::new` accepts:
///
/// - `\{` and `\}` (escaped braces) become `[{]` / `[}]` so the literal
///   character survives when `globset` would otherwise treat them as
///   alternate-group delimiters.
/// - Any remaining unescaped `{` / `}` after `expand_braces` ran are
///   leftovers from unbalanced input; convert them to `[{]` / `[}]` too so
///   the segment compiles instead of erroring out.
/// - A trailing dangling `\` is doubled to `\\` (literal backslash) since
///   CRuby treats it as a literal backslash and `globset` errors out.
fn normalize_glob_segment(seg: &str) -> String {
    let bytes = seg.as_bytes();
    let mut out = String::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c == b'\\' {
            if i + 1 >= bytes.len() {
                // Trailing dangling backslash → literal backslash.
                out.push_str("\\\\");
                i += 1;
                continue;
            }
            let next = bytes[i + 1];
            if next == b'{' {
                out.push_str("[{]");
                i += 2;
                continue;
            }
            if next == b'}' {
                out.push_str("[}]");
                i += 2;
                continue;
            }
            // Other escapes pass through to globset, which already handles
            // them (`\*`, `\?`, `\\`, etc.).
            out.push(c as char);
            out.push(next as char);
            i += 2;
            continue;
        }
        if c == b'{' {
            out.push_str("[{]");
            i += 1;
            continue;
        }
        if c == b'}' {
            out.push_str("[}]");
            i += 1;
            continue;
        }
        out.push(c as char);
        i += 1;
    }
    out
}

/// Parse one glob pattern string and append matches to `matches`.
fn process_glob_pattern(
    pattern_str: &str,
    base: Option<&str>,
    dotmatch: bool,
    matches: &mut Vec<String>,
) -> Result<()> {
    if pattern_str.is_empty() {
        // Empty pattern matches nothing (CRuby behavior).
        return Ok(());
    }

    // Expand brace alternations before path-splitting, so that patterns like
    // `{,*,*/*}.gemspec` (where `{}` contains `/`) are handled correctly.
    let expanded = expand_braces(pattern_str);
    if expanded.len() != 1 || expanded[0] != pattern_str {
        for pat in expanded {
            process_glob_pattern(&pat, base, dotmatch, matches)?;
        }
        return Ok(());
    }

    let mut segments = pattern_str.split(std::path::MAIN_SEPARATOR_STR).peekable();

    // Determine the root PathPair.
    let path = if segments.peek() == Some(&"") {
        // Absolute path — starts with `/`.
        segments.next();
        if segments.peek().is_none() {
            // Pattern was exactly "/".
            if std::path::Path::new("/").exists() {
                matches.push("/".to_string());
            }
            return Ok(());
        }
        PathPair::new(PathBuf::from("/"), PathBuf::from("/"))
    } else if let Some(base) = base {
        let mut p = std::env::current_dir().unwrap();
        p.push(base);
        match p.canonicalize() {
            Ok(p) => PathPair::new(p, PathBuf::new()),
            Err(_) => return Ok(()),
        }
    } else {
        PathPair::new(std::env::current_dir().unwrap(), PathBuf::new())
    };

    // Build the component list from the remaining path segments.
    let mut components: Vec<PathComponent> = vec![];
    for seg in segments {
        components.push(match seg {
            "." => PathComponent::Current,
            ".." => PathComponent::Parent,
            "" => PathComponent::None,
            "**" => PathComponent::Globstar,
            s => {
                let normalized = normalize_glob_segment(s);
                PathComponent::Name(match globset::Glob::new(&normalized) {
                    Ok(g) => g,
                    Err(e) => {
                        return Err(MonorubyErr::runtimeerr(format!(
                            "invalid glob pattern {:?}: {}",
                            s, e
                        )));
                    }
                })
            }
        });
    }

    traverse_dir(path, components, matches, dotmatch)
}

fn traverse_dir(
    mut path: PathPair,
    mut glob_rest: Vec<PathComponent>,
    matches: &mut Vec<String>,
    dotmatch: bool,
) -> Result<()> {
    loop {
        if glob_rest.is_empty() {
            matches.push(path.full.to_string_lossy().to_string());
            return Ok(());
        }
        match glob_rest.remove(0) {
            PathComponent::None => {
                matches.push(path.full.to_string_lossy().to_string());
                return Ok(());
            }
            PathComponent::Parent => {
                path.parent();
            }
            PathComponent::Current => {
                path.current();
            }

            // `**` — match zero or more directory levels.
            PathComponent::Globstar => {
                // Zero levels: apply remaining components at the current directory.
                traverse_dir(path.clone(), glob_rest.clone(), matches, dotmatch)?;

                // One or more levels: descend into each subdirectory and keep `**`.
                let entries = match std::fs::read_dir(&path.path) {
                    Ok(e) => e,
                    Err(_) => return Ok(()),
                };
                let mut dirs: Vec<String> = entries
                    .flatten()
                    .filter_map(|e| {
                        let ft = e.file_type().ok()?;
                        if !ft.is_dir() {
                            return None;
                        }
                        let name = e.file_name().to_string_lossy().to_string();
                        if name.starts_with('.') && !dotmatch {
                            return None;
                        }
                        Some(name)
                    })
                    .collect();
                dirs.sort();
                for name in dirs {
                    let mut new_path = path.clone();
                    new_path.push(&name);
                    let mut new_glob = vec![PathComponent::Globstar];
                    new_glob.extend(glob_rest.iter().cloned());
                    traverse_dir(new_path, new_glob, matches, dotmatch)?;
                }
                return Ok(());
            }

            // Literal name or single-level glob pattern for one path segment.
            PathComponent::Name(glob) => {
                let glob = glob.compile_matcher();
                let entries = match std::fs::read_dir(&path.path) {
                    Ok(e) => e,
                    Err(_) => return Ok(()),
                };
                let pat_starts_with_dot = glob.glob().glob().starts_with('.');
                let mut to_visit: Vec<(String, PathPair)> = entries
                    .flatten()
                    .filter_map(|e| {
                        let name = e.file_name().to_string_lossy().to_string();
                        if name.starts_with('.') && !pat_starts_with_dot && !dotmatch {
                            return None;
                        }
                        if glob.is_match_candidate(&globset::Candidate::new(&name)) {
                            let mut new_path = path.clone();
                            new_path.push(&name);
                            Some((name, new_path))
                        } else {
                            None
                        }
                    })
                    .collect();

                // Synthesize "." (the directory itself): read_dir omits it, but
                // CRuby includes it when the pattern matches dot-files and "."
                // itself matches the pattern.  ".." is never included (CRuby also
                // excludes it).
                if (pat_starts_with_dot || dotmatch)
                    && glob.is_match_candidate(&globset::Candidate::new("."))
                {
                    let mut dot_path = path.clone();
                    dot_path.push(".");
                    to_visit.push((".".to_string(), dot_path));
                }

                to_visit.sort_by(|a, b| a.0.cmp(&b.0));
                for (_, new_path) in to_visit {
                    traverse_dir(new_path, glob_rest.clone(), matches, dotmatch)?;
                }
                return Ok(());
            }
        }
    }
}

///
/// ### Dir.home
///
/// - home -> String | nil
/// - [NOT SUPPORTED] home(user) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/home.html]
#[monoruby_builtin]
fn home(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let home = match dirs::home_dir() {
        Some(home) => home,
        None => return Ok(Value::nil()),
    };
    Ok(Value::string(home.to_string_lossy().to_string()))
}

///
/// ### Dir.getwd
///
/// - getwd -> String
/// - pwd -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/getwd.html]
#[monoruby_builtin]
fn pwd(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let pwd = std::env::current_dir()
        .unwrap()
        .to_string_lossy()
        .to_string();
    Ok(Value::string(pwd))
}

///
/// ### Dir.chdir
///
/// - chdir -> 0
/// - chdir(path) -> 0
/// - chdir {|path| ... } -> object
/// - chdir(path) {|path| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/chdir.html]
#[monoruby_builtin]
fn chdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = if let Some(path) = lfp.try_arg(0) {
        path.coerce_to_path_rstring(vm, globals)?.to_str()?.to_string()
    } else {
        dirs::home_dir().unwrap().to_string_lossy().to_string()
    };
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let old_pwd = std::env::current_dir().unwrap();
        match std::env::set_current_dir(&path) {
            Ok(_) => {}
            Err(err) => {
                return Err(MonorubyErr::errno_with_msg(&globals.store, &err, &path));
            }
        }
        let path = Value::string(path);
        let res = vm.invoke_block(globals, &data, &[path]);
        let _ = std::env::set_current_dir(old_pwd);
        res
    } else {
        std::env::set_current_dir(&path)
            .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?;
        Ok(Value::integer(0))
    }
}

///
/// ### Dir.exist?
///
/// - exist?(path) -> bool
///
/// Returns `true` if the given path exists and is a directory, `false` otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/exist=3f.html]
#[monoruby_builtin]
fn exist(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path_str = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    let path = std::path::Path::new(&path_str);
    Ok(Value::bool(path.is_dir()))
}

///
/// ### Dir.entries
///
/// - entries(path) -> [String]
///
/// Returns an array containing all of the filenames in the given directory.
/// Includes "." and ".." entries.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/entries.html]
#[monoruby_builtin]
fn entries(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    let mut result = vec![
        Value::string(".".to_string()),
        Value::string("..".to_string()),
    ];
    for entry in
        std::fs::read_dir(&path).map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?
    {
        let entry = entry.map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?;
        result.push(Value::string(
            entry.file_name().to_string_lossy().to_string(),
        ));
    }
    Ok(Value::array_from_vec(result))
}

/// Read the `@path` ivar set by Ruby-side `Dir#initialize`.
fn dir_path_ivar(globals: &Globals, self_: Value) -> Result<String> {
    match globals
        .store
        .get_ivar(self_, IdentId::get_id("@path"))
    {
        Some(v) if !v.is_nil() => Ok(v.to_s(&globals.store)),
        _ => Err(MonorubyErr::ioerr("uninitialized Dir")),
    }
}

fn dir_check_closed(globals: &Globals, self_: Value) -> Result<()> {
    let v = globals
        .store
        .get_ivar(self_, IdentId::get_id("@closed"));
    if v.map(|v| v.as_bool()).unwrap_or(false) {
        Err(MonorubyErr::ioerr("closed directory"))
    } else {
        Ok(())
    }
}

///
/// ### Dir#chdir
/// - chdir -> 0
/// - chdir { ... } -> Object
///
/// Changes the working directory to the directory represented by self.
/// Without a block, the change persists until another `chdir` is issued.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/i/chdir.html]
#[monoruby_builtin]
fn dir_inst_chdir(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = dir_path_ivar(globals, lfp.self_val())?;
    let saved = if lfp.block().is_some() {
        Some(
            std::env::current_dir()
                .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, "."))?,
        )
    } else {
        None
    };
    std::env::set_current_dir(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_dir_s_chdir", &path)
    })?;
    if let Some(bh) = lfp.block() {
        let result = vm.invoke_block_once(globals, bh, &[lfp.self_val()]);
        if let Some(prev) = saved {
            let _ = std::env::set_current_dir(&prev);
        }
        return result;
    }
    Ok(Value::integer(0))
}

///
/// ### Dir#fileno
/// - fileno -> Integer
///
/// Returns a file descriptor for the directory by opening it with
/// `O_DIRECTORY`. monoruby does not currently keep a `DIR *` open per Dir
/// instance, so each call opens a fresh descriptor — comparing two `fileno`
/// values from the same Dir will give the same number for the same path.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/i/fileno.html]
#[monoruby_builtin]
fn dir_fileno(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    dir_check_closed(globals, self_)?;
    let path = dir_path_ivar(globals, self_)?;
    let c = std::ffi::CString::new(path.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("path contains NUL byte"))?;
    // SAFETY: O_DIRECTORY | O_RDONLY is a POSIX-defined open mode.
    let fd = unsafe { libc::open(c.as_ptr(), libc::O_DIRECTORY | libc::O_RDONLY) };
    if fd < 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "rb_dir_s_fileno",
            &path,
        ));
    }
    Ok(Value::integer(fd as i64))
}

///
/// ### Dir.fchdir
/// - fchdir(fd) -> 0
/// - fchdir(fd) { ... } -> Object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/fchdir.html]
#[monoruby_builtin]
fn fchdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let fd = lfp.arg(0).coerce_to_int_i64(vm, globals)? as i32;
    let saved = if lfp.block().is_some() {
        Some(
            std::env::current_dir()
                .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, "."))?,
        )
    } else {
        None
    };
    // SAFETY: fchdir(2) is a POSIX system call. Invalid fds surface as EBADF.
    let rc = unsafe { libc::fchdir(fd) };
    if rc != 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, ""));
    }
    if let Some(bh) = lfp.block() {
        let result = vm.invoke_block_once(globals, bh, &[]);
        if let Some(prev) = saved {
            let _ = std::env::set_current_dir(&prev);
        }
        return result;
    }
    Ok(Value::integer(0))
}

///
/// ### Dir.chroot
/// - chroot(path) -> 0
///
/// Changes the root directory for the process. Requires CAP_SYS_CHROOT
/// (typically root); raises `Errno::EPERM` otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/chroot.html]
#[monoruby_builtin]
fn chroot(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp
        .arg(0)
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string();
    let c = std::ffi::CString::new(path.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("path contains NUL byte"))?;
    // SAFETY: chroot(2) is a POSIX system call. Failures surface via errno.
    let rc = unsafe { libc::chroot(c.as_ptr()) };
    if rc != 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "rb_dir_s_chroot",
            &path,
        ));
    }
    Ok(Value::integer(0))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn exist() {
        run_tests(&[
            r#"Dir.exist?(".")"#,
            r#"Dir.exist?("..")"#,
            r#"Dir.exist?("src")"#,
            r#"Dir.exist?("nonexistent_dir_xyz")"#,
            r#"Dir.exist?("Cargo.toml")"#,
        ]);
    }

    #[test]
    fn glob() {
        run_tests(&[
            r#"Dir.glob("b*")"#,
            r#"Dir.glob("*.rb")"#,
            r#"Dir.glob("Cargo?????")"#,
            r#"Dir.glob("d{a,c}*")"#,
            r#"Dir.glob("/*")"#,
            r#"Dir.glob("././././C*")"#,
            r#"Dir.glob("../../../../*")"#,
            r#"Dir.glob("../*")"#,
            r#"Dir.glob("src/builtins/*.rs")"#,
            r#"Dir["src/builtins/*.rs"]"#,
            r#"Dir.glob("src/**/*.rs").sort"#,
            r#"Dir.glob("/")"#,
            r#"Dir.glob(".")"#,
            r#"Dir.glob("")"#,
            r#"Dir.glob("*", base: "src/builtins")"#,
            // Array of patterns (merged, sorted, deduped — same as CRuby).
            r#"Dir.glob(["b*", "*.toml"])"#,
            r#"Dir["b*", "*.toml"]"#,
            // FNM_DOTMATCH: wildcards match dot-files.
            r#"Dir.glob(".*")"#,
            r#"Dir.glob("*", File::FNM_DOTMATCH)"#,
            // Brace alternation containing `/` — must be expanded before path-splitting.
            r#"Dir.glob("{,*,*/*,*/*/*}.rs").sort"#,
            r#"Dir.glob("src/{lib,builtins}/*.rs")"#,
        ]);
    }

    /// Tests that do not require CRuby comparison.
    #[test]
    fn glob_extensions() {
        run_tests(&[
            // sort: false — just verify it runs without error.
            r#"Dir.glob("b*", sort: false).sort"#,
            // block form — verify it does not raise.
            r#"res = []; Dir.glob("b*") { |f| res << f.upcase }; res"#,
            // ** matches zero directories (direct child).
            r#"Dir.glob("src/**/*.rs").include?("src/lib.rs")"#,
            // ** matches multiple levels.
            r#"Dir.glob("src/**/*.rs").include?("src/builtins/dir.rs")"#,
            // Array of patterns.
            r#"Dir.glob(["C*", "*.toml"])"#,
        ]);
    }

    #[test]
    fn home() {
        run_test(r#"Dir.home"#);
    }

    #[test]
    fn pwd() {
        run_test(r#"Dir.pwd"#);
        run_test(r#"Dir.getwd"#);
        run_test(
            r##"
        $x = []
        $x << Dir.getwd
        Dir.chdir("../") do |path|
            $x << path
            $x << Dir.getwd
        end
        $x << Dir.getwd
        $x
        "##,
        );
    }

    #[test]
    fn mkdir() {
        // if the directory exists, CRuby raise Errno::EEXIST.
        run_test_error("Dir.mkdir('/tmp')");
        // mkdir creates a new directory
        run_test(
            r##"
            $x = []
            path = "/tmp/monoruby_test_mkdir_#{Process.pid}"
            Dir.mkdir(path)
            $x << Dir.exist?(path)
            Dir.rmdir(path)
            $x << Dir.exist?(path)
            $x
            "##,
        );
    }

    #[test]
    fn dir_entries() {
        run_test(
            r##"
            Dir.entries(".").sort
            "##,
        );
    }

    #[test]
    fn rmdir_non_directory() {
        // Dir.rmdir on a regular file should raise Errno::ENOTDIR
        run_test_error("Dir.rmdir('Cargo.toml')");
    }

    #[test]
    fn mkdir_existing() {
        // Dir.mkdir on an existing directory should raise Errno::EEXIST
        run_test_error("Dir.mkdir('/tmp')");
    }

    #[test]
    fn dir_foreach_block() {
        // Returned entries are unordered; sort before compare so the result
        // is stable across implementations.
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_foreach_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              File.write("#{base}/a", "")
              File.write("#{base}/b", "")
              names = []
              Dir.foreach(base) { |n| names << n }
              names.sort
            ensure
              File.unlink("#{base}/a") rescue nil
              File.unlink("#{base}/b") rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_children_class_method() {
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_children_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              File.write("#{base}/x", "")
              File.write("#{base}/y", "")
              Dir.children(base).sort
            ensure
              File.unlink("#{base}/x") rescue nil
              File.unlink("#{base}/y") rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_each_child_class_method() {
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_eachchild_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              File.write("#{base}/x", "")
              File.write("#{base}/y", "")
              names = []
              Dir.each_child(base) { |n| names << n }
              names.sort
            ensure
              File.unlink("#{base}/x") rescue nil
              File.unlink("#{base}/y") rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_instance_basic() {
        // CRuby's Dir#pos returns an opaque seekdir cookie while monoruby
        // uses an Array index, so don't compare pos values directly. Both
        // implementations must move forward on read and reset on rewind, so
        // verify those behaviors instead.
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_inst_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              File.write("#{base}/a", "")
              d = Dir.new(base)
              entries = []
              while (e = d.read) ; entries << e ; end
              d.rewind
              first_after_rewind = d.read
              [entries.sort, [".", "..", "a"].include?(first_after_rewind), d.path == base, d.pos.is_a?(Integer)]
            ensure
              d&.close
              File.unlink("#{base}/a") rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_open_with_block() {
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_openblk_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              kids_count = Dir.open(base) { |d| d.children.length }
              kids_count
            ensure
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_close_then_read_raises() {
        run_test_error(
            r##"
            base = "/tmp/monoruby_dir_closed_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              d = Dir.new(base)
              d.close
              d.read
            ensure
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_inst_chdir_with_block() {
        run_test_once(
            r##"
            before = Dir.pwd
            d = Dir.new("/tmp")
            inside = d.chdir { Dir.pwd }
            after = Dir.pwd
            d.close
            [inside == "/tmp", before == after]
            "##,
        );
    }

    #[test]
    fn dir_fileno_returns_integer() {
        run_test_once(
            r##"
            d = Dir.new("/tmp")
            begin
              fd = d.fileno
              fd.is_a?(Integer) && fd >= 0
            ensure
              d.close
            end
            "##,
        );
    }

    #[test]
    fn dir_fchdir() {
        run_test_once(
            r##"
            before = Dir.pwd
            d = Dir.new("/tmp")
            fd = d.fileno
            inside = nil
            Dir.fchdir(fd) { inside = Dir.pwd }
            after = Dir.pwd
            d.close
            [inside == "/tmp", before == after]
            "##,
        );
    }

    #[test]
    fn dir_chroot_argument_required() {
        // Calling without a path is always an ArgumentError regardless of
        // privileges, so this is safe to run on any uid (in particular,
        // it does not actually chroot the test process).
        run_test_error(r#"Dir.chroot"#);
    }

    #[test]
    fn dir_glob_brace_escape() {
        // The temp paths embed Process.pid which differs between the two
        // runs being compared, so reduce the result to a stable shape: the
        // number of matches and a tail-relative path.
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_brace_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            inner = "#{base}/test{1}"
            Dir.mkdir(inner)
            begin
              File.write("#{inner}/file", "")
              # Backslash-escaped braces must match the literal { } chars.
              matches = Dir.glob("#{base}/test\\{1\\}/file")
              [matches.length, matches.first&.end_with?("/test{1}/file")]
            ensure
              File.unlink("#{inner}/file") rescue nil
              Dir.rmdir(inner) rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_inst_children_excludes_dots() {
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_instchildren_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              File.write("#{base}/x", "")
              File.write("#{base}/y", "")
              d = Dir.new(base)
              kids = d.children.sort
              has_dot    = kids.include?(".")
              has_dotdot = kids.include?("..")
              d.close
              [kids, has_dot, has_dotdot]
            ensure
              File.unlink("#{base}/x") rescue nil
              File.unlink("#{base}/y") rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_inst_each_child_yields_only_children() {
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_eachinst_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            begin
              File.write("#{base}/a", "")
              File.write("#{base}/b", "")
              d = Dir.new(base)
              names = []
              d.each_child { |n| names << n }
              d.close
              names.sort
            ensure
              File.unlink("#{base}/a") rescue nil
              File.unlink("#{base}/b") rescue nil
              Dir.rmdir(base) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn dir_chroot_missing_path_raises() {
        // Errno::ENOENT under root, Errno::EPERM under non-root — either
        // way it's a SystemCallError and CRuby/monoruby raise the same
        // class on the same euid. As root with a missing target it's
        // ENOENT, which is what the harness will agree on.
        run_test_error(r#"Dir.chroot("/no_such_dir_xyz_qq")"#);
    }

    // ----- error patterns --------------------------------------------------

    #[test]
    fn dir_new_nonexistent_raises() {
        run_test_error(r#"Dir.new("/no_such_dir_xyz_qq_for_new")"#);
    }

    #[test]
    fn dir_open_nonexistent_raises() {
        run_test_error(r#"Dir.open("/no_such_dir_xyz_qq_for_open")"#);
    }

    #[test]
    fn dir_fchdir_invalid_fd_raises() {
        run_test_error(r#"Dir.fchdir(-1)"#);
    }

    #[test]
    fn dir_fchdir_with_non_integer_raises() {
        run_test_error(r#"Dir.fchdir("not an fd")"#);
    }

    #[test]
    fn dir_foreach_missing_path_raises() {
        run_test_error(
            r#"Dir.foreach("/no_such_dir_xyz_qq_foreach") { |_| }"#,
        );
    }

    #[test]
    fn dir_inst_chdir_to_missing_path_raises() {
        // CRuby holds the directory open via DIR*+fchdir, so chdir to a
        // removed-but-still-open dir succeeds in CRuby and surfaces ENOENT
        // in monoruby. Avoid that observability gap and instead exercise a
        // path that was never a real directory.
        run_test_once(
            r##"
            base = "/tmp/monoruby_dir_chdir_missing_#{Process.pid}_#{rand(100000)}"
            Dir.mkdir(base)
            d = Dir.new(base)
            d.close
            raised = false
            begin
              d.instance_variable_set(:@path, "/no_such_dir_qq_xyz_for_chdir")
              d.chdir
            rescue SystemCallError, IOError
              raised = true
            end
            raised
            "##,
        );
    }
}
