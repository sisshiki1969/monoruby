use std::os::unix::fs::DirBuilderExt;
use std::path::PathBuf;

use super::*;

/// Return a human-readable description for an `std::io::Error`, matching CRuby's Errno messages.
fn errno_description(err: &std::io::Error) -> &'static str {
    match err.raw_os_error() {
        Some(1) => "Operation not permitted",
        Some(2) => "No such file or directory",
        Some(13) => "Permission denied",
        Some(17) => "File exists",
        Some(20) => "Not a directory",
        Some(21) => "Is a directory",
        Some(22) => "Invalid argument",
        Some(39) => "Directory not empty",
        _ => "Unknown error",
    }
}

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
    globals.define_builtin_class_func(klass, "entries", entries, 1);
    globals.define_builtin_class_funcs(klass, "rmdir", &["delete", "unlink"], rmdir, 1);
}

///
/// ### Dir.rmdir / Dir.delete / Dir.unlink
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/rmdir.html]
#[monoruby_builtin]
fn rmdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
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
fn mkdir(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(_vm, globals)?;
    let mode = if let Some(m) = lfp.try_arg(1) {
        m.coerce_to_i64(&globals.store)? as u32
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
            .map(|v| v.coerce_to_str(vm, globals))
            .collect::<Result<_>>()?
    } else {
        vec![pat_val.coerce_to_str(vm, globals)?]
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
        .map(|v| v.coerce_to_str(vm, globals))
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
fn split_by_top_level_comma(s: &str) -> Vec<&str> {
    let mut result = vec![];
    let mut depth = 0usize;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
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
/// Unmatched braces are returned as-is.
fn expand_braces(pattern: &str) -> Vec<String> {
    // Find the first top-level `{`.
    let mut depth = 0usize;
    let mut open = None;
    for (i, c) in pattern.char_indices() {
        match c {
            '{' => {
                if depth == 0 {
                    open = Some(i);
                }
                depth += 1;
            }
            '}' => {
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
    }
    // No complete brace group found — return as-is.
    vec![pattern.to_string()]
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
            s => PathComponent::Name(match globset::Glob::new(s) {
                Ok(g) => g,
                Err(e) => {
                    return Err(MonorubyErr::runtimeerr(format!(
                        "invalid glob pattern {:?}: {}",
                        s, e
                    )));
                }
            }),
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
        path.coerce_to_string(vm, globals)?
    } else {
        dirs::home_dir().unwrap().to_string_lossy().to_string()
    };
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let old_pwd = std::env::current_dir().unwrap();
        match std::env::set_current_dir(&path) {
            Ok(_) => {}
            Err(err) => {
                return Err(MonorubyErr::runtimeerr(err.to_string()));
            }
        }
        let path = Value::string(path);
        let res = vm.invoke_block(globals, &data, &[path]);
        let _ = std::env::set_current_dir(old_pwd);
        res
    } else {
        std::env::set_current_dir(&path)
            .map_err(|e| MonorubyErr::runtimeerr(format!("Dir.chdir: {}: {}", path, e)))?;
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
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let mut result = vec![
        Value::string(".".to_string()),
        Value::string("..".to_string()),
    ];
    for entry in
        std::fs::read_dir(&path).map_err(|e| MonorubyErr::runtimeerr(format!("{}: {}", path, e)))?
    {
        let entry = entry.map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
        result.push(Value::string(
            entry.file_name().to_string_lossy().to_string(),
        ));
    }
    Ok(Value::array_from_vec(result))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn exist() {
        run_test_once(r#"Dir.exist?(".")"#);
        run_test_once(r#"Dir.exist?("..")"#);
        run_test_once(r#"Dir.exist?("src")"#);
        run_test_once(r#"Dir.exist?("nonexistent_dir_xyz")"#);
        run_test_once(r#"Dir.exist?("Cargo.toml")"#);
    }

    #[test]
    fn glob() {
        run_test_once(r#"Dir.glob("b*")"#);
        run_test_once(r#"Dir.glob("*.rb")"#);
        run_test_once(r#"Dir.glob("Cargo?????")"#);
        run_test_once(r#"Dir.glob("d{a,c}*")"#);
        run_test_once(r#"Dir.glob("/*")"#);
        run_test_once(r#"Dir.glob("././././C*")"#);
        run_test_once(r#"Dir.glob("../../../../*")"#);
        run_test_once(r#"Dir.glob("../*")"#);
        run_test_once(r#"Dir.glob("src/builtins/*.rs")"#);
        run_test_once(r#"Dir["src/builtins/*.rs"]"#);
        run_test_once(r#"Dir.glob("src/**/*.rs").sort"#);
        run_test_once(r#"Dir.glob("/")"#);
        run_test_once(r#"Dir.glob(".")"#);
        run_test_once(r#"Dir.glob("")"#);
        run_test_once(r#"Dir.glob("*", base: "src/builtins")"#);
        // Array of patterns (merged, sorted, deduped — same as CRuby).
        run_test_once(r#"Dir.glob(["b*", "*.toml"])"#);
        run_test_once(r#"Dir["b*", "*.toml"]"#);
        // FNM_DOTMATCH: wildcards match dot-files.
        run_test_once(r#"Dir.glob(".*")"#);
        run_test_once(r#"Dir.glob("*", File::FNM_DOTMATCH)"#);
        // Brace alternation containing `/` — must be expanded before path-splitting.
        run_test_once(r#"Dir.glob("{,*,*/*,*/*/*}.rs").sort"#);
        run_test_once(r#"Dir.glob("src/{lib,builtins}/*.rs")"#);
    }

    /// Tests that do not require CRuby comparison.
    #[test]
    fn glob_extensions() {
        // sort: false — just verify it runs without error.
        run_test_once(r#"Dir.glob("b*", sort: false).sort"#);
        // block form — verify it does not raise.
        run_test_once(r#"res = []; Dir.glob("b*") { |f| res << f.upcase }; res"#);
        // ** matches zero directories (direct child).
        run_test_once(r#"Dir.glob("src/**/*.rs").include?("src/lib.rs")"#);
        // ** matches multiple levels.
        run_test_once(r#"Dir.glob("src/**/*.rs").include?("src/builtins/dir.rs")"#);
        // Array of patterns.
        run_test_once(r#"Dir.glob(["C*", "*.toml"])"#);
    }

    #[test]
    fn home() {
        run_test_once(r#"Dir.home"#);
    }

    #[test]
    fn pwd() {
        run_test_once(r#"Dir.pwd"#);
        run_test_once(r#"Dir.getwd"#);
        run_test_once(
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
        run_test_once(
            r#"
            $x = []
            path = "/tmp/monoruby_test_mkdir_#{Process.pid}"
            Dir.mkdir(path)
            $x << Dir.exist?(path)
            Dir.rmdir(path)
            $x << Dir.exist?(path)
            $x
            "#,
        );
    }

    #[test]
    fn dir_entries() {
        run_test_once(
            r#"
            Dir.entries(".").sort
            "#,
        );
    }
}
