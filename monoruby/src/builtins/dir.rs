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
        &["base", "sort", "flags"],
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
    globals.define_builtin_class_func_with_kw(
        klass,
        "entries",
        entries,
        1,
        1,
        false,
        &["encoding"],
        false,
    );
    globals.define_builtin_class_func_with_kw(
        klass,
        "foreach",
        foreach,
        1,
        2,
        false,
        &["encoding"],
        false,
    );
    globals.define_builtin_class_funcs(klass, "rmdir", &["delete", "unlink"], rmdir, 1);

    // Methods that need libc syscalls or external state. The rest of Dir's
    // surface (initialize/open/read/close/pos/each/children/empty? …) lives
    // in monoruby/builtins/dir.rb so the iterator state can be plain
    // Ruby ivars.
    globals.define_builtin_class_func(klass, "fchdir", fchdir, 1);
    globals.define_builtin_class_func(klass, "chroot", chroot, 1);
    globals.define_builtin_func_with(klass, "chdir", dir_inst_chdir, 0, 0, false);
    // fd-backed internals for builtins/dir.rb: every Dir instance holds a
    // real O_DIRECTORY descriptor so `fileno`, `Dir.for_fd`, and the
    // close(2)-level double-close detection behave like CRuby's DIR*.
    globals.define_builtin_class_func(klass, "__open_fd", dir_open_fd, 1);
    globals.define_builtin_class_func(klass, "__close_fd", dir_close_fd, 1);
    globals.define_builtin_class_func(klass, "__entries_fd", dir_entries_fd, 2);
}

/// Read a directory's entry names (including "." and "..") through a
/// *duplicate* of `fd` so the caller's descriptor position/ownership is
/// untouched, tagging each name with `enc` (or the default external
/// encoding) and transcoding to the default internal encoding when set
/// (entries whose bytes don't convert keep the external tag).
fn read_entries_via_fd(
    globals: &mut Globals,
    fd: i32,
    enc_obj: Option<Value>,
) -> Result<Vec<Value>> {
    // SAFETY: dup(2) then fdopendir(3); the DIR* takes ownership of the
    // dup'd fd and is released with closedir below. readdir entries are
    // copied out before the next readdir call.
    unsafe {
        let dup = libc::dup(fd);
        if dup < 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "readdir"));
        }
        let dirp = libc::fdopendir(dup);
        if dirp.is_null() {
            let err = std::io::Error::last_os_error();
            libc::close(dup);
            return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "readdir"));
        }
        // dup(2) shares the directory offset with the original fd (a
        // previous full listing leaves it at EOF), so always start over.
        libc::rewinddir(dirp);
        let mut names: Vec<Vec<u8>> = vec![];
        loop {
            let ent = libc::readdir(dirp);
            if ent.is_null() {
                break;
            }
            let name = std::ffi::CStr::from_ptr((*ent).d_name.as_ptr());
            names.push(name.to_bytes().to_vec());
        }
        libc::closedir(dirp);
        Ok(names
            .into_iter()
            .map(|n| external_str_with_enc(globals, n, enc_obj))
            .collect())
    }
}

/// Tag one directory entry the way dir.c does, through CRuby's
/// `rb_external_str_with_enc`: a name that is **not** all-ASCII under a
/// US-ASCII target encoding is tagged ASCII-8BIT instead — and skips
/// the `default_internal` conversion — rather than carrying a US-ASCII
/// tag its own bytes are invalid in. Every other case is the ordinary
/// external/internal tagging. The rule is the target encoding's, so it
/// fires for an explicit `encoding: "US-ASCII"` just as it does for a
/// US-ASCII `Encoding.find("filesystem")`.
fn external_str_with_enc(globals: &mut Globals, bytes: Vec<u8>, enc_obj: Option<Value>) -> Value {
    use crate::value::Encoding as E;
    let target = match enc_obj {
        Some(o) => super::io::enc_obj_to_enum(globals, o),
        None => {
            let de = super::io::enc_default_external_obj(globals);
            super::io::enc_obj_to_enum(globals, de)
        }
    };
    if target == Some(E::UsAscii) && !bytes.is_ascii() {
        return super::file::path_value(&bytes, E::Ascii8);
    }
    super::io::tag_with_encs(globals, bytes, enc_obj, None)
}

/// Resolve an `encoding:` keyword value (String name or Encoding
/// object; nil/absent → None = default external).
fn entries_enc_obj(globals: &Globals, v: Option<Value>) -> Option<Value> {
    v.filter(|v| !v.is_nil())
        .and_then(|v| super::io::arg_to_enc_obj(globals, v))
}

///
/// ### Dir.children
/// - children(path) -> [String]
///
/// Same as `Dir.entries` but excludes the `"."` and `".."` entries.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/children.html]
/// List a directory's entry names as raw byte vectors (no "." / "..").
fn read_dir_names(globals: &Globals, path: &RString) -> Result<Vec<Vec<u8>>> {
    use std::os::unix::ffi::OsStrExt;
    let display = String::from_utf8_lossy(path.as_bytes()).to_string();
    let dir = super::file::bytes_to_pathbuf(path.as_bytes());
    let mut result = vec![];
    for entry in std::fs::read_dir(&dir)
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &display))?
    {
        let entry = entry.map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &display))?;
        result.push(entry.file_name().as_os_str().as_bytes().to_vec());
    }
    Ok(result)
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
fn foreach(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    // The `encoding:` keyword lives in slot 2; the spare positional
    // slot 1 carries it through an Enumerator replay (which passes
    // positional arguments only).
    let enc_val = lfp.try_arg(2).or_else(|| lfp.try_arg(1));
    // Without a block, return a (lazy, size-less) Enumerator that replays
    // `Dir.foreach(path)` when iterated — matching CRuby, which defers the
    // directory read (and any ENOENT) until enumeration.
    let Some(bh) = lfp.block() else {
        let method = IdentId::get_id("foreach");
        let mut args = vec![lfp.arg(0)];
        if let Some(e) = enc_val
            && !e.is_nil()
        {
            args.push(e);
        }
        return vm.generate_enumerator(method, lfp.self_val(), args, pc);
    };
    let path = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    super::file::check_path_encoding(globals, &path)?;
    let enc_obj = entries_enc_obj(globals, enc_val);
    let mut names: Vec<Vec<u8>> = vec![b".".to_vec(), b"..".to_vec()];
    names.extend(read_dir_names(globals, &path)?);
    let entries: Vec<Value> = names
        .into_iter()
        .map(|n| external_str_with_enc(globals, n, enc_obj))
        .collect();
    let p = vm.get_block_data(globals, bh)?;
    // Root the not-yet-yielded name strings: the block body reaches
    // safepoints, and a bare Rust `Vec<Value>` is invisible to the GC
    // mark phase (caught by the true `gc-stress` mode).
    vm.with_temp_scope(|vm| {
        vm.temp_array_new(entries.len());
        vm.temp_array_extend_from_slice(&entries);
        for name in entries {
            vm.invoke_block(globals, &p, &[name])?;
        }
        Ok(Value::nil())
    })
}

///
/// ### Dir.rmdir / Dir.delete / Dir.unlink
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/rmdir.html]
#[monoruby_builtin]
fn rmdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rs = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    super::file::check_path_encoding(globals, &rs)?;
    let path = super::file::bytes_to_pathbuf(rs.as_bytes());
    let display = path.to_string_lossy().to_string();
    std::fs::remove_dir(&path).map_err(|e| {
        let desc = errno_description(&e);
        MonorubyErr::from_io_err(globals, &e, format!("{} @ dir_s_rmdir - {}", desc, display))
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
    let rs = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    super::file::check_path_encoding(globals, &rs)?;
    let path = super::file::bytes_to_pathbuf(rs.as_bytes());
    let display = path.to_string_lossy().to_string();
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
                format!("{} @ dir_s_mkdir - {}", desc, display),
            ))
        }
    }
}

/// `File::FNM_*` flag bits honoured by `Dir.glob`.
const FNM_NOESCAPE: i64 = 1;
const FNM_DOTMATCH: i64 = 4;
const FNM_CASEFOLD: i64 = 8;

/// Translate `Dir.glob`'s Integer flags into the per-segment matcher
/// flags. macOS's default filesystem is case-insensitive; CRuby
/// compiles with `HAVE_CASEFOLD_FILESYSTEM` there and folds every
/// glob, so mirror that.
fn seg_flags(flags: i64) -> u32 {
    use super::fnmatch as fm;
    let mut f = 0;
    if flags & FNM_DOTMATCH != 0 {
        f |= fm::FNM_DOTMATCH;
    }
    if flags & FNM_NOESCAPE != 0 {
        f |= fm::FNM_NOESCAPE;
    }
    if flags & FNM_CASEFOLD != 0 {
        f |= fm::FNM_CASEFOLD;
    }
    #[cfg(target_os = "macos")]
    {
        f |= fm::FNM_CASEFOLD;
    }
    f
}

#[derive(Debug, Clone)]
struct PathPair {
    /// Filesystem path (used for directory reads / existence checks).
    path: PathBuf,
    /// The user-facing match string, built verbatim from the pattern:
    /// `.` / `..` components and doubled slashes are preserved exactly
    /// as written (CRuby glob output).
    full: String,
}

impl PathPair {
    fn new(path: PathBuf, full: String) -> Self {
        Self { path, full }
    }

    /// Append one output component with a separator (unless at the
    /// very start or just after the absolute root). A preceding
    /// [`Self::extra_slash`] shows through as a doubled `/`.
    fn push_out(&mut self, name: &str) {
        if !(self.full.is_empty() || self.full == "/") {
            self.full.push('/');
        }
        self.full.push_str(name);
    }

    fn push(&mut self, name: &str) {
        self.path.push(name);
        self.push_out(name);
    }

    fn parent(&mut self) {
        self.path.pop();
        self.push_out("..");
    }

    fn current(&mut self) {
        self.push_out(".");
    }

    /// An interior empty pattern segment (`a//b`): the extra `/` is
    /// preserved in the output verbatim.
    fn extra_slash(&mut self) {
        self.full.push('/');
    }
}

#[derive(Debug, Clone, PartialEq)]
enum PathComponent {
    /// A literal name or single-level glob pattern (matched with
    /// [`super::fnmatch::match_segment`]).
    Name(String),
    /// `..` — go up one directory.
    Parent,
    /// `.` — stay in current directory.
    Current,
    /// An interior empty segment (`a//b`) — preserved in the output.
    ExtraSlash,
    /// A `**` segment followed by `/` — match zero or more directory
    /// levels recursively.
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
    // `flags:` keyword wins over the positional flags argument
    // (core/dir/glob_spec.rb "prefers the keyword argument").
    let flags = if let Some(f) = lfp.try_arg(4)
        && !f.is_nil()
    {
        f.coerce_to_int_i64(vm, globals)?
    } else {
        lfp.try_arg(1).and_then(|v| v.try_fixnum()).unwrap_or(0)
    };
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
    let sort = validate_sort(globals, lfp.try_arg(3))?;

    // Accept a single String pattern or an Array of String patterns.
    // A NUL byte raises ArgumentError; the message differs between the
    // single-String form ("nul-separated glob pattern is deprecated")
    // and the Array form ("path name contains null byte"), matching
    // CRuby.
    let patterns: Vec<(String, crate::value::Encoding)> = if pat_val.is_array_ty() {
        pat_val
            .as_array_inner()
            .iter()
            .map(|v| {
                let s = glob_pattern(vm, globals, *v)?;
                reject_array_pattern_nul(&s.0)?;
                Ok(s)
            })
            .collect::<Result<_>>()?
    } else {
        let s = glob_pattern(vm, globals, pat_val)?;
        reject_single_pattern_nul(&s.0)?;
        vec![s]
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
    let sort = validate_sort(globals, lfp.try_arg(2))?;

    // `Dir[pat]` with a single argument behaves like `Dir.glob(pat)`
    // (single-String NUL message); with several it behaves like the
    // Array form.
    let single = pat_val.len() == 1;
    let patterns: Vec<(String, crate::value::Encoding)> = pat_val
        .iter()
        .map(|v| {
            let s = glob_pattern(vm, globals, *v)?;
            if single {
                reject_single_pattern_nul(&s.0)?;
            } else {
                reject_array_pattern_nul(&s.0)?;
            }
            Ok(s)
        })
        .collect::<Result<_>>()?;

    let all_matches: Vec<RStringInner> = glob_impl(patterns, flags, base, sort)?;

    lfp.expect_no_block()?;
    Ok(Value::array_from_iter(
        all_matches.into_iter().map(|s| Value::string_from_inner(s)),
    ))
}

/// Validate a `sort:` keyword argument. CRuby accepts only `true`/`false`
/// (or an absent argument, defaulting to `true`); anything else raises
/// `ArgumentError: expected true or false as sort: <inspect>`.
fn validate_sort(globals: &Globals, arg: Option<Value>) -> Result<bool> {
    match arg {
        None => Ok(true),
        Some(v) if v == Value::bool(true) => Ok(true),
        Some(v) if v == Value::bool(false) => Ok(false),
        Some(v) => Err(MonorubyErr::argumenterr(format!(
            "expected true or false as sort: {}",
            v.inspect(&globals.store)
        ))),
    }
}

/// A single-String glob pattern containing a NUL byte. CRuby reports
/// this with a glob-specific (deprecation) message.
fn reject_single_pattern_nul(s: &str) -> Result<()> {
    if s.contains('\0') {
        return Err(MonorubyErr::argumenterr(
            "nul-separated glob pattern is deprecated",
        ));
    }
    Ok(())
}

/// An Array element / multi-argument glob pattern containing a NUL byte.
/// CRuby reports this with the generic string→path null-byte message.
fn reject_array_pattern_nul(s: &str) -> Result<()> {
    if s.contains('\0') {
        return Err(MonorubyErr::argumenterr("path name contains null byte"));
    }
    Ok(())
}

/// Coerce one glob pattern, keeping its encoding for the result tags.
/// An ASCII-incompatible pattern encoding raises CRuby's
/// `Encoding::CompatibilityError` ("… UTF-16BE and US-ASCII").
fn glob_pattern(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<(String, crate::value::Encoding)> {
    let rs = v.coerce_to_path_rstring_allow_nul(vm, globals)?;
    let enc = rs.encoding();
    if !enc.is_ascii_compatible() {
        return Err(MonorubyErr::incompatible_encoding(
            &globals.store,
            enc,
            crate::value::Encoding::UsAscii,
        ));
    }
    Ok((rs.to_str()?.to_string(), enc))
}

fn glob_impl(
    patterns: Vec<(String, crate::value::Encoding)>,
    flags: i64,
    base: Option<String>,
    sort: bool,
) -> Result<Vec<RStringInner>> {
    let noescape = flags & FNM_NOESCAPE != 0;
    let mut all_matches = vec![];
    for (pattern_str, enc) in &patterns {
        // Brace alternations expand first, in source order, and each
        // expansion contributes its own (individually sorted) result
        // group — duplicates across groups are kept (CRuby:
        // `Dir.glob("{a,a}")` lists `a` twice).
        for pat in super::fnmatch::expand_braces(pattern_str, noescape) {
            let mut matches = vec![];
            process_glob_pattern(&pat, base.as_deref(), flags, &mut matches)?;
            if sort {
                matches.sort();
            }
            // Matches inherit the pattern's encoding (glob_spec.rb
            // "preserves the encoding of the path").
            all_matches.extend(
                matches
                    .into_iter()
                    .map(|m| RStringInner::from_encoding(m.as_bytes(), *enc)),
            );
        }
    }
    Ok(all_matches)
}

/// Parse one (brace-free) glob pattern string and append matches.
fn process_glob_pattern(
    pattern_str: &str,
    base: Option<&str>,
    flags: i64,
    matches: &mut Vec<String>,
) -> Result<()> {
    if pattern_str.is_empty() {
        // Empty pattern matches nothing (CRuby behavior).
        return Ok(());
    }

    let mut segments: Vec<&str> = pattern_str.split('/').collect();
    let absolute = segments.len() > 1 && segments[0].is_empty();
    if absolute {
        segments.remove(0);
    }
    // Trailing separators: `a/` matches only directories and the
    // separators are preserved in the output (`a//` → `"a//"`).
    let mut trailing = 0usize;
    while segments.last() == Some(&"") {
        segments.pop();
        trailing += 1;
    }
    if absolute && segments.is_empty() {
        // The pattern was just "/" (or "//"…).
        if std::path::Path::new("/").exists() {
            matches.push("/".to_string());
        }
        return Ok(());
    }

    let root = if absolute {
        PathPair::new(PathBuf::from("/"), "/".to_string())
    } else if let Some(base) = base {
        let mut p = std::env::current_dir().unwrap();
        p.push(base);
        match p.canonicalize() {
            Ok(p) => PathPair::new(p, String::new()),
            Err(_) => return Ok(()),
        }
    } else {
        PathPair::new(std::env::current_dir().unwrap(), String::new())
    };

    let mut components: Vec<PathComponent> = vec![];
    for seg in &segments {
        match *seg {
            "." => components.push(PathComponent::Current),
            ".." => components.push(PathComponent::Parent),
            // Interior empty segment: an extra `/` in the output —
            // except right after `**`, which swallows it
            // (`a/**//b` prints single-slashed).
            "" => {
                if components.last() != Some(&PathComponent::Globstar) {
                    components.push(PathComponent::ExtraSlash);
                }
            }
            "**" => components.push(PathComponent::Globstar),
            s => components.push(PathComponent::Name(s.to_string())),
        }
    }
    // A trailing `**` with no `/` after it behaves like `*` — just this
    // level (CRuby: `Dir.glob("**") == Dir.glob("*")`).
    if trailing == 0 && components.last() == Some(&PathComponent::Globstar) {
        components.pop();
        components.push(PathComponent::Name("*".to_string()));
    }
    // `**//` prints a single trailing slash.
    if trailing > 1 && components.last() == Some(&PathComponent::Globstar) {
        trailing = 1;
    }

    traverse_dir(
        root,
        components,
        trailing,
        base.is_some(),
        Reached::default(),
        matches,
        seg_flags(flags),
    )
}

/// How the current traversal position was reached — drives CRuby's
/// rules for synthesizing the "." entry (probed on 4.0.2).
#[derive(Debug, Clone, Copy, Default)]
struct Reached {
    /// A wildcard component has matched an entry, or a `**` has
    /// descended a level: the prefix is no longer a concrete
    /// (literal) path.
    wildcard: bool,
    /// A `**` component was crossed (even at zero levels).
    globstar: bool,
}

/// Whether a segment pattern contains an unescaped metacharacter.
fn has_meta(pat: &str) -> bool {
    let mut chars = pat.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                let _ = chars.next();
            }
            '*' | '?' | '[' => return true,
            _ => {}
        }
    }
    false
}

fn traverse_dir(
    mut path: PathPair,
    mut glob_rest: Vec<PathComponent>,
    trailing: usize,
    base_given: bool,
    reached: Reached,
    matches: &mut Vec<String>,
    sf: u32,
) -> Result<()> {
    let dotmatch = sf & super::fnmatch::FNM_DOTMATCH != 0;
    loop {
        if glob_rest.is_empty() {
            if trailing > 0 {
                // Only directories match a trailing `/`, and the
                // separators show up in the output.
                if path.path.is_dir() {
                    if path.full.is_empty() {
                        // `**/` matching zero levels: with `base:` CRuby
                        // reports the base directory itself as "/";
                        // without it, nothing.
                        if base_given {
                            matches.push("/".to_string());
                        }
                    } else if path.full == "/" {
                        matches.push("/".to_string());
                    } else {
                        matches.push(format!("{}{}", path.full, "/".repeat(trailing)));
                    }
                }
            } else {
                matches.push(path.full.clone());
            }
            return Ok(());
        }
        match glob_rest.remove(0) {
            PathComponent::Parent => {
                path.parent();
            }
            PathComponent::Current => {
                path.current();
            }
            PathComponent::ExtraSlash => {
                path.extra_slash();
            }

            // `**/` — match zero or more directory levels.
            PathComponent::Globstar => {
                // Zero levels: apply remaining components here.
                traverse_dir(
                    path.clone(),
                    glob_rest.clone(),
                    trailing,
                    base_given,
                    Reached {
                        globstar: true,
                        ..reached
                    },
                    matches,
                    sf,
                )?;

                // One or more levels: descend into each subdirectory and
                // keep the `**`. Dot-directories are skipped without
                // FNM_DOTMATCH; symlinked directories are not followed
                // (`file_type` does not traverse the link), like CRuby.
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
                    traverse_dir(
                        new_path,
                        new_glob,
                        trailing,
                        base_given,
                        Reached {
                            wildcard: true,
                            globstar: true,
                        },
                        matches,
                        sf,
                    )?;
                }
                return Ok(());
            }

            // Literal name or single-level glob pattern for one segment.
            PathComponent::Name(pat) => {
                let entries = match std::fs::read_dir(&path.path) {
                    Ok(e) => e,
                    Err(_) => return Ok(()),
                };
                // The leading-period guard lives in the matcher: `*`
                // skips dot-entries unless the pattern itself starts
                // with `.` or FNM_DOTMATCH is set.
                let mut names: Vec<String> = entries
                    .flatten()
                    .filter_map(|e| {
                        let name = e.file_name().to_string_lossy().to_string();
                        super::fnmatch::match_segment(&pat, &name, sf).then_some(name)
                    })
                    .collect();
                // Synthesize "." (read_dir omits it; ".." is never
                // reported, CRuby 3.1+). CRuby's rules, probed on 4.0.2:
                // "." appears only under a concrete (all-literal) prefix
                // — `Dir.glob("nested/*", DOTMATCH)` has "nested/." but
                // `Dir.glob("nest*/*", DOTMATCH)` does not — and, for
                // dot-leading patterns without FNM_DOTMATCH, only when
                // no `**` was crossed (`nested/**/.*` omits "nested/.",
                // `nested/.*` includes it).
                let starts_dot = pat.starts_with('.');
                let allow_dot =
                    !reached.wildcard && (dotmatch || (starts_dot && !reached.globstar));
                if allow_dot && super::fnmatch::match_segment(&pat, ".", sf) {
                    names.push(".".to_string());
                }
                names.sort();
                let pat_meta = has_meta(&pat);
                for name in names {
                    let mut new_path = path.clone();
                    new_path.push(&name);
                    traverse_dir(
                        new_path,
                        glob_rest.clone(),
                        trailing,
                        base_given,
                        Reached {
                            wildcard: reached.wildcard || pat_meta,
                            ..reached
                        },
                        matches,
                        sf,
                    )?;
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
fn home(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // With a username argument, return that user's home directory. An unknown
    // user raises ArgumentError (core/dir/home_spec.rb), matching CRuby.
    if let Some(arg) = lfp.try_arg(0)
        && !arg.is_nil()
    {
        let user = arg
            .coerce_to_path_rstring(vm, globals)?
            .to_str()?
            .to_string();
        let c_user = std::ffi::CString::new(user.as_bytes())
            .map_err(|_| MonorubyErr::argumenterr("user name cannot contain NUL"))?;
        // SAFETY: `getpwnam` reads the passwd DB for the NUL-terminated name
        // and returns a pointer into a static buffer (or null when unknown);
        // we only read `pw_dir` immediately, before any other libc call.
        let dir = unsafe {
            let pw = libc::getpwnam(c_user.as_ptr());
            if pw.is_null() {
                return Err(MonorubyErr::argumenterr(format!(
                    "user {} doesn't exist",
                    user
                )));
            }
            std::ffi::CStr::from_ptr((*pw).pw_dir).to_bytes().to_vec()
        };
        let enc = super::file::filesystem_encoding(globals);
        return Ok(super::file::path_value(&dir, enc));
    }
    let home = match dirs::home_dir() {
        Some(home) => home,
        None => return Ok(Value::nil()),
    };
    // CRuby's `copy_home_path` associates the raw bytes with the
    // filesystem encoding — a plain associate, as `File.readlink` does.
    let enc = super::file::filesystem_encoding(globals);
    Ok(super::file::path_value(
        super::file::pathbuf_bytes(&home),
        enc,
    ))
}

///
/// ### Dir.getwd
///
/// - getwd -> String
/// - pwd -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/getwd.html]
#[monoruby_builtin]
fn pwd(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    let cwd = std::env::current_dir().unwrap();
    let bytes = super::file::pathbuf_bytes(&cwd);
    // CRuby's `rb_dir_getwd` switches on the filesystem encoding: a
    // US-ASCII one becomes ASCII-8BIT *unconditionally* — whether or
    // not the path is all-ASCII, unlike the directory-entry rule — and
    // every other one is associated as is. The association is plain, so
    // bytes that are invalid under a UTF-8 filesystem encoding stay
    // UTF-8 and invalid rather than falling back to BINARY
    // (core/dir/pwd_spec.rb).
    let enc = match super::file::filesystem_encoding(globals) {
        crate::value::Encoding::UsAscii => crate::value::Encoding::Ascii8,
        e => e,
    };
    Ok(super::file::path_value(bytes, enc))
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
    let (path, path_val) = if let Some(path) = lfp.try_arg(0) {
        let rs = path.coerce_to_path_rstring(vm, globals)?;
        super::file::check_path_encoding(globals, &rs)?;
        let enc = rs.encoding();
        (
            super::file::bytes_to_pathbuf(rs.as_bytes()),
            super::file::path_value(rs.as_bytes(), enc),
        )
    } else {
        let home = dirs::home_dir().unwrap();
        let v = super::file::path_value(
            super::file::pathbuf_bytes(&home),
            crate::value::Encoding::Utf8,
        );
        (home, v)
    };
    let display = path.to_string_lossy().to_string();
    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let old_pwd = std::env::current_dir().unwrap();
        match std::env::set_current_dir(&path) {
            Ok(_) => {}
            Err(err) => {
                // The block form fails inside `dir_chdir0`, where the
                // argument form has already reported `chdir_path`.
                return Err(MonorubyErr::errno_with_path(
                    &globals.store,
                    &err,
                    "dir_chdir0",
                    &display,
                ));
            }
        }
        let res = vm.invoke_block(globals, &data, &[path_val]);
        // Restoring the original directory can itself fail (it may have
        // been removed inside the block); CRuby surfaces that Errno
        // (core/dir/chdir_spec.rb "raises an Errno::ENOENT if the
        // original directory no longer exists").
        match std::env::set_current_dir(&old_pwd) {
            Ok(_) => res,
            Err(err) => {
                res?;
                Err(MonorubyErr::errno_with_path(
                    &globals.store,
                    &err,
                    "dir_chdir0",
                    &old_pwd,
                ))
            }
        }
    } else {
        std::env::set_current_dir(&path)
            .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "chdir_path", &display))?;
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
    let rs = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    super::file::check_path_encoding(globals, &rs)?;
    let path = super::file::bytes_to_pathbuf(rs.as_bytes());
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
    let path = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    super::file::check_path_encoding(globals, &path)?;
    let enc_obj = entries_enc_obj(globals, lfp.try_arg(1));
    let mut names: Vec<Vec<u8>> = vec![b".".to_vec(), b"..".to_vec()];
    names.extend(read_dir_names(globals, &path)?);
    let result: Vec<Value> = names
        .into_iter()
        .map(|n| external_str_with_enc(globals, n, enc_obj))
        .collect();
    Ok(Value::array_from_vec(result))
}

///
/// ### Dir.__open_fd (internal)
///
/// Open `path` with `O_RDONLY|O_DIRECTORY|O_CLOEXEC` and return the fd.
/// Backs `Dir#initialize` in builtins/dir.rb.
#[monoruby_builtin]
fn dir_open_fd(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    super::file::check_path_encoding(globals, &path)?;
    if path.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr("path name contains null byte"));
    }
    let display = String::from_utf8_lossy(path.as_bytes()).to_string();
    let c = std::ffi::CString::new(path.as_bytes().to_vec()).unwrap();
    // SAFETY: open(2) with a NUL-terminated path; the fd's ownership moves
    // to the Ruby-side Dir object (closed via Dir.__close_fd).
    let fd = unsafe {
        libc::open(
            c.as_ptr(),
            libc::O_DIRECTORY | libc::O_RDONLY | libc::O_CLOEXEC,
        )
    };
    if fd < 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "dir_initialize",
            &display,
        ));
    }
    Ok(Value::integer(fd as i64))
}

///
/// ### Dir.__close_fd (internal)
///
/// close(2) a directory fd, surfacing failures the way CRuby's
/// `closedir` does (`Errno::EBADF: Bad file descriptor - closedir`).
#[monoruby_builtin]
fn dir_close_fd(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let fd = lfp.arg(0).coerce_to_int_i64(vm, globals)? as i32;
    // SAFETY: close(2); an invalid fd is reported via errno, not UB.
    let rc = unsafe { libc::close(fd) };
    if rc != 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(
            &globals.store,
            &err,
            "closedir",
        ));
    }
    Ok(Value::nil())
}

///
/// ### Dir.__entries_fd (internal)
///
/// Entry names (including "." and "..") of the directory open at `fd`,
/// tagged/transcoded per the optional `encoding` argument.
#[monoruby_builtin]
fn dir_entries_fd(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let fd = lfp.arg(0).coerce_to_int_i64(vm, globals)? as i32;
    let enc_obj = entries_enc_obj(globals, Some(lfp.arg(1)));
    let entries = read_entries_via_fd(globals, fd, enc_obj)?;
    Ok(Value::array_from_vec(entries))
}

/// Read the `@path` ivar set by Ruby-side `Dir#initialize`.
fn dir_path_ivar(globals: &Globals, self_: Value) -> Result<String> {
    match globals.store.get_ivar(self_, IdentId::get_id("@path")) {
        Some(v) if !v.is_nil() => Ok(v.to_s(&globals.store)),
        _ => Err(MonorubyErr::ioerr("uninitialized Dir")),
    }
}

//fn dir_check_closed(globals: &Globals, self_: Value) -> Result<()> {
//    let v = globals
//        .store
//        .get_ivar(self_, IdentId::get_id("@closed"));
//    if v.map(|v| v.as_bool()).unwrap_or(false) {
//        Err(MonorubyErr::ioerr("closed directory"))
//    } else {
//        Ok(())
//    }
//}

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
    std::env::set_current_dir(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_dir_s_chdir", &path))?;
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
        // CRuby tags the SystemCallError message with the syscall name, e.g.
        // "Bad file descriptor - fchdir" (core/dir/fchdir_spec.rb).
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "fchdir"));
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
    fn dir_entries_encoding_keyword() {
        // entries/foreach/children accept `encoding:` and tag names with
        // it (default: external encoding); default_internal transcodes.
        run_test_once(
            r##"(d="/tmp/mono_de_#{Process.pid}"; Dir.mkdir(d); File.write("#{d}/a", ""); a=Dir.entries(d, encoding: "euc-jp").map { |e| e.encoding.name }.uniq; b=Dir.children(d, encoding: Encoding::ISO_8859_1).map { |e| e.encoding.name }.uniq; c=Dir.foreach(d, encoding: "iso-8859-1").to_a.map { |e| e.encoding.name }.uniq; names=[]; Dir.foreach(d, encoding: Encoding::ISO_8859_1) { |e| names << e.encoding.name }; e2=Dir.entries(d).map { |x| x.encoding.name }.uniq; File.unlink("#{d}/a"); Dir.rmdir(d); [a,b,c,names.uniq,e2])"##,
        );
    }

    #[test]
    fn dir_entries_us_ascii_binary_fallback() {
        // CRuby's `rb_external_str_with_enc`: a name that is not
        // all-ASCII under a US-ASCII target encoding comes back
        // ASCII-8BIT, not US-ASCII-with-invalid-bytes. Spelled with an
        // explicit `encoding:` so the answer does not ride on the
        // machine's locale.
        run_test_once(
            r##"(d="/tmp/mono_eu_#{Process.pid}"; Dir.mkdir(d); n="ã".dup.force_encoding("binary"); File.write("#{d}/#{n}", ""); File.write("#{d}/a", ""); f=->(enc){ Dir.children(d, encoding: enc).sort_by(&:bytes).map { |s| [s.bytes, s.encoding.name] } }; r=[f.call("US-ASCII"), f.call("UTF-8"), f.call("BINARY"), Dir.entries(d, encoding: "US-ASCII").sort_by(&:bytes).map { |s| s.encoding.name }]; Dir.children(d).each { |c| File.unlink("#{d}/#{c}") }; Dir.rmdir(d); r)"##,
        );
    }

    #[test]
    fn dir_glob_encoding_and_flags_keyword() {
        // Matches inherit the pattern's encoding; the flags: keyword is
        // accepted and preferred over the positional argument.
        run_test_once(
            r##"(d="/tmp/mono_ge_#{Process.pid}"; Dir.mkdir(d); File.write("#{d}/.dot", ""); File.write("#{d}/plain", ""); a=Dir.glob("*", base: d).sort; b=Dir.glob("*", flags: File::FNM_DOTMATCH, base: d).sort; c=Dir.glob("*", :ignored, flags: File::FNM_DOTMATCH, base: d).sort; e2=Dir.glob("pl*".encode(Encoding::EUC_JP), base: d).map { |x| x.encoding.name }; f=Dir["pl*", base: d]; File.unlink("#{d}/.dot"); File.unlink("#{d}/plain"); Dir.rmdir(d); [a,b,c,e2,f])"##,
        );
    }

    #[test]
    fn dir_for_fd_shares_descriptor() {
        // Dir.for_fd shares the fd (no dup): closing the original makes
        // the wrapper's close(2) fail with CRuby's closedir EBADF; the
        // wrapper lists the same entries and has a nil path.
        run_test_once(
            r##"(d="/tmp/mono_ff_#{Process.pid}"; Dir.mkdir(d); File.write("#{d}/x", ""); dir=Dir.open(d); a=dir.fileno.is_a?(Integer); dn=Dir.for_fd(dir.fileno); b=dn.to_a.sort; c=dn.path; dir.close; e2=(begin; dn.close; rescue => e; [e.class, e.message]; end); f=(begin; Dir.for_fd("x"); rescue => e; e.class; end); File.unlink("#{d}/x"); Dir.rmdir(d); [a,b,c,e2,f])"##,
        );
    }

    #[test]
    fn dir_pwd_filesystem_encoding() {
        // `rb_dir_getwd`: the cwd carries the filesystem encoding, with
        // a US-ASCII one turned into ASCII-8BIT; the association is
        // plain, so bytes that are invalid in a UTF-8 filesystem
        // encoding come back UTF-8 and invalid, not BINARY. Phrased
        // against `Encoding.find("filesystem")` so the answer is the
        // same whatever locale the test host runs under.
        //
        // The second half needs a directory whose name is *not* valid
        // UTF-8, which APFS refuses outright (`EILSEQ @ dir_s_mkdir`),
        // so it is skipped where the mkdir fails — both runtimes take
        // the same branch on the same host. That makes the result
        // platform-dependent, hence a live CRuby rather than the oracle.
        run_test_once_live(
            r##"(d="/tmp/mono_pwde_#{Process.pid}"; Dir.mkdir(d); fs=Encoding.find("filesystem"); want=(fs == Encoding::US_ASCII ? Encoding::BINARY : fs); r=[Dir.pwd.encoding == want, Dir.getwd.encoding == want]; orig=Encoding.default_external; Encoding.default_external=Encoding::US_ASCII; r << Dir.pwd.encoding.name; Encoding.default_external=Encoding::UTF_8; r << Dir.pwd.encoding.name; Encoding.default_external=orig; sub="#{d}/" + "\xFF\xFE".dup.force_encoding("binary"); made=(begin; Dir.mkdir(sub); true; rescue SystemCallError; false; end); if made; r << Dir.chdir(sub) { [Dir.pwd.encoding == want, Dir.pwd.valid_encoding? == (want == Encoding::BINARY)] }; Dir.rmdir(sub); else; r << :no_invalid_utf8_names; end; Dir.rmdir(d); r)"##,
        );
    }

    #[test]
    fn dir_pwd_binary_names() {
        // mkdir/chdir/pwd round-trip raw non-ASCII bytes; pwd tags the
        // result UTF-8 when it decodes.
        // /tmp resolves through /private on macOS — live CRuby.
        run_test_once_live(
            r##"(base="/tmp/mono_pwd_#{Process.pid}"; Dir.mkdir(base); name="#{base}/あ".dup.force_encoding(Encoding::BINARY); Dir.mkdir(name); r=Dir.chdir(name) { [Dir.pwd.encoding.name, Dir.pwd.force_encoding("binary") == name] }; Dir.rmdir(name); Dir.rmdir(base); r)"##,
        );
    }

    #[test]
    fn dir_chdir_restore_failure() {
        // Dir.chdir's block form surfaces the Errno when the original
        // directory vanished inside the block.
        run_test_once(
            r##"(d1="/tmp/mono_cr1_#{Process.pid}"; d2="/tmp/mono_cr2_#{Process.pid}"; Dir.mkdir(d1); Dir.mkdir(d2); r=(begin; Dir.chdir(d1) { Dir.chdir(d2) { Dir.unlink(d1) } }; rescue => e; e.class; end); Dir.rmdir(d2); r)"##,
        );
    }

    #[test]
    fn dir_methods_coverage() {
        // Dir.home(user) via getpwnam (+ ArgumentError for an unknown user),
        // Dir.foreach's no-block Enumerator (size == nil), Dir.fchdir's tagged
        // SystemCallError message, and the trailing `**` glob == `*` behaviour.
        // Dir.home("root") is OS-dependent (/root on Linux, /var/root on
        // macOS): verify against a live CRuby, not the oracle.
        run_test_once_live(
            r##"(a=Dir.home("root"); c=Dir.foreach("/").is_a?(Enumerator); d=Dir.foreach("/").size; e2=(begin; Dir.fchdir(-1); rescue => x; [x.class, x.message]; end); f=(Dir.glob("**").sort==Dir.glob("*").sort); [a,c,d,e2,f])"##,
        );
        // The unknown-user ArgumentError is CRuby's own, OS-independent —
        // oracle-backed, so a live ruby (whose miss lookup can hang for
        // 45 s behind WSL2's nss-systemd) is spawned at most once.
        run_test_once(
            r##"(begin; Dir.home("no_such_user_zzq"); rescue => e; e.class; end)"##,
        );
    }

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
    fn glob_argument_validation() {
        run_tests(&[
            // `sort:` must be exactly true or false.
            r#"Dir.glob("*", sort: 0) rescue [$!.class, $!.message]"#,
            r#"Dir.glob("*", sort: nil) rescue [$!.class, $!.message]"#,
            r#"Dir.glob("*", sort: "false") rescue [$!.class, $!.message]"#,
            r#"Dir.glob("*", sort: true).class"#,
            r#"Dir.glob("*", sort: false).class"#,
            // NUL byte in a pattern is rejected.
            r#"Dir.glob("a\0b") rescue [$!.class, $!.message]"#,
            r#"Dir["a\0b"] rescue [$!.class, $!.message]"#,
            r#"Dir.glob(["ok*", "a\0b"]) rescue [$!.class, $!.message]"#,
        ]);
    }

    #[test]
    fn home() {
        // Host-dependent value: verify against a live CRuby, not the oracle.
        run_test_live(r#"Dir.home"#);
    }

    #[test]
    fn home_filesystem_encoding() {
        // `copy_home_path` associates the home directory's raw bytes
        // with the filesystem encoding — plainly, with no ASCII-8BIT
        // fallback, unlike `Dir.pwd`. Phrased against
        // `Encoding.find("filesystem")` so the answer does not depend
        // on the test host's locale.
        run_test_once(
            r##"(fs=Encoding.find("filesystem"); [Dir.home.encoding == fs, Dir.home("root").encoding == fs, File.expand_path("~").encoding == fs, File.expand_path("~/foo").encoding == fs, File.expand_path("foo").encoding == Encoding::UTF_8])"##,
        );
    }

    #[test]
    fn pwd() {
        // Host-dependent values (absolute cwd paths): verify against a live
        // CRuby, not the oracle.
        run_test_live(r#"Dir.pwd"#);
        run_test_live(r#"Dir.getwd"#);
        run_test_live(
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
    fn dir_each_child_encoding_keyword() {
        // `Dir.each_child` forwards `encoding:` to the names it yields —
        // both the block form and the Enumerator the no-block form
        // returns (the sibling entries/children/foreach already did).
        run_test_once(
            r##"(d="/tmp/mono_ec_#{Process.pid}"; Dir.mkdir(d); File.write("#{d}/a", ""); names=[]; Dir.each_child(d, encoding: "euc-jp") { |e| names << e.encoding.name }; a=Dir.each_child(d, encoding: "utf-8").to_a.map { |e| e.encoding.name }; b=Dir.each_child(d, encoding: Encoding::ISO_8859_1).map { |e| e.encoding.name }; File.unlink("#{d}/a"); Dir.rmdir(d); [names.uniq, a.uniq, b.uniq])"##,
        );
    }

    #[test]
    fn dir_instance_children_take_no_arguments() {
        // The instance forms take no arguments at all: the encoding was
        // fixed when the Dir was opened, so a keyword is an ArgumentError.
        run_test_once(
            r##"(d="/tmp/mono_ic_#{Process.pid}"; Dir.mkdir(d); File.write("#{d}/a", ""); f=->(&blk){ begin; blk.call; rescue => e; [e.class, e.message]; end }; a=f.call { Dir.open(d).children(encoding: "utf-8") }; b=f.call { Dir.open(d).each_child(encoding: "utf-8") { |x| x } }; File.unlink("#{d}/a"); Dir.rmdir(d); [a, b])"##,
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
        // Dir.pwd inside /tmp differs on macOS (/private/tmp) — live CRuby.
        run_test_once_live(
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
        // Dir.pwd inside /tmp differs on macOS (/private/tmp) — live CRuby.
        run_test_once_live(
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
        run_test_error(r#"Dir.foreach("/no_such_dir_xyz_qq_foreach") { |_| }"#);
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
