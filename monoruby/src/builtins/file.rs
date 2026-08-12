use super::*;
use std::{
    fs::File,
    io::{Seek, SeekFrom},
};

//
// File class
//

pub(super) fn init(globals: &mut Globals) {
    let io_class = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("IO"))
        .unwrap()
        .as_class();
    let file = globals
        .define_builtin_class("File", FILE_CLASS, io_class, OBJECT_CLASS, ObjTy::IO)
        .id();
    let file_test = globals.define_toplevel_module("FileTest").id();
    // CRuby: `File.write(name, string, offset=nil, **opts)` where
    // opts ⊃ {mode:, perm:, encoding:, …}. monoruby honours only
    // arity here — offset is currently ignored (always truncating
    // from 0) and the keywords are accepted for compatibility but
    // not enforced. Fixing that is left to follow-up PRs; the
    // arity opening alone closes a 25-strong cluster in
    // `core/kernel` (mspec's `before :each` writes a fixture file
    // with `perm: 0o700`).
    // File.write / File.binwrite are inherited from IO (implemented in
    // Ruby, builtins/io.rb) — CRuby defines them on IO too.
    globals.define_builtin_class_func_with(file, "read", file_read, 1, 4, false);
    globals.define_builtin_class_func_with(file, "binread", file_binread, 1, 3, false);

    // IO class methods that share semantics with File.* class methods.
    // (IO.write / IO.binwrite live in builtins/io.rb.)
    globals.define_builtin_class_func_with(IO_CLASS, "binread", file_binread, 1, 3, false);
    globals.define_builtin_class_func(IO_CLASS, "try_convert", io_try_convert, 1);
    globals.define_builtin_class_func_rest(file, "join", file_join);
    globals.define_builtin_class_func_with(file, "expand_path", file_expand_path, 1, 2, false);
    globals.define_builtin_class_func_with(file, "dirname", file_dirname, 1, 2, false);
    globals.define_builtin_class_func_with(file, "basename", file_basename, 1, 2, false);
    globals.define_builtin_class_func(file, "extname", file_extname, 1);
    globals.define_builtin_class_func(file, "path", file_path, 1);
    globals.define_builtin_class_func_with(file, "realpath", realpath, 1, 2, false);
    globals.define_builtin_class_func_with_kw(file, "open", open, 1, 3, false, OPEN_KW, true);
    globals.define_builtin_class_func_with_kw(file, "new", file_new, 1, 3, false, OPEN_KW, true);
    globals.define_builtin_class_func_with_kw(IO_CLASS, "open", open, 1, 3, false, OPEN_KW, true);

    globals.define_builtin_class_func(file, "directory?", directory_, 1);
    globals.define_builtin_module_func(file_test, "directory?", directory_, 1);

    globals.define_builtin_class_func(file, "symlink?", symlink_, 1);
    globals.define_builtin_module_func(file_test, "symlink?", symlink_, 1);

    globals.define_builtin_class_func(file, "exist?", exist, 1);
    globals.define_builtin_module_func(file_test, "exist?", exist, 1);

    globals.define_builtin_class_func(file, "file?", file_, 1);
    globals.define_builtin_module_func(file_test, "file?", file_, 1);

    globals.define_builtin_class_func(file, "executable?", executable_, 1);
    globals.define_builtin_module_func(file_test, "executable?", executable_, 1);

    globals.define_builtin_class_func(file, "readable?", readable_, 1);
    globals.define_builtin_module_func(file_test, "readable?", readable_, 1);

    globals.define_builtin_class_func(file, "writable?", writable_, 1);
    globals.define_builtin_module_func(file_test, "writable?", writable_, 1);

    // `*_real?` test with the real uid/gid. In monoruby's typical
    // single-user runtime real == effective, so they delegate to the
    // effective-uid predicates (matches CRuby's result here).
    globals.define_builtin_class_func(file, "executable_real?", executable_, 1);
    globals.define_builtin_module_func(file_test, "executable_real?", executable_, 1);
    globals.define_builtin_class_func(file, "readable_real?", readable_, 1);
    globals.define_builtin_module_func(file_test, "readable_real?", readable_, 1);
    globals.define_builtin_class_func(file, "writable_real?", writable_, 1);
    globals.define_builtin_module_func(file_test, "writable_real?", writable_, 1);

    globals.define_builtin_func_rest(file, "write", write);
    globals.define_builtin_func(file, "size", size, 0);
    globals.define_builtin_func(file, "truncate", file_truncate_instance, 1);
    globals.define_builtin_func(file, "flock", flock_, 1);

    globals.define_builtin_class_func_with(file, "umask", umask, 0, 1, false);
    globals.define_builtin_class_funcs_with(file, "fnmatch", &["fnmatch?"], fnmatch, 2, 3, false);
    globals.define_builtin_class_func_with(file, "absolute_path", absolute_path, 1, 2, false);
    globals.define_builtin_class_func(file, "absolute_path?", absolute_path_, 1);
    globals.define_builtin_class_func(file, "split", file_split, 1);
    globals.define_builtin_class_funcs_rest(file, "delete", &["unlink"], delete);
    globals.define_builtin_class_func_rest(file, "chmod", chmod);
    globals.define_builtin_class_func(file, "symlink", file_symlink, 2);
    globals.define_builtin_class_func_with_kw(
        file,
        "readlines",
        readlines,
        1,
        3,
        false,
        &["chomp"],
        true,
    );

    globals.define_builtin_class_func(file, "size", file_size, 1);
    globals.define_builtin_module_func(file_test, "size", file_size, 1);

    globals.define_builtin_class_func(file, "size?", file_size_, 1);
    globals.define_builtin_module_func(file_test, "size?", file_size_, 1);

    globals.define_builtin_class_func(file, "ftype", ftype, 1);

    globals.define_builtin_class_func(file, "owned?", owned_, 1);
    globals.define_builtin_module_func(file_test, "owned?", owned_, 1);

    globals.define_builtin_class_func(file, "grpowned?", grpowned_, 1);
    globals.define_builtin_module_func(file_test, "grpowned?", grpowned_, 1);

    globals.define_builtin_class_func(file, "setuid?", setuid_, 1);
    globals.define_builtin_module_func(file_test, "setuid?", setuid_, 1);

    globals.define_builtin_class_func(file, "setgid?", setgid_, 1);
    globals.define_builtin_module_func(file_test, "setgid?", setgid_, 1);

    globals.define_builtin_class_func(file, "sticky?", sticky_, 1);
    globals.define_builtin_module_func(file_test, "sticky?", sticky_, 1);

    globals.define_builtin_class_func(file, "world_readable?", world_readable_, 1);
    globals.define_builtin_module_func(file_test, "world_readable?", world_readable_, 1);

    globals.define_builtin_class_func(file, "world_writable?", world_writable_, 1);
    globals.define_builtin_module_func(file_test, "world_writable?", world_writable_, 1);

    globals.define_builtin_class_func(file, "socket?", socket_, 1);
    globals.define_builtin_module_func(file_test, "socket?", socket_, 1);

    globals.define_builtin_class_func(file, "chardev?", chardev_, 1);
    globals.define_builtin_module_func(file_test, "chardev?", chardev_, 1);

    globals.define_builtin_class_func(file, "blockdev?", blockdev_, 1);
    globals.define_builtin_module_func(file_test, "blockdev?", blockdev_, 1);

    globals.define_builtin_class_func(file, "pipe?", pipe_, 1);
    globals.define_builtin_module_func(file_test, "pipe?", pipe_, 1);

    globals.define_builtin_class_func(file, "readlink", file_readlink, 1);
    globals.define_builtin_class_func(file, "link", file_link, 2);
    globals.define_builtin_class_func(file, "rename", file_rename, 2);
    globals.define_builtin_class_func(file, "truncate", file_truncate, 2);
    globals.define_builtin_class_func_with(file, "realdirpath", file_realdirpath, 1, 2, false);

    globals.define_builtin_class_func(file, "identical?", identical_, 2);
    globals.define_builtin_module_func(file_test, "identical?", identical_, 2);

    globals.define_builtin_class_func_rest(file, "utime", utime);
    globals.define_builtin_class_func_rest(file, "lutime", lutime);
    globals.define_builtin_class_func_rest(file, "chown", file_chown);
    globals.define_builtin_class_func_rest(file, "lchown", file_lchown);
    globals.define_builtin_class_func_with(file, "mkfifo", file_mkfifo, 1, 2, false);

    globals.define_builtin_class_func(file, "atime", file_atime, 1);
    globals.define_builtin_class_func(file, "mtime", file_mtime, 1);
    globals.define_builtin_class_func(file, "ctime", file_ctime, 1);
    globals.define_builtin_class_func(file, "birthtime", file_birthtime, 1);

    // File::Stat — the class body (accessors / predicates) lives in
    // `builtins/file_stat.rb`; here we own construction. `File.stat`,
    // `File.lstat`, and `File::Stat.new` all populate the fields from
    // a real stat(2) / lstat(2), raising the matching Errno on
    // failure. The instance ivars (@dev, @ino, …) are read by the
    // Ruby accessors.
    let object_class = globals.store.object_class();
    let stat = globals.define_class("Stat", object_class, file).id();
    globals.define_builtin_class_func(file, "stat", file_stat, 1);
    globals.define_builtin_class_func(file, "lstat", file_lstat, 1);
    globals.define_builtin_func(stat, "initialize", stat_initialize, 1);
    globals.define_builtin_func(IO_CLASS, "stat", file_instance_stat, 0);
    globals.define_builtin_func(file, "stat", file_instance_stat, 0);
    globals.define_builtin_func(file, "lstat", file_instance_lstat, 0);

    globals.define_builtin_singleton_func(
        globals.get_load_path(),
        "resolve_feature_path",
        resolve_feature_path,
        1,
    );
}


///
/// ### IO.read
///
/// - read(path, [NOT SUPPORTED]**opt) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/read.html]
#[monoruby_builtin]
fn file_read(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let filename_str = filename.to_string_lossy();
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &filename,
            ));
        }
    };
    // Optional length / offset, plus a trailing options Hash
    // (`File.read(path, length, offset)` /
    //  `File.read(path, encoding: "...", mode: "...")`).
    let mut positional: Vec<Value> = vec![];
    let mut opts_enc: Option<Value> = None;
    for i in 1..=3 {
        let Some(v) = lfp.try_arg(i) else { break };
        if v.is_nil() {
            positional.push(v);
        } else if let Some(h) = v.try_hash_ty() {
            opts_enc = h.get(Value::symbol(IdentId::get_id("encoding")), vm, globals)?;
        } else {
            positional.push(v);
        }
    }
    let length = match positional.first() {
        Some(v) if !v.is_nil() => {
            let l = v.coerce_to_int_i64(vm, globals)?;
            if l < 0 {
                return Err(MonorubyErr::argumenterr(format!("negative length {l} given")));
            }
            Some(l as usize)
        }
        _ => None,
    };
    let offset = match positional.get(1) {
        Some(v) if !v.is_nil() => v.coerce_to_int_i64(vm, globals)?.max(0) as u64,
        _ => 0,
    };
    if offset > 0 {
        std::io::Seek::seek(&mut file, std::io::SeekFrom::Start(offset)).map_err(|err| {
            MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_read", &filename)
        })?;
    }
    let mut contents = vec![];
    let res = match length {
        Some(l) => std::io::Read::read_to_end(&mut std::io::Read::take(file, l as u64), &mut contents),
        None => std::io::Read::read_to_end(&mut file, &mut contents),
    };
    if let Err(err) = res {
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "rb_io_read",
            &filename,
        ));
    }
    // A sized read that hits EOF immediately reads as nil; sized reads
    // come back binary, like `IO#read(len)`. An `encoding:` option tags
    // the result explicitly.
    let res = match length {
        Some(l) if l > 0 && contents.is_empty() => return Ok(Value::nil()),
        Some(_) => Value::bytes(contents),
        None => Value::string_from_vec(contents),
    };
    if let Some(enc_v) = opts_enc {
        let name = if let Some(s) = enc_v.is_str() {
            Some(s.to_string())
        } else {
            super::encoding::encoding_object_name(globals, enc_v)
        };
        if let Some(name) = name
            && let Ok(enc) = crate::value::Encoding::try_from_str(&name)
        {
            let bytes = res.as_rstring_inner().as_bytes().to_vec();
            return Ok(Value::string_from_inner(RStringInner::from_encoding(
                &bytes, enc,
            )));
        }
    }
    Ok(res)
}

///
/// ### IO.binread
///
/// - binread(path, length = nil, offset = 0) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/binread.html]
#[monoruby_builtin]
fn file_binread(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let length = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        let n = arg1.coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            return Err(MonorubyErr::argumenterr(format!("negative length {}", n)));
        }
        Some(n)
    } else {
        None
    };
    let offset = if let Some(arg2) = lfp.try_arg(2)
        && !arg2.is_nil()
    {
        let n = arg2.coerce_to_int_i64(vm, globals)?;
        if n < 0 {
            // CRuby: the lseek(2) failure surfaces as Errno::EINVAL.
            let err = std::io::Error::from_raw_os_error(libc::EINVAL);
            return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "seek"));
        }
        Some(n)
    } else {
        None
    };
    let filename_str = filename.to_string_lossy().to_string();
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &filename,
            ));
        }
    };
    if let Some(offset) = offset {
        match file.seek(SeekFrom::Start(offset as _)) {
            Ok(_) => {}
            Err(err) => {
                return Err(MonorubyErr::errno_with_path(
                    &globals.store,
                    &err,
                    "rb_io_seek",
                    &filename_str,
                ));
            }
        };
    }
    if let Some(length) = length {
        let mut contents = vec![0; length as usize];
        if let Err(err) = std::io::Read::read_exact(&mut file, &mut contents) {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_io_read",
                &filename_str,
            ));
        };
        Ok(Value::bytes(contents))
    } else {
        let mut contents = vec![];
        if let Err(err) = std::io::Read::read_to_end(&mut file, &mut contents) {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_io_read",
                &filename_str,
            ));
        };
        Ok(Value::bytes(contents))
    }
}


///
/// ### IO.try_convert
/// - try_convert(obj) -> IO | nil
///
/// Returns `obj` if it is already an IO. Otherwise calls `obj.to_io` if
/// defined and returns the result, or `nil` if the conversion is not
/// supported.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/try_convert.html]
#[monoruby_builtin]
fn io_try_convert(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let v = lfp.arg(0);
    if let Some(rv) = v.try_rvalue()
        && rv.ty() == ObjTy::IO
    {
        return Ok(v);
    }
    let respond_to = IdentId::get_id("respond_to?");
    let to_io = IdentId::get_id("to_io");
    let responds = match vm.invoke_method_inner(
        globals,
        respond_to,
        v,
        &[Value::symbol(to_io)],
        None,
        None,
    ) {
        Ok(val) => val.as_bool(),
        Err(_) => return Ok(Value::nil()),
    };
    if !responds {
        return Ok(Value::nil());
    }
    let result = vm.invoke_method_inner(globals, to_io, v, &[], None, None)?;
    if let Some(rv) = result.try_rvalue()
        && rv.ty() == ObjTy::IO
    {
        return Ok(result);
    }
    if result.is_nil() {
        return Ok(Value::nil());
    }
    Err(MonorubyErr::typeerr(format!(
        "can't convert {} to IO ({}#to_io gives {})",
        globals.get_class_name(v.class()),
        globals.get_class_name(v.class()),
        globals.get_class_name(result.class()),
    )))
}

///
/// ### File.join
///
/// - join(*item) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/join.html]
#[monoruby_builtin]
fn file_join(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Collect the leaf path components in order (flattening nested arrays).
    fn flatten(
        vm: &mut Executor,
        globals: &mut Globals,
        parts: &mut Vec<(Vec<u8>, crate::value::Encoding)>,
        val: Value,
        seen: &mut Vec<u64>,
    ) -> Result<()> {
        match val.try_array_ty() {
            Some(ainfo) => {
                // An empty array argument joins as a single empty component,
                // so `File.join([], [])` == "/" (core/file/join_spec.rb).
                if ainfo.len() == 0 {
                    parts.push((Vec::new(), crate::value::Encoding::Utf8));
                    return Ok(());
                }
                let id = val.id();
                if seen.contains(&id) {
                    return Err(MonorubyErr::argumenterr("recursive array"));
                }
                seen.push(id);
                for v in ainfo.iter().cloned() {
                    flatten(vm, globals, parts, v, seen)?;
                }
                seen.pop();
            }
            None => {
                let s = val.coerce_to_path_rstring_allow_nul(vm, globals)?;
                if s.as_bytes().contains(&0) {
                    return Err(MonorubyErr::argumenterr("string contains null byte"));
                }
                parts.push((s.as_bytes().to_vec(), s.encoding()));
            }
        }
        Ok(())
    }
    let mut parts = vec![];
    let mut seen = vec![];
    for v in lfp.arg(0).as_array().iter().cloned() {
        flatten(vm, globals, &mut parts, v, &mut seen)?;
    }
    // Join adjacent components with a single separator. When both sides of a
    // junction already carry a separator, CRuby keeps the right part's leading
    // separators and drops the left part's trailing ones ("usr//" + "/bin" ->
    // "usr/bin", "usr/" + "//bin" -> "usr//bin"); when exactly one side has one
    // it is reused; when neither does a "/" is inserted.
    let mut path: Vec<u8> = Vec::new();
    // The joined result carries the first non-UTF-8 component's encoding
    // (join_spec.rb "preserves the encoding of the path").
    let mut enc = crate::value::Encoding::Utf8;
    for (i, (part, part_enc)) in parts.iter().enumerate() {
        if enc == crate::value::Encoding::Utf8 && *part_enc != crate::value::Encoding::Utf8 {
            enc = *part_enc;
        }
        if i == 0 {
            path.extend_from_slice(part);
            continue;
        }
        let left_sep = path.last() == Some(&b'/');
        let right_sep = part.first() == Some(&b'/');
        if left_sep && right_sep {
            while path.last() == Some(&b'/') {
                path.pop();
            }
        } else if !left_sep && !right_sep {
            path.push(b'/');
        }
        path.extend_from_slice(part);
    }
    Ok(path_value(&path, enc))
}

///
/// ### File.expand_path
/// - expand_path(path, default_dir = '.') -> String
/// TODO: support ~USER
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/expand_path.html]
#[monoruby_builtin]
fn file_expand_path(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    // CRuby converts the result to the filesystem encoding; with an
    // ASCII-incompatible default_external this is impossible up front.
    let de = super::io::enc_default_external_obj(globals);
    if let Some(e) = super::io::enc_obj_to_enum(globals, de)
        && !e.is_ascii_compatible()
    {
        return Err(MonorubyErr::encoding_compatibility_error_with_store(
            &globals.store,
            format!("incompatible character encodings: UTF-8 and {}", e.name()),
        ));
    }
    let rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = rs.encoding();
    let dfl: Option<Vec<u8>> = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        Some(to_path_rstring(vm, globals, arg1)?.as_bytes().to_vec())
    } else {
        None
    };
    let res = expand_path_bytes(rs.as_bytes(), dfl.as_deref())?;
    Ok(path_value(&res, enc))
}

/// Expand `path` into an absolute byte path against `dfl` (both may
/// start with `~`/`~user`), mirroring CRuby's `rb_file_expand_path`:
/// `.`/`..` are collapsed lexically, interior slash runs are squeezed,
/// and the leading slash run of the absolute prefix is preserved
/// verbatim (`////some/path` stays as written).
fn expand_path_bytes(path: &[u8], dfl: Option<&[u8]>) -> Result<Vec<u8>> {
    if path.first() == Some(&b'~') {
        let (home, rest) = if path.len() == 1 || path[1] == b'/' {
            (expand_home_dir()?, &path[1..])
        } else {
            let end = path.iter().position(|&b| b == b'/').unwrap_or(path.len());
            (user_home_dir(&path[1..end])?, &path[end..])
        };
        if home.first() != Some(&b'/') {
            return Err(MonorubyErr::argumenterr("non-absolute home"));
        }
        let mut joined = home;
        joined.extend_from_slice(rest);
        Ok(normalize_abs_bytes(&joined))
    } else if path.first() == Some(&b'/') {
        Ok(normalize_abs_bytes(path))
    } else {
        let mut base = match dfl {
            Some(d) => expand_path_bytes(d, None)?,
            None => cwd_bytes()?,
        };
        if base.last() != Some(&b'/') {
            base.push(b'/');
        }
        base.extend_from_slice(path);
        Ok(normalize_abs_bytes(&base))
    }
}

/// Current working directory as raw bytes.
fn cwd_bytes() -> Result<Vec<u8>> {
    match std::env::current_dir() {
        Ok(dir) => Ok(pathbuf_bytes(&dir).to_vec()),
        Err(err) => Err(MonorubyErr::runtimeerr(format!(
            "failed to get current directory: {err}"
        ))),
    }
}

/// `$HOME` (must be set and non-empty; falls back to the passwd entry
/// when unset — CRuby raises "non-absolute home" for a set-but-empty or
/// relative HOME later, in the caller's absolute check).
fn expand_home_dir() -> Result<Vec<u8>> {
    use std::os::unix::ffi::OsStrExt;
    match std::env::var_os("HOME") {
        Some(h) if !h.is_empty() => Ok(h.as_bytes().to_vec()),
        Some(_) => Err(MonorubyErr::argumenterr("non-absolute home")),
        None => {
            // SAFETY: getpwuid returns a pointer to a static passwd entry
            // (or null); pw_dir is read immediately before any other libc
            // call could clobber the buffer.
            unsafe {
                let pw = libc::getpwuid(libc::getuid());
                if pw.is_null() {
                    return Err(MonorubyErr::argumenterr("couldn't find HOME environment -- expanding `~'"));
                }
                Ok(std::ffi::CStr::from_ptr((*pw).pw_dir).to_bytes().to_vec())
            }
        }
    }
}

/// Home directory of the named user (for `~user` expansion).
fn user_home_dir(user: &[u8]) -> Result<Vec<u8>> {
    let display = String::from_utf8_lossy(user).to_string();
    let c_user = std::ffi::CString::new(user)
        .map_err(|_| MonorubyErr::argumenterr("user name contains null byte"))?;
    // SAFETY: getpwnam reads the passwd DB for the NUL-terminated name and
    // returns a pointer into a static buffer (or null when unknown); pw_dir
    // is copied out immediately.
    unsafe {
        let pw = libc::getpwnam(c_user.as_ptr());
        if pw.is_null() {
            return Err(MonorubyErr::argumenterr(format!(
                "user {display} doesn't exist"
            )));
        }
        Ok(std::ffi::CStr::from_ptr((*pw).pw_dir).to_bytes().to_vec())
    }
}

/// Lexically normalize an absolute byte path: squeeze interior slash
/// runs, drop `.`, collapse `..` (never above root), and keep the
/// leading run of slashes exactly as written.
fn normalize_abs_bytes(bytes: &[u8]) -> Vec<u8> {
    let lead = bytes.iter().take_while(|&&b| b == b'/').count().max(1);
    let mut comps: Vec<&[u8]> = vec![];
    for c in bytes[lead.min(bytes.len())..].split(|&b| b == b'/') {
        match c {
            b"" | b"." => {}
            b".." => {
                comps.pop();
            }
            c => comps.push(c),
        }
    }
    let mut out = vec![b'/'; lead];
    for (i, c) in comps.iter().enumerate() {
        if i > 0 {
            out.push(b'/');
        }
        out.extend_from_slice(c);
    }
    out
}

///
/// ### File.dirname
/// - dirname(filename, [NOT SUPPRTED]level=1) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/dirname.html]
#[monoruby_builtin]
fn file_dirname(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let mut dirname = match filename.parent() {
        Some(ostr) => conv_pathbuf(ostr),
        None => "".to_string(),
    };
    if dirname.is_empty() {
        dirname += "."
    };
    Ok(Value::string(dirname))
}

///
/// ### File.basename
/// - basename(filename, suffix = "") -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/basename.html]
#[monoruby_builtin]
fn file_basename(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = filename.encoding();
    let suffix: Option<Vec<u8>> = if let Some(arg1) = lfp.try_arg(1) {
        let s = arg1.coerce_to_rstring(vm, globals)?;
        if s.is_empty() {
            None
        } else {
            Some(s.as_bytes().to_vec())
        }
    } else {
        None
    };
    if filename.is_empty() {
        return Ok(path_value(b"", enc));
    }
    let basename: &[u8] = filename
        .as_bytes()
        .split(|&b| b == b'/')
        .rev()
        .find(|s| !s.is_empty())
        .unwrap_or(b"/");
    if let Some(suffix) = suffix {
        if suffix == b".*" {
            // CRuby treats the ".*" suffix specially: strip the last
            // extension (a '.' that is not the leading char, so
            // dotfiles like ".bashrc" are preserved).
            if let Some(pos) = basename.iter().rposition(|&b| b == b'.') {
                if pos > 0 {
                    return Ok(path_value(&basename[..pos], enc));
                }
            }
        } else if basename.ends_with(&suffix) {
            return Ok(path_value(&basename[..basename.len() - suffix.len()], enc));
        }
    }
    Ok(path_value(basename, enc))
}

///
/// ### File.directory?
/// - directory?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/directory=3f.html]
#[monoruby_builtin]
fn directory_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // CRuby's `rb_stat` accepts either a path (coerced via `#to_path`/`#to_str`)
    // or an IO/fd (converted via `#to_io` and `fstat`'d). Try the path form
    // first — this covers String and IO objects that expose `#to_path` — and
    // fall back to `#to_io` on a coercion failure before surfacing the
    // TypeError. `Path::is_dir` stats the path and returns false for a
    // non-existent entry, so no canonicalization (which would swallow the
    // TypeError for a non-path type such as Integer/nil) is needed.
    let arg = lfp.arg(0);
    match to_path(vm, globals, arg) {
        Ok(path) => Ok(Value::bool(path.is_dir())),
        Err(e) => {
            let to_io = IdentId::get_id("to_io");
            if globals.check_method(arg, to_io).is_some() {
                let io = vm.invoke_method_inner(globals, to_io, arg, &[], None, None)?;
                if let Some(rv) = io.try_rvalue()
                    && rv.ty() == ObjTy::IO
                {
                    let fd = io.as_io_inner().fileno()?;
                    return Ok(Value::bool(fd_is_dir(fd)));
                }
            }
            Err(e)
        }
    }
}

/// `fstat(2)` an open file descriptor and report whether it refers to a
/// directory. A failed `fstat` (bad fd, etc.) reports `false`.
fn fd_is_dir(fd: i32) -> bool {
    // SAFETY: `fstat` fills a zeroed `stat` buffer for an open fd; we only
    // read `st_mode`. On failure the buffer is unused and we return false.
    unsafe {
        let mut st: libc::stat = std::mem::zeroed();
        if libc::fstat(fd, &mut st) == 0 {
            (st.st_mode & libc::S_IFMT) == libc::S_IFDIR
        } else {
            false
        }
    }
}

///
/// ### File.symlink?
/// - symlink?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/symlink=3f.html]
#[monoruby_builtin]
fn symlink_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = to_path(vm, globals, lfp.arg(0))?;
    Ok(Value::bool(path.is_symlink()))
}

///
/// ### File.extname
/// - extname(filename) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/extname.html]
#[monoruby_builtin]
fn file_extname(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = filename.encoding();
    // Work on the final path component; an extension is the part after the
    // last dot, but a basename that is nothing but dots ("...", "..") or
    // starts with its only dot (".profile") has no extension.
    let base: &[u8] = filename
        .as_bytes()
        .rsplit(|&b| b == b'/')
        .next()
        .unwrap_or(b"");
    let extname: &[u8] = match base.iter().rposition(|&b| b == b'.') {
        // No extension when nothing but dots precedes the last dot:
        // ".profile" / ".." / "...a" → "".
        Some(pos) if !base[..pos].is_empty() && !base[..pos].iter().all(|&b| b == b'.') => {
            if pos + 1 < base.len() {
                &base[pos..]
            } else {
                // Trailing dot: "foo." → "." on non-Windows CRuby.
                b"."
            }
        }
        _ => b"",
    };
    Ok(path_value(extname, enc))
}

///
/// ### File.exist?
/// - exist?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/exist=3f.html]
#[monoruby_builtin]
fn exist(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Validate the argument type first (raises TypeError for non-string)
    let path = to_path(vm, globals, lfp.arg(0))?;
    let b = path.canonicalize().is_ok();
    Ok(Value::bool(b))
}

///
/// ### File.file?
/// - file?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/file=3f.html]
#[monoruby_builtin]
fn file_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // Validate the argument type first (raises TypeError for non-string)
    let path = to_path(vm, globals, lfp.arg(0))?;
    match path.canonicalize() {
        Ok(path) => Ok(Value::bool(path.is_file())),
        Err(_) => Ok(Value::bool(false)),
    }
}

///
/// ### File.executable?
/// - executable?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/executable=3f.html]
#[monoruby_builtin]
fn executable_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use std::os::unix::fs::PermissionsExt;
    let path = to_path(vm, globals, lfp.arg(0))?;
    let b = match std::fs::metadata(&path) {
        Ok(meta) => meta.permissions().mode() & 0o111 != 0,
        Err(_) => false,
    };
    Ok(Value::bool(b))
}

fn access_path(vm: &mut Executor, globals: &mut Globals, val: Value, mode: i32) -> Result<bool> {
    use std::os::unix::ffi::OsStrExt;
    let path = to_path(vm, globals, val)?;
    let c = match std::ffi::CString::new(path.as_os_str().as_bytes()) {
        Ok(c) => c,
        Err(_) => return Ok(false),
    };
    Ok(unsafe { libc::access(c.as_ptr(), mode) } == 0)
}

///
/// ### File.readable?
/// - readable?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/readable=3f.html]
#[monoruby_builtin]
fn readable_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(access_path(vm, globals, lfp.arg(0), libc::R_OK)?))
}

///
/// ### File.writable?
/// - writable?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/writable=3f.html]
#[monoruby_builtin]
fn writable_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(access_path(vm, globals, lfp.arg(0), libc::W_OK)?))
}

///
/// ### File#flock
/// - flock(operation) -> 0 | false
///
/// Wraps `flock(2)`. Returns `0` on success. With `LOCK_NB` set, returns
/// `false` instead of blocking when the lock would not be granted.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/i/flock.html]
#[monoruby_builtin]
fn flock_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let op = lfp.arg(0).coerce_to_i64(&globals.store)? as i32;
    let self_ = lfp.self_val();
    let fd = self_.as_io_inner().fileno()?;
    let (r, errno) = if (op & libc::LOCK_NB) != 0 || (op & libc::LOCK_UN) != 0 {
        // Non-blocking request / unlock: never blocks, run inline.
        // SAFETY: plain flock(2) on the IO's fd.
        let r = unsafe { libc::flock(fd, op) };
        let errno = if r == 0 {
            0
        } else {
            std::io::Error::last_os_error().raw_os_error().unwrap_or(0)
        };
        (r as i64, errno)
    } else {
        // A blocking flock waits *in the kernel* with nothing to poll:
        // inline it would freeze every green thread (and the process).
        // Run it on a native worker and park this thread instead.
        let comp = crate::native_pool::run_blocking(
            vm,
            globals,
            crate::native_pool::NativeOp::Flock { fd, op },
        )?;
        (comp.ret, comp.errno)
    };
    if r == 0 {
        return Ok(Value::integer(0));
    }
    // Non-blocking lock that would have blocked: return false (Ruby spec).
    if (op & libc::LOCK_NB) != 0 && (errno == libc::EWOULDBLOCK || errno == libc::EAGAIN) {
        return Ok(Value::bool(false));
    }
    Err(MonorubyErr::ioerr(format!(
        "flock failed: {}",
        std::io::Error::from_raw_os_error(errno)
    )))
}

///
/// ### File.path
/// - path(filename) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/path.html]
#[monoruby_builtin]
fn file_path(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    Ok(path_value(rs.as_bytes(), rs.encoding()))
}

///
/// ### File.realpath
/// - realpath(pathname, basedir = nil) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/realpath.html]
#[monoruby_builtin]
fn realpath(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = rs.encoding();
    let base: Option<Vec<u8>> = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        Some(to_path_rstring(vm, globals, arg1)?.as_bytes().to_vec())
    } else {
        None
    };
    let resolved = check_realpath(globals, rs.as_bytes(), base.as_deref(), true)?;
    Ok(resolved_path_value(globals, resolved, enc, true))
}

/// Tag a resolved (filesystem / UTF-8) path with the path argument's
/// encoding: converted when possible; when the conversion fails,
/// `File.realpath` *forces* the argument's encoding onto the raw bytes
/// while `File.realdirpath` keeps the resolved UTF-8 tag (CRuby's
/// realpath specs distinguish the two).
fn resolved_path_value(
    globals: &Globals,
    bytes: Vec<u8>,
    enc: crate::value::Encoding,
    force: bool,
) -> Value {
    use crate::value::Encoding as E;
    if enc == E::Utf8 {
        return path_value(&bytes, E::Utf8);
    }
    let topts = super::encoding::TranscodeOpts::default();
    match super::encoding::transcode_bytes_with_opts(&bytes, E::Utf8, enc, &topts, &globals.store) {
        Ok(b) => path_value(&b, enc),
        Err(_) if force => path_value(&bytes, enc),
        Err(_) => path_value(&bytes, E::Utf8),
    }
}

/// Resolve `path` (absolutized against `base` / the cwd) component-wise
/// the way CRuby's `rb_check_realpath` does: each component is
/// `lstat`ed, symlinks are followed (raising `ELOOP` after too many
/// hops), and `..` collapses the resolved prefix *lexically* — so
/// `dir/file/../` resolves to `dir` without an `ENOTDIR`. With
/// `strict_last` false (File.realdirpath) the final component may be
/// absent; everything else must exist.
///
/// Error spelling follows CRuby: strict-mode ENOENT/ELOOP report the
/// path *as given* (base-joined) under `rb_check_realpath_internal`
/// (CRuby's native realpath(3) path); every other failure reports the
/// resolved-so-far prefix under `realpath_rec` (the emulation).
fn check_realpath(
    globals: &Globals,
    path: &[u8],
    base: Option<&[u8]>,
    strict_last: bool,
) -> Result<Vec<u8>> {
    use std::collections::VecDeque;
    fn push_front_components(queue: &mut VecDeque<Vec<u8>>, bytes: &[u8]) {
        for c in bytes.split(|&b| b == b'/').rev() {
            if !c.is_empty() {
                queue.push_front(c.to_vec());
            }
        }
    }
    // The user-facing path for strict-mode native-style errors: the
    // argument as written, prefixed by the base when one was given and
    // the argument is relative.
    let given_display = {
        let arg = String::from_utf8_lossy(path).to_string();
        match base {
            Some(b) if path.first() != Some(&b'/') => {
                format!("{}/{}", String::from_utf8_lossy(b), arg)
            }
            _ => arg,
        }
    };
    let raise = |raw: i32, resolved: &[u8]| {
        let err = std::io::Error::from_raw_os_error(raw);
        if strict_last && matches!(raw, libc::ENOENT | libc::ELOOP) {
            MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_check_realpath_internal",
                &given_display,
            )
        } else {
            MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "realpath_rec",
                {
                    use std::os::unix::ffi::OsStrExt;
                    std::ffi::OsStr::from_bytes(resolved)
                },
            )
        }
    };
    if path.is_empty() {
        return Err(raise(libc::ENOENT, b""));
    }
    let mut queue: VecDeque<Vec<u8>> = VecDeque::new();
    push_front_components(&mut queue, path);
    if path.first() != Some(&b'/') {
        let abs_base: Vec<u8> = match base {
            Some(b) if b.first() == Some(&b'/') => b.to_vec(),
            Some(b) => {
                let mut cur = cwd_bytes()?;
                cur.push(b'/');
                cur.extend_from_slice(b);
                cur
            }
            None => cwd_bytes()?,
        };
        push_front_components(&mut queue, &abs_base);
    }

    let mut resolved: Vec<u8> = Vec::new();
    let mut links = 0usize;
    while let Some(comp) = queue.pop_front() {
        if comp == b"." {
            continue;
        }
        if comp == b".." {
            while let Some(b) = resolved.pop() {
                if b == b'/' {
                    break;
                }
            }
            continue;
        }
        let prev_len = resolved.len();
        resolved.push(b'/');
        resolved.extend_from_slice(&comp);
        match std::fs::symlink_metadata(bytes_to_pathbuf(&resolved)) {
            Ok(md) if md.file_type().is_symlink() => {
                links += 1;
                if links > 40 {
                    return Err(raise(libc::ELOOP, &resolved));
                }
                let target = std::fs::read_link(bytes_to_pathbuf(&resolved))
                    .map_err(|e| raise(e.raw_os_error().unwrap_or(libc::EIO), &resolved))?;
                let tb = pathbuf_bytes(&target).to_vec();
                resolved.truncate(prev_len);
                if tb.first() == Some(&b'/') {
                    resolved.clear();
                }
                push_front_components(&mut queue, &tb);
            }
            Ok(_) => {}
            Err(e) => {
                let missing_last_ok = !strict_last
                    && queue.is_empty()
                    && e.kind() == std::io::ErrorKind::NotFound;
                if !missing_last_ok {
                    return Err(raise(e.raw_os_error().unwrap_or(libc::EIO), &resolved));
                }
            }
        }
    }
    if resolved.is_empty() {
        resolved.push(b'/');
    }
    Ok(resolved)
}

///
/// ### File.open
///
/// - open(path, mode = "r", [NOT SUPPORTED] perm = 0666) -> File
/// - open(path, mode = "r", [NOT SUPPORTED] perm = 0666) {|file| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/new.html]
/// Translate a CRuby-style flags integer (e.g. `File::WRONLY | File::CREAT`)
/// into the mode string monoruby's open path understands: access mode in
/// the low 2 bits (RDONLY=0/WRONLY=1/RDWR=2) plus the platform's
/// `O_CREAT`/`O_TRUNC`/`O_APPEND` bits — `File::*` mirrors the libc-backed
/// `IO::*` constants, and Linux and Darwin disagree on every one of these.
/// Other flags (EXCL, NONBLOCK, …) pass through silently — `open` handles
/// their effect via mode-string semantics.
pub(super) fn mode_string_from_flags(flags: i64) -> String {
    const O_CREAT: i64 = libc::O_CREAT as i64;
    const O_TRUNC: i64 = libc::O_TRUNC as i64;
    const O_APPEND: i64 = libc::O_APPEND as i64;
    let access = flags & 0b11;
    let create = flags & O_CREAT != 0;
    let trunc = flags & O_TRUNC != 0;
    let append = flags & O_APPEND != 0;
    match access {
        // RDONLY
        0 => "r".to_string(),
        // WRONLY. `WRONLY|CREAT` *without* `TRUNC` has no public mode
        // string; the internal spellings "w-" (write+create) / "-w"
        // (write only) keep it lossless — `IO.write` with an offset
        // depends on the file NOT being truncated.
        1 => {
            if append {
                "a".to_string()
            } else if trunc {
                "w".to_string()
            } else if create {
                "w-".to_string()
            } else {
                "-w".to_string()
            }
        }
        // RDWR
        _ => {
            if append {
                "a+".to_string()
            } else if trunc || create {
                "w+".to_string()
            } else {
                "r+".to_string()
            }
        }
    }
}

/// Close `io` at `File.open` block exit through `#close` *dispatch* (a
/// subclass/singleton override must run), swallowing the IOError that
/// means "already closed" and propagating everything else. A block
/// error, when present, wins over a close error (close still runs).
pub(super) fn block_close(
    vm: &mut Executor,
    globals: &mut Globals,
    io: Value,
    block_result: Result<Value>,
) -> Result<Value> {
    let close_result =
        vm.invoke_method_inner(globals, IdentId::get_id("close"), io, &[], None, None);
    match (block_result, close_result) {
        (Err(e), _) => Err(e),
        (Ok(v), Ok(_)) => Ok(v),
        (Ok(v), Err(ce)) => {
            if ce.message().contains("closed stream") {
                Ok(v)
            } else {
                Err(ce)
            }
        }
    }
}

/// Keyword parameters accepted by `File.open` / `File.new` / `IO.open`.
/// Their slots start right after the 3 positional parameters (path,
/// mode, perm); the kw_rest Hash (transcode options and other extras
/// monoruby accepts but does not implement) sits after them.
pub(super) const OPEN_KW: &[&str] = &[
    "flags",
    "mode",
    "perm",
    "encoding",
    "external_encoding",
    "internal_encoding",
    "textmode",
    "binmode",
    "autoclose",
    "path",
    "newline",
    "invalid",
    "undef",
    "replace",
    "fallback",
    "xml",
];

/// A named `OPEN_KW` keyword's value (absent or nil → `None`).
fn open_kw(lfp: Lfp, name: &str) -> Option<Value> {
    let i = OPEN_KW.iter().position(|n| *n == name)?;
    lfp.try_arg(3 + i).filter(|v| !v.is_nil())
}

/// Collect the named keyword slots (+ any kw_rest extras) back into an
/// options Hash so the shared IO option readers (`io_open_opts`,
/// `init_io_encodings`) see keyword and positional-Hash call forms
/// uniformly.
fn open_kw_hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Option<Value>> {
    let mut map = RubyMap::default();
    let mut any = false;
    for (i, name) in OPEN_KW.iter().enumerate() {
        if let Some(v) = lfp.try_arg(3 + i) {
            map.insert(Value::symbol(IdentId::get_id(name)), v, vm, globals)?;
            any = true;
        }
    }
    if let Some(rest) = lfp.try_arg(3 + OPEN_KW.len())
        && rest.try_hash_ty().is_some()
    {
        for (k, v) in rest.as_hash().iter() {
            map.insert(k, v, vm, globals)?;
            any = true;
        }
    }
    Ok(if any { Some(Value::hash(map)) } else { None })
}

/// Translate a mode string ("r", "wb+", "wx", …; a ":enc" suffix is
/// ignored here) into open(2) flags + the binmode marker. Unknown or
/// misplaced letters raise CRuby's "invalid access mode" ArgumentError
/// (the 'x' creation guard is only valid with 'w').
pub(super) fn oflags_from_mode_string(mode: &str) -> Result<(i64, bool)> {
    let base = mode.split(':').next().unwrap_or("");
    let invalid = || MonorubyErr::argumenterr(format!("invalid access mode {base}"));
    let mut it = base.chars();
    let first = it.next();
    let mut oflags: i64 = match first {
        Some('r') => libc::O_RDONLY as i64,
        Some('w') => (libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC) as i64,
        Some('a') => (libc::O_WRONLY | libc::O_CREAT | libc::O_APPEND) as i64,
        _ => return Err(invalid()),
    };
    let mut binmode = false;
    for c in it {
        match c {
            'b' => binmode = true,
            't' => {}
            '+' => {
                oflags = (oflags & !(libc::O_ACCMODE as i64)) | libc::O_RDWR as i64;
            }
            'x' => {
                if first != Some('w') {
                    return Err(invalid());
                }
                oflags |= libc::O_EXCL as i64;
            }
            _ => return Err(invalid()),
        }
    }
    Ok((oflags, binmode))
}

#[monoruby_builtin]
fn open(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    open_impl(vm, globals, lfp, false)
}

/// `File.new` — same as `File.open` except a given block is *not*
/// called (CRuby warns and returns the File).
#[monoruby_builtin]
fn file_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    open_impl(vm, globals, lfp, true)
}

fn open_impl(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    is_new: bool,
) -> Result<Value> {
    let kw_hash = open_kw_hash(vm, globals, lfp)?;
    // If the first argument is an Integer, treat it as a file descriptor.
    if let Some(fd) = lfp.arg(0).try_fixnum() {
        let fd_i32 = fd as i32;
        // Validate the file descriptor before using it.
        // Use fcntl(fd, F_GETFD) to check if the fd is valid.
        if fd_i32 < 0 || unsafe { libc::fcntl(fd_i32, libc::F_GETFD) } == -1 {
            let err = std::io::Error::from_raw_os_error(9); // EBADF
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &format!("fd {}", fd),
            ));
        }
        // An integer mode cannot change an already-open fd's access mode;
        // CRuby surfaces the mismatch as Errno::EINVAL
        // (core/file/new_spec.rb "can't alter mode or permissions when
        // opening a file").
        if let Some(n) = lfp.try_arg(1).and_then(|v| v.try_fixnum()) {
            let want = (n as i32) & libc::O_ACCMODE;
            // SAFETY: fcntl(F_GETFL) on the fd validated above.
            let cur = unsafe { libc::fcntl(fd_i32, libc::F_GETFL) };
            if cur != -1 && (cur & libc::O_ACCMODE) != want {
                let err = std::io::Error::from_raw_os_error(libc::EINVAL);
                return Err(MonorubyErr::errno_plain(&globals.store, &err));
            }
        }
        // Scan trailing args for an options Hash and pick up `:path`
        // (display name) and `:autoclose` (fd ownership) — required by
        // patterns like `File.new(io.fileno, autoclose: false, path: "")`
        // (logger/log_device.rb feature-detection code) where the caller
        // explicitly disclaims ownership of the borrowed fd.
        let (name, has_path, autoclose) =
            super::io::io_open_opts(vm, globals, lfp, 1..3, fd, kw_hash)?;
        let (readable, writable) = super::io::fd_rw_mode(fd_i32);
        // If another monoruby IO already owns this fd, borrow it (autoclose
        // = false) instead of creating a second closing `OwnedFd`, which
        // would double-close and trip Rust's IO-safety abort. See the
        // matching guard in `io_new` and `OWNED_FDS` in value/rvalue/io.rs.
        let effective_autoclose =
            autoclose && !crate::value::rvalue::fd_is_owned(fd_i32);
        // SAFETY: fd has been validated as a valid file descriptor above.
        let io_inner = IoInner::from_raw_fd_autoclose(
            fd_i32,
            name,
            has_path,
            readable,
            writable,
            effective_autoclose,
        );
        let res = Value::new_io_with_class(io_inner, FILE_CLASS);
        // The fresh File is referenced only by this Rust local while
        // `init_io_encodings` / `ruby_warn` re-enter Ruby — root it.
        return vm.with_temp_scope(|vm| {
            vm.temp_push(res);
            let mode_for_enc = lfp
                .try_arg(1)
                .and_then(|a| a.is_str().map(|s| s.to_string()))
                .unwrap_or_else(|| {
                    match (readable, writable) {
                        (true, true) => "r+",
                        (false, true) => "w",
                        _ => "r",
                    }
                    .to_string()
                });
            super::io::init_io_encodings(
                vm,
                globals,
                lfp,
                res,
                &mode_for_enc,
                readable,
                1..3,
                kw_hash,
            )?;
            if let Some(bh) = lfp.block() {
                if is_new {
                    vm.ruby_warn(
                        globals,
                        "warning: File::new() does not take block; use File::open() instead",
                    )?;
                    return Ok(res);
                }
                let r = vm.invoke_block_once(globals, bh, &[res]);
                // Match CRuby File.open(...) {|io| ... }: close at block exit.
                // Holding the underlying fd open across blocks defeats `flock`
                // (rubygems' open_with_flock relies on this) and leaks fds.
                return block_close(vm, globals, res, r);
            }
            Ok(res)
        });
    }

    // Resolve the open mode into open(2) flags. Precedence (CRuby):
    // `mode:` keyword > positional mode arg (String / Integer / Hash
    // with `:mode`) > default "r". A `flags:` keyword ORs extra bits on
    // top of either form.
    let mut mode_val: Option<Value> = None;
    if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        if let Some(h) = arg1.try_hash_ty() {
            if let Some(m) = h.get(Value::symbol(IdentId::get_id("mode")), vm, globals)?
                && !m.is_nil()
            {
                mode_val = Some(m);
            }
        } else {
            mode_val = Some(arg1);
        }
    }
    if let Some(m) = open_kw(lfp, "mode") {
        mode_val = Some(m);
    }
    // `mode` (the string form) also drives the encoding suffix parsing
    // in init_io_encodings, so keep a string spelling alongside the
    // flag bits.
    let (mut oflags, mut binmode, mode) = match mode_val {
        None => (libc::O_RDONLY as i64, false, "r".to_string()),
        Some(m) => {
            if let Some(n) = m.try_fixnum() {
                (n, false, mode_string_from_flags(n))
            } else {
                let s = m.coerce_to_string(vm, globals)?;
                let (f, b) = oflags_from_mode_string(&s)?;
                (f, b, s)
            }
        }
    };
    if let Some(f) = open_kw(lfp, "flags") {
        oflags |= f.coerce_to_int_i64(vm, globals)?;
    }
    if open_kw(lfp, "binmode").is_some_and(|v| v.as_bool()) {
        binmode = true;
    }
    // CRuby rejects newline decorators on a binary-mode stream.
    if binmode && open_kw(lfp, "newline").is_some() {
        return Err(MonorubyErr::argumenterr("newline decorator with binary mode"));
    }
    let access = (oflags as i32) & libc::O_ACCMODE;
    let readable = access == libc::O_RDONLY || access == libc::O_RDWR;
    let writable = access == libc::O_WRONLY || access == libc::O_RDWR;
    // Creation permissions: the positional Integer after the mode, or a
    // `perm:` keyword (only applied when the open creates the file).
    let mut perm: i64 = 0o666;
    if let Some(arg2) = lfp.try_arg(2)
        && let Some(n) = arg2.try_fixnum()
    {
        perm = n;
    }
    if let Some(p) = open_kw(lfp, "perm")
        && let Some(n) = p.try_fixnum()
    {
        perm = n;
    }
    let path_rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    let path_bytes = path_rs.as_bytes().to_vec();
    let path = String::from_utf8_lossy(&path_bytes).to_string();
    // NUL bytes were rejected by to_path_rstring, so this cannot fail.
    let cpath = std::ffi::CString::new(path_bytes.clone())
        .map_err(|_| MonorubyErr::argumenterr("path name contains null byte"))?;
    let open_flags = (oflags as i32) | libc::O_CLOEXEC;
    // A FIFO's open(2) blocks in the kernel until the peer end appears;
    // opening it inline would freeze every green thread (and the
    // process). Detect the FIFO up front and run the blocking open on
    // a native worker instead, parking only this thread
    // (doc/threads.md §9). A TOCTOU miss here just falls back to
    // the previous inline behavior.
    let is_fifo = std::fs::metadata(bytes_to_pathbuf(&path_bytes))
        .map(|m| std::os::unix::fs::FileTypeExt::is_fifo(&m.file_type()))
        .unwrap_or(false);
    let file = if is_fifo {
        let comp = crate::native_pool::run_blocking(
            vm,
            globals,
            crate::native_pool::NativeOp::Open {
                path: cpath,
                flags: open_flags,
                mode: perm as u32,
            },
        )?;
        if comp.ret < 0 {
            let err = std::io::Error::from_raw_os_error(comp.errno);
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &path,
            ));
        }
        // SAFETY: `ret` is a fresh fd from the worker's open(2); ownership
        // transfers to the File here.
        unsafe { <File as std::os::fd::FromRawFd>::from_raw_fd(comp.ret as i32) }
    } else {
        // SAFETY: open(2) with a NUL-terminated path; the returned fd (when
        // valid) is owned by the File constructed below.
        let fd = unsafe { libc::open(cpath.as_ptr(), open_flags, perm as libc::c_uint) };
        if fd < 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &path,
            ));
        }
        // SAFETY: `fd` is a fresh descriptor from open(2); ownership
        // transfers to the File here.
        unsafe { <File as std::os::fd::FromRawFd>::from_raw_fd(fd) }
    };
    let res = Value::new_file(
        file,
        path,
        Some((path_bytes.clone(), path_rs.encoding())),
        readable,
        writable,
    );
    // The fresh File is referenced only by this Rust local while
    // `init_io_encodings` / `ruby_warn` re-enter Ruby — root it.
    vm.with_temp_scope(|vm| {
        vm.temp_push(res);
        super::io::init_io_encodings(vm, globals, lfp, res, &mode, readable, 1..3, kw_hash)?;
        if let Some(bh) = lfp.block() {
            if is_new {
                vm.ruby_warn(
                    globals,
                    "warning: File::new() does not take block; use File::open() instead",
                )?;
                return Ok(res);
            }
            let r = vm.invoke_block_once(globals, bh, &[res]);
            // CRuby File.open(...) {|io| ... } closes the file at block exit.
            return block_close(vm, globals, res, r);
        }
        Ok(res)
    })
}

///
/// ### IO#write
/// - write(*str) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/write.html]
#[monoruby_builtin]
fn write(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let data = lfp.arg(0).as_array();
    let mut count = 0i64;
    for s in data.iter() {
        // External-encoding conversion (CRuby's do_writeconv) — shared
        // with IO#write.
        let bytes = super::io::bytes_for_write(vm, globals, lfp.self_val(), *s)?;
        count += bytes.len() as i64;
        let mut done = 0;
        super::io::blocking_io_region(vm, globals, lfp.self_val(), libc::POLLOUT, |_store| {
            lfp.self_val().as_io_inner_mut().write(&bytes, &mut done, _store)
        })?;
    }
    Ok(Value::integer(count))
}

///
/// ### $LOAD_PATH#resolve_feature_path
///
/// - file_name -> [ext: Symbol, path: String]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Array/i/length.html]
#[monoruby_builtin]
fn resolve_feature_path(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let file_name = to_path(vm, globals, lfp.arg(0))?;
    match globals.search_lib(vm, &file_name) {
        Some(path) => {
            let ext = match path.extension().and_then(|s| s.to_str()) {
                Some(ext) => Value::symbol_from_str(ext),
                _ => {
                    return Err(MonorubyErr::runtimeerr(
                        "resolve_feature_path: Failed to get file extension.",
                    ));
                }
            };
            let path_str = Value::string_from_str(&conv_pathbuf(&path));
            let arr = Value::array2(ext, path_str);
            Ok(arr)
        }
        None => Ok(Value::nil()),
    }
}

///
/// ### File.umask
/// - umask -> Integer
/// - umask(mask) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/umask.html]
#[monoruby_builtin]
fn umask(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        let mask = arg0.coerce_to_int_i64(vm, globals)? as u32;
        // SAFETY: umask is a POSIX system call that is safe to call.
        let old = unsafe { libc::umask(mask as libc::mode_t) };
        Ok(Value::integer(old as i64))
    } else {
        // Get current umask by setting and restoring
        // SAFETY: umask is a POSIX system call that is safe to call.
        let current = unsafe { libc::umask(0) };
        unsafe { libc::umask(current) };
        Ok(Value::integer(current as i64))
    }
}

///
/// ### File.fnmatch
/// - fnmatch(pattern, path, flags = 0) -> bool
/// - fnmatch?(pattern, path, flags = 0) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/fnmatch.html]
#[monoruby_builtin]
fn fnmatch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let pattern = lfp.arg(0).coerce_to_string(vm, globals)?;
    let path_str = to_path_str(vm, globals, lfp.arg(1))?;
    let flags = if let Some(arg2) = lfp.try_arg(2) {
        arg2.coerce_to_int_i64(vm, globals)? as u32
    } else {
        0
    };
    let result = super::fnmatch::fnmatch(&pattern, &path_str, flags);
    Ok(Value::bool(result))
}

///
/// ### File.absolute_path
/// - absolute_path(file_name, dir_string = nil) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/absolute_path.html]
#[monoruby_builtin]
fn absolute_path(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = rs.encoding();
    if rs.as_bytes().first() == Some(&b'/') {
        return Ok(path_value(rs.as_bytes(), enc));
    }
    let base = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        bytes_to_pathbuf(to_path_rstring(vm, globals, arg1)?.as_bytes())
    } else {
        match std::env::current_dir() {
            Ok(dir) => dir,
            Err(err) => return Err(MonorubyErr::errno_with_msg(&globals.store, &err, ".")),
        }
    };
    let mut result = base;
    result.push(bytes_to_pathbuf(rs.as_bytes()));
    Ok(path_value(pathbuf_bytes(&result), enc))
}

///
/// ### File.absolute_path?
/// - absolute_path?(file_name) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/absolute_path=3f.html]
#[monoruby_builtin]
fn absolute_path_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let file_name = to_path_str(vm, globals, lfp.arg(0))?;
    Ok(Value::bool(file_name.starts_with('/')))
}

///
/// ### File.split
/// - split(pathname) -> [dirname, basename]
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/split.html]
#[monoruby_builtin]
fn file_split(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = rs.encoding();
    let filename = normalize_pathbuf(rs.as_bytes());
    let dir: Vec<u8> = match filename.parent() {
        Some(p) if !pathbuf_bytes(p).is_empty() => pathbuf_bytes(p).to_vec(),
        _ => b".".to_vec(),
    };
    let base: Vec<u8> = match filename.file_name() {
        Some(ostr) => {
            use std::os::unix::ffi::OsStrExt;
            ostr.as_bytes().to_vec()
        }
        None => {
            if filename.as_os_str() == "/" {
                b"/".to_vec()
            } else {
                Vec::new()
            }
        }
    };
    Ok(Value::array2(path_value(&dir, enc), path_value(&base, enc)))
}

// Utils

/// Convert `file` to PathBuf.
fn to_path(vm: &mut Executor, globals: &mut Globals, file: Value) -> Result<std::path::PathBuf> {
    let file = to_path_rstring(vm, globals, file)?;
    Ok(normalize_pathbuf(file.as_bytes()))
}

/// Lexically normalize raw path bytes into a `PathBuf`, collapsing
/// `name/..` pairs.
fn normalize_pathbuf(bytes: &[u8]) -> std::path::PathBuf {
    let mut path = std::path::PathBuf::new();
    for p in bytes_to_pathbuf(bytes).iter() {
        if p == ".." && path.file_name().is_some() {
            path.pop();
        } else {
            path.push(p);
        };
    }
    path
}

/// Coerce `val` to a path String, keeping the raw bytes and the encoding
/// tag. Mirrors CRuby's `rb_get_path`: NUL bytes raise ArgumentError and
/// an ASCII-incompatible encoding raises `Encoding::CompatibilityError`.
pub(super) fn to_path_rstring(
    vm: &mut Executor,
    globals: &mut Globals,
    val: Value,
) -> Result<RString> {
    // Check the encoding before the NUL-byte scan: a UTF-16/32 path is a
    // CompatibilityError even though its bytes contain NULs.
    let rs = val.coerce_to_path_rstring_allow_nul(vm, globals)?;
    check_path_encoding(globals, &rs)?;
    if rs.as_bytes().contains(&0) {
        return Err(MonorubyErr::argumenterr("path name contains null byte"));
    }
    Ok(rs)
}

/// Reject ASCII-incompatible path encodings (UTF-16/32, ISO-2022-JP)
/// with CRuby's `Encoding::CompatibilityError` message.
pub(super) fn check_path_encoding(globals: &Globals, rs: &RString) -> Result<()> {
    let enc = rs.encoding();
    if !enc.is_ascii_compatible() {
        return Err(MonorubyErr::encoding_compatibility_error_with_store(
            &globals.store,
            format!(
                "path name must be ASCII-compatible ({}): \"{}\"",
                enc.name(),
                rs.inspect().trim_matches('"'),
            ),
        ));
    }
    Ok(())
}

/// Build a `PathBuf` from raw path bytes (no UTF-8 requirement on Unix).
pub(super) fn bytes_to_pathbuf(bytes: &[u8]) -> std::path::PathBuf {
    use std::os::unix::ffi::OsStrExt;
    std::path::PathBuf::from(std::ffi::OsStr::from_bytes(bytes))
}

/// The raw bytes of a `Path`.
pub(super) fn pathbuf_bytes(path: &std::path::Path) -> &[u8] {
    use std::os::unix::ffi::OsStrExt;
    path.as_os_str().as_bytes()
}

/// Build a path result String from raw bytes tagged with `enc` — the
/// encoding of the originating path argument, which CRuby's path
/// operations preserve in their results.
pub(super) fn path_value(bytes: &[u8], enc: crate::value::Encoding) -> Value {
    Value::string_from_inner(RStringInner::from_encoding(bytes, enc))
}

pub(super) fn to_path_str(vm: &mut Executor, globals: &mut Globals, val: Value) -> Result<String> {
    Ok(to_path_rstring(vm, globals, val)?.to_str()?.to_string())
}

#[cfg(not(windows))]
fn conv_pathbuf(dir: &std::path::Path) -> String {
    dir.to_string_lossy().to_string()
}

#[cfg(windows)]
fn conv_pathbuf(dir: &std::path::PathBuf) -> String {
    dir.to_string_lossy()
        .replace("\\\\?\\", "")
        .replace('\\', "/")
}

///
/// ### File.delete / File.unlink
///
/// - delete(*filename) -> Integer
/// - unlink(*filename) -> Integer
///
/// Deletes the named files, returning the number of names passed as arguments.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/delete.html]
#[monoruby_builtin]
fn delete(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let mut count = 0i64;
    for arg in args.iter() {
        let path = to_path_str(vm, globals, *arg)?;
        std::fs::remove_file(&path)
            .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, &path))?;
        count += 1;
    }
    Ok(Value::integer(count))
}

///
/// ### File.chmod
///
/// - chmod(mode, *filename) -> Integer
///
/// Changes permission bits on the named files to the bit pattern represented by `mode`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/chmod.html]
#[monoruby_builtin]
fn chmod(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    let mode = args[0].coerce_to_int_i64(_vm, globals)? as u32;
    let mut count = 0i64;
    for arg in args[1..].iter() {
        let path = to_path_str(_vm, globals, *arg)?;
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(mode)).map_err(|e| {
            MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_chmod", &path)
        })?;
        count += 1;
    }
    Ok(Value::integer(count))
}

///
/// ### File.symlink
///
/// - symlink(old, new) -> 0
///
/// Creates a symbolic link called `new` for the existing file `old`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/symlink.html]
#[monoruby_builtin]
fn file_symlink(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let old = to_path_str(vm, globals, lfp.arg(0))?;
    let new = to_path_str(vm, globals, lfp.arg(1))?;
    std::os::unix::fs::symlink(&old, &new)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_symlink", &new))?;
    Ok(Value::integer(0))
}

///
/// ### File.readlines
///
/// - readlines(path) -> [String]
///
/// Reads the entire file and returns an array of lines.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/readlines.html]
#[monoruby_builtin]
fn readlines(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let content = std::fs::read(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    // Optional separator / limit, `chomp:` keyword — `IO#gets` rules,
    // shifted one slot right of the path argument.
    let mut sep: Option<Vec<u8>> = globals.rs_bytes();
    let mut limit: Option<usize> = None;
    match (lfp.try_arg(1), lfp.try_arg(2)) {
        (None, _) => {}
        (Some(v), None) => {
            if v.is_nil() {
                sep = None;
            } else if let Some(rs) = v.is_rstring() {
                sep = Some(rs.as_bytes().to_vec());
            } else {
                let l = v.coerce_to_int_i64(vm, globals)?;
                limit = (l >= 0).then_some(l as usize);
            }
        }
        (Some(v), Some(l)) => {
            sep = if v.is_nil() {
                None
            } else {
                Some(v.coerce_to_rstring(vm, globals)?.as_bytes().to_vec())
            };
            let l = l.coerce_to_int_i64(vm, globals)?;
            limit = (l >= 0).then_some(l as usize);
        }
    }
    let chomp = lfp.try_arg(3).is_some_and(|v| v.as_bool());
    let lines: Vec<Value> = split_records(&content, sep.as_deref(), limit, chomp)
        .into_iter()
        .map(Value::string_from_vec)
        .collect();
    Ok(Value::array_from_vec(lines))
}

/// Split `content` into records the way repeated `IO#gets(sep, limit)`
/// would: each record ends with `sep` (kept unless `chomp`), a `nil`
/// separator slurps, `""` is paragraph mode, and `limit` caps a
/// record's byte length.
fn split_records(
    content: &[u8],
    sep: Option<&[u8]>,
    limit: Option<usize>,
    chomp: bool,
) -> Vec<Vec<u8>> {
    let mut out: Vec<Vec<u8>> = vec![];
    match sep {
        None => {
            if !content.is_empty() {
                out.push(content.to_vec());
            }
        }
        Some([]) => {
            // Paragraph mode: records end at a blank line; extra blank
            // lines between paragraphs are skipped.
            let mut i = 0;
            while i < content.len() {
                while content[i..].starts_with(b"\n") {
                    i += 1;
                }
                if i >= content.len() {
                    break;
                }
                let end = content[i..]
                    .windows(2)
                    .position(|w| w == b"\n\n")
                    .map(|p| i + p + 2)
                    .unwrap_or(content.len());
                out.push(content[i..end].to_vec());
                i = end;
            }
        }
        Some(sep) => {
            let mut i = 0;
            while i < content.len() {
                let mut end = content[i..]
                    .windows(sep.len())
                    .position(|w| w == sep)
                    .map(|p| i + p + sep.len())
                    .unwrap_or(content.len());
                if let Some(l) = limit
                    && end - i > l
                {
                    end = i + l;
                }
                out.push(content[i..end].to_vec());
                i = end;
            }
        }
    }
    if chomp {
        for rec in &mut out {
            super::io::chomp_line(rec, sep, limit);
        }
    }
    out
}

///
/// ### File.size
/// - size(path) -> Integer
///
/// Returns the size of the file in bytes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/size.html]
#[monoruby_builtin]
fn file_size(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match size_source(vm, globals, lfp.arg(0))? {
        SizeSource::Path(path) => {
            let path_str = path.to_string_lossy();
            let metadata = std::fs::metadata(&path).map_err(|e| {
                MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_size", &path)
            })?;
            Ok(Value::integer(metadata.len() as i64))
        }
        SizeSource::Fd(fd) => Ok(Value::integer(fstat_size(fd)?)),
    }
}

enum SizeSource {
    Path(std::path::PathBuf),
    Fd(i32),
}

/// `File.size` / `File.size?` accept a path (via `#to_path` / `#to_str`)
/// or an IO-convertible object (via `#to_io`, fstat(2) on its fd) —
/// CRuby's `rb_stat` order: path coercion first, then `#to_io`.
fn size_source(vm: &mut Executor, globals: &mut Globals, arg: Value) -> Result<SizeSource> {
    match to_path(vm, globals, arg) {
        Ok(path) => Ok(SizeSource::Path(path)),
        Err(path_err) => {
            let to_io = IdentId::get_id("to_io");
            if globals.check_method(arg, to_io).is_none() {
                return Err(path_err);
            }
            let io = vm.invoke_method_inner(globals, to_io, arg, &[], None, None)?;
            if io.try_rvalue().map(|rv| rv.ty()) != Some(ObjTy::IO) {
                return Err(path_err);
            }
            Ok(SizeSource::Fd(io.as_io_inner().fileno()?))
        }
    }
}

/// Size of an open descriptor via fstat(2).
fn fstat_size(fd: i32) -> Result<i64> {
    let mut st = std::mem::MaybeUninit::<libc::stat>::uninit();
    // SAFETY: `st` is a properly sized out-buffer for fstat(2).
    let rc = unsafe { libc::fstat(fd, st.as_mut_ptr()) };
    if rc != 0 {
        return Err(MonorubyErr::ioerr("closed stream"));
    }
    // SAFETY: fstat succeeded, so `st` is initialized.
    Ok(unsafe { st.assume_init() }.st_size as i64)
}

///
/// ### File.size?
/// - size?(path) -> Integer | nil
///
/// Returns the size of the file if it exists and has non-zero size, nil otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/size=3f.html]
#[monoruby_builtin]
fn file_size_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let size = match size_source(vm, globals, lfp.arg(0))? {
        SizeSource::Path(path) => match std::fs::metadata(&path) {
            Ok(metadata) => metadata.len() as i64,
            Err(_) => return Ok(Value::nil()),
        },
        SizeSource::Fd(fd) => match fstat_size(fd) {
            Ok(size) => size,
            Err(_) => return Ok(Value::nil()),
        },
    };
    if size == 0 {
        Ok(Value::nil())
    } else {
        Ok(Value::integer(size))
    }
}

///
/// ### File#size (instance method)
/// - size -> Integer
///
/// Returns the size of the file in bytes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/i/size.html]
#[monoruby_builtin]
fn size(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let io = self_.as_io_inner();
    // fstat(2) on the open descriptor: works after the path was unlinked
    // and raises IOError (via `fileno`) on a closed stream.
    let fd = io.fileno()?;
    Ok(Value::integer(fstat_size(fd)?))
}

///
/// ### File.ftype
/// - ftype(filename) -> String
///
/// Returns one of: `"file"`, `"directory"`, `"characterSpecial"`,
/// `"blockSpecial"`, `"fifo"`, `"link"`, `"socket"`, `"unknown"`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/ftype.html]
#[monoruby_builtin]
fn ftype(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::FileTypeExt;
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy();
    let metadata = std::fs::symlink_metadata(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_ftype", &path)
    })?;
    let ft = metadata.file_type();
    let s = if ft.is_file() {
        "file"
    } else if ft.is_dir() {
        "directory"
    } else if ft.is_symlink() {
        "link"
    } else if ft.is_char_device() {
        "characterSpecial"
    } else if ft.is_block_device() {
        "blockSpecial"
    } else if ft.is_fifo() {
        "fifo"
    } else if ft.is_socket() {
        "socket"
    } else {
        "unknown"
    };
    Ok(Value::string_from_str(s))
}

/// Stat the path and return metadata, or return `default` on error.
fn stat_or<T>(
    vm: &mut Executor,
    globals: &mut Globals,
    val: Value,
    default: T,
    f: impl FnOnce(&std::fs::Metadata) -> T,
) -> Result<T> {
    let path = to_path(vm, globals, val)?;
    Ok(match std::fs::metadata(&path) {
        Ok(meta) => f(&meta),
        Err(_) => default,
    })
}

///
/// ### File.owned?
/// - owned?(path) -> bool
///
/// Returns `true` if the file's owner uid matches the effective uid of
/// the calling process.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/owned=3f.html]
#[monoruby_builtin]
fn owned_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    // SAFETY: geteuid is a POSIX system call that is safe to call.
    let euid = unsafe { libc::geteuid() };
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.uid() == euid)?;
    Ok(Value::bool(b))
}

///
/// ### File.grpowned?
/// - grpowned?(path) -> bool
///
/// Returns `true` if the file's owner gid matches the effective gid of
/// the calling process.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/grpowned=3f.html]
#[monoruby_builtin]
fn grpowned_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    // SAFETY: getegid is a POSIX system call that is safe to call.
    let egid = unsafe { libc::getegid() };
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.gid() == egid)?;
    Ok(Value::bool(b))
}

const S_ISUID: u32 = 0o4000;
const S_ISGID: u32 = 0o2000;
const S_ISVTX: u32 = 0o1000;

///
/// ### File.setuid?
/// - setuid?(path) -> bool
///
/// Returns `true` if the file has the set-user-id bit set.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/setuid=3f.html]
#[monoruby_builtin]
fn setuid_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.mode() & S_ISUID != 0)?;
    Ok(Value::bool(b))
}

///
/// ### File.setgid?
/// - setgid?(path) -> bool
///
/// Returns `true` if the file has the set-group-id bit set.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/setgid=3f.html]
#[monoruby_builtin]
fn setgid_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.mode() & S_ISGID != 0)?;
    Ok(Value::bool(b))
}

///
/// ### File.sticky?
/// - sticky?(path) -> bool
///
/// Returns `true` if the file has the sticky bit set.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/sticky=3f.html]
#[monoruby_builtin]
fn sticky_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.mode() & S_ISVTX != 0)?;
    Ok(Value::bool(b))
}

///
/// ### File.world_readable?
/// - world_readable?(path) -> Integer | nil
///
/// Returns the file's permission bits if the file is world-readable,
/// `nil` otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/world_readable=3f.html]
#[monoruby_builtin]
fn world_readable_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let opt = stat_or(vm, globals, lfp.arg(0), None, |m| {
        if m.mode() & 0o004 != 0 {
            Some((m.mode() & 0o777) as i64)
        } else {
            None
        }
    })?;
    Ok(opt.map(Value::integer).unwrap_or_else(Value::nil))
}

///
/// ### File.world_writable?
/// - world_writable?(path) -> Integer | nil
///
/// Returns the file's permission bits if the file is world-writable,
/// `nil` otherwise.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/world_writable=3f.html]
#[monoruby_builtin]
fn world_writable_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let opt = stat_or(vm, globals, lfp.arg(0), None, |m| {
        if m.mode() & 0o002 != 0 {
            Some((m.mode() & 0o777) as i64)
        } else {
            None
        }
    })?;
    Ok(opt.map(Value::integer).unwrap_or_else(Value::nil))
}

///
/// ### File.socket?
/// - socket?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/socket=3f.html]
#[monoruby_builtin]
fn socket_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::FileTypeExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.file_type().is_socket())?;
    Ok(Value::bool(b))
}

///
/// ### File.chardev?
/// - chardev?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/chardev=3f.html]
#[monoruby_builtin]
fn chardev_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::FileTypeExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| {
        m.file_type().is_char_device()
    })?;
    Ok(Value::bool(b))
}

///
/// ### File.blockdev?
/// - blockdev?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/blockdev=3f.html]
#[monoruby_builtin]
fn blockdev_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::FileTypeExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| {
        m.file_type().is_block_device()
    })?;
    Ok(Value::bool(b))
}

///
/// ### File.pipe?
/// - pipe?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/pipe=3f.html]
#[monoruby_builtin]
fn pipe_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::FileTypeExt;
    let b = stat_or(vm, globals, lfp.arg(0), false, |m| m.file_type().is_fifo())?;
    Ok(Value::bool(b))
}

///
/// ### File.readlink
/// - readlink(path) -> String
///
/// Returns the target of the symbolic link `path`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/readlink.html]
#[monoruby_builtin]
fn file_readlink(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy();
    let target = std::fs::read_link(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_readlink", &path)
    })?;
    Ok(Value::string(conv_pathbuf(&target)))
}

///
/// ### File.link
/// - link(old, new) -> 0
///
/// Creates a hard link `new` pointing to the existing file `old`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/link.html]
#[monoruby_builtin]
fn file_link(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let old = to_path(vm, globals, lfp.arg(0))?;
    let new = to_path(vm, globals, lfp.arg(1))?;
    let new_str = new.to_string_lossy().to_string();
    std::fs::hard_link(&old, &new).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_link", &new_str)
    })?;
    Ok(Value::integer(0))
}

///
/// ### File.rename
/// - rename(from, to) -> 0
///
/// Renames the file `from` to `to`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/rename.html]
#[monoruby_builtin]
fn file_rename(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let from = to_path(vm, globals, lfp.arg(0))?;
    let to = to_path(vm, globals, lfp.arg(1))?;
    let from_str = from.to_string_lossy().to_string();
    std::fs::rename(&from, &to).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_rename", &from_str)
    })?;
    Ok(Value::integer(0))
}

///
/// ### File.truncate
/// - truncate(path, length) -> 0
///
/// Truncates the file `path` to be at most `length` bytes.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/truncate.html]
#[monoruby_builtin]
fn file_truncate(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy().to_string();
    let length = lfp.arg(1).coerce_to_int_i64(vm, globals)?;
    if length < 0 {
        // CRuby surfaces the truncate(2) failure: Errno::EINVAL.
        let err = std::io::Error::from_raw_os_error(libc::EINVAL);
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "truncate",
            &path_str,
        ));
    }
    let file = std::fs::OpenOptions::new()
        .write(true)
        .open(&path)
        .map_err(|e| {
            MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path_str)
        })?;
    file.set_len(length as u64).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_truncate", &path_str)
    })?;
    Ok(Value::integer(0))
}

///
/// ### File#truncate (instance method)
/// - truncate(length) -> 0
///
/// Truncates the open file to at most `length` bytes via `ftruncate(2)` on the
/// underlying descriptor (so it works even after the path was unlinked, and
/// does not disturb the read/write offset).
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/i/truncate.html]
#[monoruby_builtin]
fn file_truncate_instance(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let length = lfp.arg(0).coerce_to_int_i64(vm, globals)?;
    lfp.self_val().as_io_inner().ensure_writable()?;
    if length < 0 {
        // CRuby surfaces the ftruncate(2) failure: Errno::EINVAL.
        let err = std::io::Error::from_raw_os_error(libc::EINVAL);
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "ftruncate"));
    }
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: `fd` is this File's open descriptor; `ftruncate` only resizes it.
    let rc = unsafe { libc::ftruncate(fd, length as libc::off_t) };
    if rc != 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "ftruncate"));
    }
    Ok(Value::integer(0))
}

/// Convert `val` (a Time, Integer, or Float) to a `libc::timeval`.
fn value_to_timeval(
    vm: &mut Executor,
    globals: &mut Globals,
    val: Value,
) -> Result<libc::timeval> {
    // CRuby: a nil time means "now" (`File.utime(nil, nil, path)`).
    if val.is_nil() {
        let mut tv = libc::timeval {
            tv_sec: 0,
            tv_usec: 0,
        };
        // SAFETY: plain gettimeofday(2) into a valid out-buffer.
        unsafe { libc::gettimeofday(&mut tv, std::ptr::null_mut()) };
        return Ok(tv);
    }
    if let Some(rv) = val.try_rvalue()
        && rv.ty() == ObjTy::TIME
    {
        let to_f = IdentId::TO_F;
        let f = vm.invoke_method_inner(globals, to_f, val, &[], None, None)?;
        if let Some(f) = f.try_float() {
            let secs = f.floor() as i64;
            let usec = ((f - f.floor()) * 1_000_000.0) as i64;
            return Ok(libc::timeval {
                tv_sec: secs,
                // `timeval.tv_usec` is `i64` on glibc but `i32` on macOS;
                // cast through the platform's `suseconds_t` alias.
                tv_usec: usec as libc::suseconds_t,
            });
        }
    }
    if let Some(f) = val.try_float() {
        let secs = f.floor() as i64;
        let usec = ((f - f.floor()) * 1_000_000.0) as i64;
        return Ok(libc::timeval {
            tv_sec: secs,
            tv_usec: usec as libc::suseconds_t,
        });
    }
    let secs = val.coerce_to_int_i64(vm, globals)?;
    Ok(libc::timeval {
        tv_sec: secs,
        tv_usec: 0,
    })
}

fn utime_impl(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    follow_symlinks: bool,
) -> Result<Value> {
    use std::os::unix::ffi::OsStrExt;
    let args = lfp.arg(0).as_array();
    if args.len() < 2 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected 2+)",
            args.len()
        )));
    }
    let atime = value_to_timeval(vm, globals, args[0])?;
    let mtime = value_to_timeval(vm, globals, args[1])?;
    let times = [atime, mtime];
    let mut count = 0i64;
    for arg in args[2..].iter() {
        let path = to_path(vm, globals, *arg)?;
        let path_str = path.to_string_lossy().to_string();
        let c = std::ffi::CString::new(path.as_os_str().as_bytes())
            .map_err(|_| MonorubyErr::argumenterr("path contains NUL byte"))?;
        // SAFETY: `c` and `times` are valid pointers for the duration of
        // the call. `utimes`/`lutimes` are POSIX system calls.
        let rc = unsafe {
            if follow_symlinks {
                libc::utimes(c.as_ptr(), times.as_ptr())
            } else {
                libc::lutimes(c.as_ptr(), times.as_ptr())
            }
        };
        if rc != 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_file_s_utime",
                &path_str,
            ));
        }
        count += 1;
    }
    Ok(Value::integer(count))
}

///
/// ### File.utime
/// - utime(atime, mtime, *path) -> Integer
///
/// Sets the access and modification times of each `path`. `atime` and
/// `mtime` may be `Time`, `Integer`, or `Float`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/utime.html]
#[monoruby_builtin]
fn utime(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    utime_impl(vm, globals, lfp, true)
}

///
/// ### File.lutime
/// - lutime(atime, mtime, *path) -> Integer
///
/// Same as `File.utime` but does not follow symlinks (uses `lutimes(2)`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/lutime.html]
#[monoruby_builtin]
fn lutime(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    utime_impl(vm, globals, lfp, false)
}

fn chown_impl(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    follow_symlinks: bool,
) -> Result<Value> {
    use std::os::unix::ffi::OsStrExt;
    let args = lfp.arg(0).as_array();
    if args.len() < 2 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected 2+)",
            args.len()
        )));
    }
    let uid = if args[0].is_nil() {
        u32::MAX
    } else {
        args[0].coerce_to_int_i64(vm, globals)? as u32
    };
    let gid = if args[1].is_nil() {
        u32::MAX
    } else {
        args[1].coerce_to_int_i64(vm, globals)? as u32
    };
    let mut count = 0i64;
    for arg in args[2..].iter() {
        let path = to_path(vm, globals, *arg)?;
        let path_str = path.to_string_lossy().to_string();
        let c = std::ffi::CString::new(path.as_os_str().as_bytes())
            .map_err(|_| MonorubyErr::argumenterr("path contains NUL byte"))?;
        // SAFETY: `c` is a valid pointer. `chown`/`lchown` are POSIX system
        // calls. uid_t::MAX is the documented sentinel for "leave unchanged".
        let rc = unsafe {
            if follow_symlinks {
                libc::chown(c.as_ptr(), uid, gid)
            } else {
                libc::lchown(c.as_ptr(), uid, gid)
            }
        };
        if rc != 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_file_s_chown",
                &path_str,
            ));
        }
        count += 1;
    }
    Ok(Value::integer(count))
}

///
/// ### File.chown
/// - chown(uid, gid, *path) -> Integer
///
/// Changes the owner uid and group gid of each `path`. Pass `nil` to leave
/// either component unchanged. Returns the number of paths processed.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/chown.html]
#[monoruby_builtin]
fn file_chown(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    chown_impl(vm, globals, lfp, true)
}

///
/// ### File.lchown
/// - lchown(uid, gid, *path) -> Integer
///
/// Same as `File.chown` but operates on symbolic links themselves rather
/// than their targets (uses `lchown(2)`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/lchown.html]
#[monoruby_builtin]
fn file_lchown(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    chown_impl(vm, globals, lfp, false)
}

///
/// ### File.mkfifo
/// - mkfifo(path, mode = 0o666) -> 0
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/mkfifo.html]
#[monoruby_builtin]
fn file_mkfifo(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use std::os::unix::ffi::OsStrExt;
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy().to_string();
    let mode = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_int_i64(vm, globals)? as libc::mode_t
    } else {
        0o666
    };
    let c = std::ffi::CString::new(path.as_os_str().as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("path contains NUL byte"))?;
    // SAFETY: `c` is a valid pointer. `mkfifo` is a POSIX system call.
    let rc = unsafe { libc::mkfifo(c.as_ptr(), mode) };
    if rc != 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "rb_file_s_mkfifo",
            &path_str,
        ));
    }
    Ok(Value::integer(0))
}

/// Given a `SystemTime` (or fallible alternative), invoke `Time.at(secs, usec)`
/// to materialize a Ruby Time value. Errors propagate as Ruby exceptions.
fn system_time_to_value(
    vm: &mut Executor,
    globals: &mut Globals,
    t: std::time::SystemTime,
) -> Result<Value> {
    let dur = t
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|e| MonorubyErr::runtimeerr(format!("invalid time: {}", e)))?;
    let secs = Value::integer(dur.as_secs() as i64);
    let usec = Value::integer((dur.subsec_micros()) as i64);
    let time_class = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Time"))
        .ok_or_else(|| MonorubyErr::runtimeerr("Time class not defined"))?;
    let at = IdentId::get_id("at");
    vm.invoke_method_inner(globals, at, time_class, &[secs, usec], None, None)
}

fn file_time_attr(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    f: impl FnOnce(&std::fs::Metadata) -> std::io::Result<std::time::SystemTime>,
) -> Result<Value> {
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy().to_string();
    let metadata = std::fs::metadata(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_stat", &path)
    })?;
    let t = f(&metadata).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_time", &path)
    })?;
    system_time_to_value(vm, globals, t)
}

///
/// ### File.atime
/// - atime(path) -> Time
///
/// Returns the last access time of `path`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/atime.html]
#[monoruby_builtin]
fn file_atime(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    file_time_attr(vm, globals, lfp, |m| m.accessed())
}

///
/// ### File.mtime
/// - mtime(path) -> Time
///
/// Returns the last modification time of `path`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/mtime.html]
#[monoruby_builtin]
fn file_mtime(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    file_time_attr(vm, globals, lfp, |m| m.modified())
}

///
/// ### File.ctime
/// - ctime(path) -> Time
///
/// Returns the inode-change time (`st_ctime`) of `path`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/ctime.html]
#[monoruby_builtin]
fn file_ctime(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy().to_string();
    let metadata = std::fs::metadata(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_stat", &path)
    })?;
    let secs = metadata.ctime();
    let nsec = metadata.ctime_nsec();
    let t = std::time::UNIX_EPOCH
        + std::time::Duration::new(secs.max(0) as u64, (nsec.max(0)) as u32);
    system_time_to_value(vm, globals, t)
}

///
/// ### File.birthtime
/// - birthtime(path) -> Time
///
/// Returns the inode birth time of `path`. Raises `NotImplementedError`
/// when the filesystem does not record it.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/birthtime.html]
#[monoruby_builtin]
fn file_birthtime(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy().to_string();
    let metadata = std::fs::metadata(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_stat", &path)
    })?;
    match metadata.created() {
        Ok(t) => system_time_to_value(vm, globals, t),
        // A filesystem that does not record the birth time (statx btime
        // unsupported) raises NotImplementedError, like CRuby.
        Err(e) if e.kind() == std::io::ErrorKind::Unsupported => {
            Err(MonorubyErr::not_implemented_err(
                &globals.store,
                "birthtime is unimplemented on this filesystem",
            ))
        }
        Err(e) => Err(MonorubyErr::errno_with_path(
            &globals.store,
            &e,
            "rb_file_s_time",
            &path_str,
        )),
    }
}

/// Build a `Time` value from a (seconds, nanoseconds) pair via
/// `Time.at(secs, nsec, :nanosecond)`.
fn time_from_secs_nsec(
    vm: &mut Executor,
    globals: &mut Globals,
    secs: i64,
    nsec: i64,
) -> Result<Value> {
    let time_class = globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Time"))
        .ok_or_else(|| MonorubyErr::runtimeerr("Time class not defined"))?;
    let at = IdentId::get_id("at");
    let unit = Value::symbol(IdentId::get_id("nanosecond"));
    vm.invoke_method_inner(
        globals,
        at,
        time_class,
        &[Value::integer(secs), Value::integer(nsec), unit],
        None,
        None,
    )
}

/// Populate a `File::Stat` instance's ivars from a `std::fs::Metadata`
/// (which wraps a `struct stat`). The Ruby accessors in
/// `builtins/file_stat.rb` read these fields.
fn fill_stat_ivars(
    vm: &mut Executor,
    globals: &mut Globals,
    mut obj: Value,
    metadata: &std::fs::Metadata,
) -> Result<()> {
    use std::os::unix::fs::MetadataExt;
    let pairs: &[(&str, i64)] = &[
        ("@dev", metadata.dev() as i64),
        ("@ino", metadata.ino() as i64),
        ("@mode", metadata.mode() as i64),
        ("@nlink", metadata.nlink() as i64),
        ("@uid", metadata.uid() as i64),
        ("@gid", metadata.gid() as i64),
        ("@rdev", metadata.rdev() as i64),
        ("@size", metadata.size() as i64),
        ("@blksize", metadata.blksize() as i64),
        ("@blocks", metadata.blocks() as i64),
    ];
    for (name, val) in pairs {
        obj.set_instance_var(&mut globals.store, name, Value::integer(*val))?;
    }
    let atime = time_from_secs_nsec(vm, globals, metadata.atime(), metadata.atime_nsec())?;
    let mtime = time_from_secs_nsec(vm, globals, metadata.mtime(), metadata.mtime_nsec())?;
    let ctime = time_from_secs_nsec(vm, globals, metadata.ctime(), metadata.ctime_nsec())?;
    obj.set_instance_var(&mut globals.store, "@atime", atime)?;
    obj.set_instance_var(&mut globals.store, "@mtime", mtime)?;
    obj.set_instance_var(&mut globals.store, "@ctime", ctime)?;
    Ok(())
}

/// Build a fresh `File::Stat` object for `path`. When `follow` is
/// true, symlinks are dereferenced (stat(2)); otherwise the link
/// itself is described (lstat(2)).
fn build_stat(
    vm: &mut Executor,
    globals: &mut Globals,
    path_val: Value,
    follow: bool,
) -> Result<Value> {
    let path = to_path(vm, globals, path_val)?;
    let path_str = path.to_string_lossy().to_string();
    let metadata = if follow {
        std::fs::metadata(&path)
    } else {
        std::fs::symlink_metadata(&path)
    }
    .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_stat", &path))?;
    let stat_class = vm
        .get_qualified_constant(globals, OBJECT_CLASS, &["File", "Stat"])?
        .as_class();
    let obj = Value::object(stat_class.id());
    fill_stat_ivars(vm, globals, obj, &metadata)?;
    Ok(obj)
}

///
/// ### File.stat
/// - stat(path) -> File::Stat
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/stat.html]
#[monoruby_builtin]
fn file_stat(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    build_stat(vm, globals, lfp.arg(0), true)
}

///
/// ### File.lstat
/// - lstat(path) -> File::Stat
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/lstat.html]
#[monoruby_builtin]
fn file_lstat(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    build_stat(vm, globals, lfp.arg(0), false)
}

///
/// ### IO#stat / File#stat
/// - stat -> File::Stat
///
/// Instance method: fstat(2) on the open descriptor — works for pipes
/// and after the path was unlinked, and raises IOError on a closed
/// stream (via `fileno`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/stat.html]
#[monoruby_builtin]
fn file_instance_stat(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // Borrow the fd as a std File without taking ownership (ManuallyDrop:
    // no close on drop) just to read its metadata.
    use std::os::fd::FromRawFd;
    // SAFETY: `fd` is this IO's live descriptor; the ManuallyDrop wrapper
    // guarantees we never close it here.
    let borrowed = std::mem::ManuallyDrop::new(unsafe { std::fs::File::from_raw_fd(fd) });
    let metadata = borrowed
        .metadata()
        .map_err(|e| MonorubyErr::errno_with_msg(&globals.store, &e, "fstat"))?;
    let stat_class = vm
        .get_qualified_constant(globals, OBJECT_CLASS, &["File", "Stat"])?
        .as_class();
    let obj = Value::object(stat_class.id());
    fill_stat_ivars(vm, globals, obj, &metadata)?;
    Ok(obj)
}

///
/// ### File#lstat
/// - lstat -> File::Stat
///
/// Instance method: like `#stat`, but does not follow the symlink the
/// File was opened through (lstat(2) on the original path).
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/i/lstat.html]
#[monoruby_builtin]
fn file_instance_lstat(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = match lfp.self_val().as_io_inner().path() {
        Some(p) => Value::string(p),
        None => {
            return Err(MonorubyErr::runtimeerr(
                "File#lstat: no path for this stream",
            ));
        }
    };
    build_stat(vm, globals, path, false)
}

///
/// ### File::Stat.new
/// - new(path) -> File::Stat
///
/// [https://docs.ruby-lang.org/ja/latest/method/File=3a=3aStat/s/new.html]
#[monoruby_builtin]
fn stat_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy().to_string();
    let metadata = std::fs::metadata(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_stat_new", &path_str))?;
    fill_stat_ivars(vm, globals, lfp.self_val(), &metadata)?;
    Ok(Value::nil())
}

///
/// ### File.identical?
/// - identical?(file1, file2) -> bool
///
/// Returns `true` if both paths refer to the same file (matching device and
/// inode numbers).
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/identical=3f.html]
#[monoruby_builtin]
fn identical_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::fs::MetadataExt;
    let path1 = to_path(vm, globals, lfp.arg(0))?;
    let path2 = to_path(vm, globals, lfp.arg(1))?;
    let m1 = match std::fs::metadata(&path1) {
        Ok(m) => m,
        Err(_) => return Ok(Value::bool(false)),
    };
    let m2 = match std::fs::metadata(&path2) {
        Ok(m) => m,
        Err(_) => return Ok(Value::bool(false)),
    };
    Ok(Value::bool(m1.dev() == m2.dev() && m1.ino() == m2.ino()))
}

///
/// ### File.realdirpath
/// - realdirpath(pathname, basedir = nil) -> String
///
/// Like `File.realpath` but does not require the last component of `pathname`
/// to exist. The directory containing the last component must exist.
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/realdirpath.html]
#[monoruby_builtin]
fn file_realdirpath(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let rs = to_path_rstring(vm, globals, lfp.arg(0))?;
    let enc = rs.encoding();
    let base: Option<Vec<u8>> = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        Some(to_path_rstring(vm, globals, arg1)?.as_bytes().to_vec())
    } else {
        None
    };
    let resolved = check_realpath(globals, rs.as_bytes(), base.as_deref(), false)?;
    Ok(resolved_path_value(globals, resolved, enc, false))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn file_path_ops_preserve_encoding() {
        // basename/extname/split/join/path/absolute_path/expand_path
        // preserve the path argument's encoding in their results.
        run_test_once(
            r##"(p="/foo/bar.baz".encode(Encoding::EUC_JP); [File.basename(p).encoding.name, File.basename(p, ".baz").encoding.name, File.basename(p, ".*"), File.extname(p).encoding.name, File.split(p).map { |x| x.encoding.name }, File.join(p, "q").encoding.name, File.path(p).encoding.name, File.absolute_path(p).encoding.name, File.expand_path("./a".encode(Encoding::Windows_1251)).encoding.name, File.expand_path(p).encoding.name])"##,
        );
    }

    #[test]
    fn file_path_ops_reject_ascii_incompatible() {
        // UTF-16/32 path arguments raise Encoding::CompatibilityError
        // (before the NUL-byte scan — UTF-32 "ab" contains NULs).
        run_test_once(
            r##"(f = ->(x) { begin; x.call; rescue => e; e.class; end }; [f.(-> { File.basename("/foo/bar".encode(Encoding::UTF_16LE)) }), f.(-> { File.path("ab".encode(Encoding::UTF_32BE)) }), f.(-> { File.extname("a.b".encode(Encoding::UTF_16BE)) }), f.(-> { Dir.glob("nope*".encode(Encoding::UTF_16BE)) }), (begin; Dir.glob("x*".encode(Encoding::UTF_16BE)); rescue => e; e.message; end)])"##,
        );
    }

    #[test]
    fn file_expand_path_slashes_and_tilde() {
        // Leading slash runs survive verbatim, interior runs squeeze,
        // ~user expands via getpwnam, mid-path ~ stays literal.
        run_test_once(
            r##"[File.expand_path("////some/path"), File.expand_path("//some/path"), File.expand_path("/some////path"), File.expand_path("/a/./b/../c//d/"), File.expand_path("~root/x"), File.expand_path("/~root/a"), File.expand_path("a", "/"), File.expand_path("../../bin", "/tmp/x"), (begin; File.expand_path("~no_such_user_zzq"); rescue => e; [e.class, e.message]; end)]"##,
        );
    }

    #[test]
    fn file_realpath_component_resolution() {
        // dir/file/../ resolves lexically (no ENOTDIR); a missing
        // intermediate component reports CRuby's realpath error message;
        // a self-referential symlink is ELOOP.
        run_test_once(
            r##"(d="/tmp/mono_rp_#{Process.pid}"; Dir.mkdir(d); File.write("#{d}/file", ""); a=(File.realpath("#{d}/file/../")==d); b=(begin; File.realpath("/no_such_dir_zzq/x"); rescue => e; [e.class, e.message]; end); File.symlink("#{d}/self", "#{d}/self"); c=(begin; File.realpath("#{d}/self"); rescue => e; e.class; end); c2=(begin; File.realdirpath("#{d}/self"); rescue => e; e.class; end); File.unlink("#{d}/self"); File.unlink("#{d}/file"); Dir.rmdir(d); [a,b,c,c2])"##,
        );
    }

    #[test]
    fn file_realdirpath_dangling_symlinks() {
        // The final component may be absent; a dangling symlink resolves
        // to its (absent) target; a missing intermediate dir is ENOENT.
        run_test_once(
            r##"(d="/tmp/mono_rdp_#{Process.pid}"; Dir.mkdir(d); a=(File.realdirpath("#{d}/missing") == "#{d}/missing"); File.symlink("#{d}/absent_file", "#{d}/link"); b=(File.realdirpath("#{d}/link") == "#{d}/absent_file"); c=(begin; File.realdirpath("#{d}/no_dir/file"); rescue => e; [e.class, e.message.sub(d, "")]; end); File.unlink("#{d}/link"); Dir.rmdir(d); [a, b, c])"##,
        );
    }

    #[test]
    fn file_realpath_result_encoding() {
        // Resolved paths convert to the argument's encoding; realpath
        // forces it when the conversion fails, realdirpath keeps UTF-8.
        run_test_once(
            r##"(d="/tmp/mono_rpe_あ_#{Process.pid}"; Dir.mkdir(d); a=File.realpath(".".encode(Encoding::ISO_8859_1), d).encoding.name; b=File.realdirpath(".".encode(Encoding::ISO_8859_1), d).encoding.name; c=File.realpath(d.encode(Encoding::EUC_JP)).encoding.name rescue c=$!.class; Dir.rmdir(d); [a, b])"##,
        );
    }

    #[test]
    fn file_open_integer_modes() {
        // Integer modes keep their creation bits: CREAT creates,
        // CREAT|EXCL raises EEXIST, TRUNC truncates a read-only open,
        // WRONLY|APPEND appends without truncating.
        run_test_once(
            r##"(f="/tmp/mono_om_#{Process.pid}"; a=File.open(f, File::CREAT) { |x| x.class }; b=(begin; File.open(f, File::CREAT|File::EXCL); rescue => e; e.class; end); File.write(f, "hello\n"); File.open(f, File::WRONLY|File::APPEND) { |x| x.write("more\n") }; c=File.read(f); d=File.open(f, File::TRUNC) { |x| x.gets }; e2=File.read(f); File.delete(f); [a,b,c,d,e2])"##,
        );
    }

    #[test]
    fn file_open_mode_string_x_flag_and_errors() {
        // 'wx' maps to O_EXCL; 'rx'/'ax'/unknown modes raise CRuby's
        // lowercase "invalid access mode"; 4 positional args raise the
        // arity error.
        run_test_once(
            r##"(f="/tmp/mono_x_#{Process.pid}"; a=File.open(f, "wx") { |x| x.write("c") }; b=File.read(f); c=(begin; File.open(f, "wx"); rescue => e; e.class; end); d=(begin; File.open(f, "rx"); rescue => e; [e.class, e.message]; end); e2=(begin; File.open(f, "ax"); rescue => e; e.message; end); g=(begin; File.open(f, "fake"); rescue => e; e.message; end); h=(begin; File.open(f, "w", 0o644, "extra"); rescue => e; [e.class, e.message]; end); File.delete(f); [a,b,c,d,e2,g,h])"##,
        );
    }

    #[test]
    fn file_open_keyword_options() {
        // mode:/flags:/perm: keywords; flags: ORs onto both string and
        // integer modes; binmode+newline is rejected.
        run_test_once(
            r##"(f="/tmp/mono_kw_#{Process.pid}"; File.write(f, ""); a=(begin; File.open(f, "w", flags: File::EXCL) {}; rescue => e; e.class; end); b=(begin; File.open(f, File::WRONLY|File::CREAT, flags: File::EXCL) {}; rescue => e; e.class; end); c=File.open(f, mode: "r") { |x| x.class }; d=(begin; File.open(f, "rb", newline: :universal) {}; rescue => e; [e.class, e.message]; end); File.delete(f); g="/tmp/mono_kw2_#{Process.pid}"; File.open(g, "w", perm: 0o600) {}; h=format("%o", File.stat(g).mode & 0o7777); File.delete(g); [a,b,c,d,h])"##,
        );
    }

    #[test]
    fn file_new_ignores_block_and_fd_creation_flags() {
        // File.new never yields a given block (returns the File), and
        // creation flags on an existing fd raise Errno::EINVAL.
        run_test_once(
            r##"(f="/tmp/mono_nb_#{Process.pid}"; fh=File.new(f, "w") { raise "block called" }; a=fh.class; fh.close; b=File.exist?(f); io=File.new(f); c=(begin; File.new(io.fileno, File::CREAT|File::TRUNC|File::WRONLY); rescue => e; [e.class, e.message]; end); c2=File.new(io.fileno, File::TRUNC, autoclose: false).class; io.close; File.delete(f); [a,b,c,c2])"##,
        );
    }

    #[test]
    fn file_to_path_preserves_open_encoding() {
        // IO#path/#to_path reproduce the exact path argument, including
        // its encoding tag.
        run_test_once(
            r##"(f="/tmp/mono_tpe_#{Process.pid}"; File.write(f, ""); io=File.new(f.encode(Encoding::EUC_JP)); a=[io.to_path.encoding.name, io.path == f]; io.close; File.delete(f); a)"##,
        );
    }

    #[test]
    fn io_write_open_args_option() {
        // IO.write's :open_args — a trailing Hash inside the array is
        // keyword-splatted into File.open.
        run_test_once(
            r##"(f="/tmp/mono_oa_#{Process.pid}"; n=IO.write(f, "hi", open_args: ["w", nil, {encoding: "UTF-8"}]); r=File.read(f); File.delete(f); [n, r])"##,
        );
    }

    #[test]
    fn file_to_path_coercion() {
        // Path arguments accept #to_path objects (CRuby's rb_get_path):
        // absolute_path?, fnmatch, chmod, symlink, lstat, realpath, delete.
        run_test_once(
            r##"(o=Object.new; def o.to_path; "/tmp/mono_tp_#{Process.pid}"; end; p2=Object.new; def p2.to_path; "/tmp/mono_tp2_#{Process.pid}"; end; File.write(o.to_path, "abc"); a=File.absolute_path?(o); b=File.fnmatch("*tp*", o); File.chmod(0o644, o); File.symlink(o, p2); c=File.lstat(p2.to_path).symlink?; d=(File.realpath(p2)==File.realpath(o.to_path)); e2=File.delete(p2, o); [a,b,c,d,e2])"##,
        );
    }

    #[test]
    fn file_size_to_io_and_fd() {
        // File.size/size? accept #to_io objects (fstat on the fd); File#size
        // survives an unlink (fstat) and raises IOError once closed.
        run_test_once(
            r##"(f="/tmp/mono_sz_#{Process.pid}"; File.write(f,"12345"); io=File.open(f); o=Object.new; o.define_singleton_method(:to_io){io}; a=File.size(o); b=File.size?(o); c=File.size?("/tmp/mono_sz_none_#{Process.pid}"); e0="/tmp/mono_sz_e_#{Process.pid}"; File.write(e0,""); d=File.size?(e0); h=io.size; File.delete(f); i=io.size; io.close; j=(begin; io.size; rescue => e; e.class; end); File.delete(e0); [a,b,c,d,h,i,j])"##,
        );
    }

    #[test]
    fn file_utime_nil_truncate_einval_binread_offset() {
        // utime(nil, nil) = "now"; truncate/-length → Errno::EINVAL;
        // #truncate on a read-only stream → IOError; binread with a
        // negative offset → Errno::EINVAL.
        run_test_once(
            r##"(f="/tmp/mono_ut_#{Process.pid}"; File.write(f,"x"*10); n=File.utime(nil, nil, f); a=(Time.now - File.mtime(f) < 600); b=(begin; File.truncate(f, -5); rescue => e; e.class; end); io=File.open(f, "r"); c=(begin; io.truncate(3); rescue => e; e.class; end); io.close; w=File.open(f, "a"); d=(begin; w.truncate(-1); rescue => e; e.class; end); w.close; e2=(begin; File.binread(f, 2, -3); rescue => x; x.class; end); g=File.binread(f, 3, 2); File.delete(f); [n,a,b,c,d,e2,g])"##,
        );
    }

    #[test]
    fn file_extname_edges() {
        run_tests(&[
            r##"File.extname("foo.")"##,
            r##"File.extname(".foo.")"##,
            r##"File.extname("...")"##,
            r##"File.extname("..a")"##,
            r##"File.extname(".profile")"##,
            r##"File.extname("/a.b/c")"##,
            r##"File.extname("a.b.c.d.e")"##,
            r##"File.dirname("/////foo/bar/")"##,
        ]);
    }

    #[test]
    fn file_aliases_and_fd_stat() {
        // unlink/delete and empty?/zero? are true aliases (Method#==);
        // IO#stat / File#stat are fstat(2)-based; File#lstat keeps the
        // opened path's symlink.
        run_test_once(
            r##"(a=(File.method(:unlink)==File.method(:delete)); b=(File.method(:empty?)==File.method(:zero?)); f="/tmp/mono_st_#{Process.pid}"; File.write(f,"12345678"); File.chmod(0o644, f); st=File.stat(f); c=st.inspect.include?("mode=0100644"); io=File.open(f); d=io.stat.file?; e2=io.lstat.file?; io.close; File.delete(f); [a,b,c,d,e2])"##,
        );
    }

    #[test]
    fn file_size_realpath_error_paths() {
        // realpath's basedir also goes through #to_path; File.size error
        // paths: #to_io returning a non-IO, a closed IO, a missing path;
        // birthtime returns a Time (or NotImplementedError on filesystems
        // without btime — identical in both interpreters).
        run_test_once(
            r##"(f="/tmp/mono_ep_#{Process.pid}"; File.write(f,"abc"); base=Object.new; def base.to_path; "/tmp"; end; rel="mono_ep_#{Process.pid}"; a=(File.realpath(rel, base)==File.realpath(f)); bad=Object.new; def bad.to_io; "x"; end; b=(begin; File.size(bad); rescue => e; e.class; end); io=File.open(f); io.close; c2=Object.new; c2.define_singleton_method(:to_io){io}; c=(begin; File.size(c2); rescue => e; e.class; end); d=(begin; File.size("/tmp/mono_ep_none_#{Process.pid}"); rescue => e; e.class; end); g=(begin; File.birthtime(f).class; rescue Exception => e; e.class; end); File.delete(f); [a,b,c,d,g])"##,
        );
    }

    #[test]
    fn file_instance_method_coverage() {
        // File# instance timestamps, #truncate (ftruncate on the fd), and
        // #chmod/#chown (return 0).
        run_test_once(
            r##"(f="/tmp/mono_cov_#{Process.pid}.txt"; File.write(f,"hello world"); io=File.open(f,"r+"); a=(io.mtime==File.mtime(f)); b=io.atime.class; c=io.ctime.class; io.truncate(5); io.close; d=File.read(f); g=File.open(f); e2=g.chmod(0o600); h=g.chown(nil,nil); g.close; File.delete(f); [a,b,c,d,e2,h])"##,
        );
    }

    #[test]
    fn file_directory_and_join_coverage() {
        // File.directory? on an IO / #to_io object and its TypeError branch,
        // File.join array flattening + NUL check, and lexical dirname edges.
        run_test_once(
            r##"(a=File.directory?(STDIN); o=Object.new; def o.to_io; STDIN; end; b=File.directory?(o); c=(begin; File.directory?(1); rescue => e; e.class; end); d=(begin; File.directory?(nil); rescue => e; e.class; end); e2=File.join("a",["b","c"]); f=(begin; File.join("\x00x","y"); rescue => x; [x.class,x.message]; end); [a,b,c,d,e2,f,File.dirname("/.."),File.dirname("./b"),File.dirname("..")])"##,
        );
    }

    #[test]
    fn basename_suffix() {
        run_tests(&[
            r##"File.basename("complex.so", ".*")"##,
            r##"File.basename("a.tar.gz", ".*")"##,
            r##"File.basename(".bashrc", ".*")"##,
            r##"File.basename("noext", ".*")"##,
            r##"File.basename("/x/y/foo.rb", ".rb")"##,
            r##"File.basename("/x/y/foo.rb")"##,
            r##"File.basename("dir.d/", ".*")"##,
        ]);
    }

    #[test]
    fn join() {
        run_tests(&[
            r##"File.join("a","b")"##,
            r##"File.join("a/","b")"##,
            r##"File.join("a/","/b")"##,
            r##"File.join("a","/b")"##,
            r##"File.join("a",["b",["c",["d"]]])"##,
            r##"File.join("", "a",["b",["c",["d"]]])"##,
            r##"File.join("","","","a")"##,
            r##"File.join([])"##,
            r##"File.join"##,
        ]);
    }

    #[test]
    fn expand_path() {
        run_test(r##"File.expand_path("..")"##);
        run_test(r##"File.expand_path("..", "/tmp")"##);
        run_test(r##"File.expand_path("~")"##);
    }

    #[test]
    fn dirname() {
        run_tests(&[
            r##"File.dirname("dir/file.ext")"##,
            r##"File.dirname("file.ext")"##,
            r##"File.dirname("foo/bar/")"##,
            r##"File.basename("dir/file.ext")"##,
            r##"File.basename("file.ext")"##,
            r##"File.basename("foo/bar/")"##,
            r##"File.basename("")"##,
            r##"File.basename("/")"##,
            r##"File.basename("//")"##,
            r##"File.basename("..")"##,
            r##"File.basename("/..")"##,
            r##"File.basename("/../")"##,
            r##"File.basename("/../.")"##,
            r##"File.extname("foo/foo.txt")"##,
            r##"File.extname("foo/foo.tar.gz")"##,
            r##"File.extname("foo/bar")"##,
            r##"File.extname("foo/.bar")"##,
            r##"File.extname("foo.txt/bar")"##,
            r##"File.extname(".foo")"##,
            r##"File.extname("foo.")"##,
        ]);
    }

    #[test]
    fn read() {
        run_test(r##"File.read("../LICENSE-MIT")"##);
        run_test(r##"File.binread("../LICENSE-MIT")"##);
        run_test(r##"File.binread("../LICENSE-MIT", 20)"##);
        run_test(r##"File.binread("../LICENSE-MIT", 20, 10)"##);
        run_test(r##"File.exist?("../LICENSE-MIT")"##);
        run_test(r##"File.exist?("../LICENCE-MIT")"##);
    }

    #[test]
    fn file_() {
        run_test(r##"File.file?("monoruby")"##);
        run_test(r##"File.file?("README.md")"##);
        run_test(r##"File.file?("readme.md")"##);
    }

    #[test]
    fn directory_() {
        run_test(r##"File.directory?("monoruby")"##);
        run_test(r##"File.directory?("bin")"##);
        run_test(r##"File.directory?("README.md")"##);
        run_test(r##"File.directory?("readme.md")"##);
    }

    #[test]
    fn path() {
        run_test(r##"File.path("/dev/null")"##);
        run_test(
            r##"
        class MyPath
          def to_path
            "../"
          end
        end
        File.path(MyPath.new)
        "##,
        );
    }

    #[test]
    fn realpath() {
        run_test(r##"File.realpath(".")"##);
        run_test(r##"File.realpath("./../../../")"##);
        run_test(r##"File.realpath("../monoruby")"##);
        run_test(r##"File.realpath("..", "/tmp")"##);
        run_test(r##"File.realpath("tmp", "/")"##);
    }

    #[test]
    fn executable_() {
        run_test(r##"File.executable?("/bin/sh")"##);
        run_test(r##"File.executable?("../LICENSE-MIT")"##);
        run_test(r##"File.executable?("nonexistent_file_xyz")"##);
        run_test(r##"FileTest.executable?("/bin/sh")"##);
    }

    #[test]
    fn readable_writable() {
        run_test(r##"File.readable?("/bin/sh")"##);
        run_test(r##"File.readable?("nonexistent_file_xyz")"##);
        run_test(r##"FileTest.readable?("/bin/sh")"##);
        run_test(r##"File.writable?("/tmp")"##);
        run_test(r##"File.writable?("nonexistent_file_xyz")"##);
        run_test(r##"FileTest.writable?("/tmp")"##);
    }

    #[test]
    fn open() {
        // The resolved path differs between monoruby (vendored stdlib under
        // ~/.monoruby/lib) and the reference CRuby (host install), so we
        // assert the shape instead of comparing the absolute path.
        run_test(
            r##"
              res = $LOAD_PATH.resolve_feature_path("pp")
              [res.class, res[0], res[1].is_a?(String), res[1].end_with?("/pp.rb")]
            "##,
        );
        run_test(r##"$LOAD_PATH.resolve_feature_path("zzzz")"##);
    }

    #[test]
    fn umask() {
        run_test_no_result_check(
            r#"
            old = File.umask(0022)
            cur = File.umask
            File.umask(old)
            raise "umask should be Integer" unless cur.is_a?(Integer)
            raise "umask should be 0022" unless cur == 0022
            "#,
        );
    }

    #[test]
    fn fnmatch() {
        run_tests(&[
            r##"File.fnmatch("cat", "cat")"##,
            r##"File.fnmatch("cat", "category")"##,
            r##"File.fnmatch("c*", "cats")"##,
            r##"File.fnmatch("c?t", "cat")"##,
            r##"File.fnmatch("c?t", "cot")"##,
            r##"File.fnmatch("c?t", "ct")"##,
            r##"File.fnmatch("c[ao]t", "cat")"##,
            r##"File.fnmatch("c[ao]t", "cot")"##,
            r##"File.fnmatch("c[ao]t", "cut")"##,
            r##"File.fnmatch?("cat", "cat")"##,
        ]);
    }

    #[test]
    fn absolute_path() {
        run_test(r##"File.absolute_path("/tmp")"##);
        run_test(r##"File.absolute_path?("/tmp")"##);
        run_test(r##"File.absolute_path?("tmp")"##);
    }

    #[test]
    fn absolute_path_relative() {
        run_test(r##"File.absolute_path("foo", "/tmp")"##);
    }

    #[test]
    fn file_split() {
        run_test(r##"File.split("/home/user/file.txt")"##);
        run_test(r##"File.split("file.txt")"##);
        run_test(r##"File.split("/home/user/")"##);
    }

    #[test]
    fn file_new() {
        run_test(
            r#"
            f = File.new("Cargo.toml", "r")
            result = f.read(10).is_a?(String)
            f.close
            result
            "#,
        );
    }

    #[test]
    fn file_stat() {
        run_test_no_result_check(
            r#"
            stat = File.stat("Cargo.toml")
            [stat.file?, stat.directory?]
            "#,
        );
        run_test_no_result_check(
            r#"
            stat = File.stat("src")
            [stat.file?, stat.directory?]
            "#,
        );
    }

    #[test]
    fn file_delete() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_delete_#{Process.pid}"
            File.write(path, "hello")
            n = File.delete(path)
            raise "expected 1" unless n == 1
            raise "file should not exist" if File.exist?(path)
            "#,
        );
    }

    #[test]
    fn file_unlink() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_unlink_#{Process.pid}"
            File.write(path, "hello")
            n = File.unlink(path)
            raise "expected 1" unless n == 1
            raise "file should not exist" if File.exist?(path)
            "#,
        );
    }

    #[test]
    fn file_chmod_test() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_chmod_#{Process.pid}"
            File.write(path, "hello")
            n = File.chmod(0644, path)
            raise "expected 1" unless n == 1
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_symlink_test() {
        run_test_no_result_check(
            r#"
            target = "/tmp/monoruby_test_symlink_target_#{Process.pid}"
            link = "/tmp/monoruby_test_symlink_link_#{Process.pid}"
            File.delete(link) if File.exist?(link)
            File.delete(target) if File.exist?(target)
            File.write(target, "hello")
            result = File.symlink(target, link)
            raise "expected 0" unless result == 0
            raise "link should exist" unless File.exist?(link)
            raise "content mismatch" unless File.read(link) == "hello"
            File.delete(link)
            File.delete(target)
            "#,
        );
    }

    #[test]
    fn file_size() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_size_#{Process.pid}"
            File.write(path, "hello")
            s = File.size(path)
            raise "expected 5" unless s == 5
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_size_() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_size_q_#{Process.pid}"
            File.write(path, "hello")
            s = File.size?(path)
            raise "expected 5" unless s == 5
            File.write(path, "")
            s = File.size?(path)
            raise "expected nil for empty" unless s.nil?
            s = File.size?("/tmp/monoruby_nonexistent_file_xyz")
            raise "expected nil for nonexistent" unless s.nil?
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_test_real_predicates() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_real_pred_#{Process.pid}"
            File.write(path, "x")
            raise unless FileTest.readable_real?(path) == true
            raise unless FileTest.writable_real?(path) == true
            raise unless File.readable_real?(path) == true
            raise unless FileTest.executable_real?(path) == false
            raise unless FileTest.readable_real?("/tmp/nope_#{Process.pid}_zzz") == false
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_readlines_test() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_readlines_#{Process.pid}"
            File.write(path, "line1\nline2\nline3\n")
            lines = File.readlines(path)
            raise "expected 3 lines" unless lines.length == 3
            raise "expected line1" unless lines[0] == "line1\n"
            raise "expected line2" unless lines[1] == "line2\n"
            raise "expected line3" unless lines[2] == "line3\n"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_path() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_path_#{Process.pid}"
            File.write(path, "x")
            f = File.open(path)
            raise "path mismatch" unless f.path == path
            raise "to_path mismatch" unless f.to_path == path
            raise "class mismatch" unless f.path.is_a?(String)
            f.close
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_instance_size() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_inst_size_#{Process.pid}"
            File.write(path, "hello world")
            f = File.open(path)
            s = f.size
            raise "expected 11 but got #{s}" unless s == 11
            f.close
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_open_with_fd() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_fd_#{Process.pid}"
            File.write(path, "hello fd")
            fd = IO.sysopen(path)
            f = File.open(fd)
            content = f.read
            f.close
            raise "expected 'hello fd' but got '#{content}'" unless content == "hello fd"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_open_mode_binary() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_mode_#{Process.pid}"
            f = File.open(path, "wb")
            f.write("binary")
            f.close
            f = File.open(path, "rb")
            content = f.read
            f.close
            raise "expected 'binary'" unless content == "binary"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_open_mode_with_encoding() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_enc_#{Process.pid}"
            f = File.open(path, "w:UTF-8")
            f.write("encoded")
            f.close
            f = File.open(path, "r:UTF-8")
            content = f.read
            f.close
            raise "expected 'encoded'" unless content == "encoded"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_open_mode_rplus_b() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_rpb_#{Process.pid}"
            File.write(path, "abcdef")
            f = File.open(path, "r+b")
            f.write("XY")
            f.close
            content = File.read(path)
            raise "expected 'XYcdef' but got '#{content}'" unless content == "XYcdef"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn file_open_invalid_fd() {
        run_test_error(r#"File.open(-1)"#);
        run_test_error(r#"File.open(9999)"#);
    }

    #[test]
    fn file_predicate_aliases() {
        // Pure-Ruby aliases over the existing predicates. Idempotent on
        // any path that exists in the workspace, so safe under run_tests'
        // 25-iteration loop.
        run_tests(&[
            r#"File.empty?("Cargo.toml")"#,
            r#"File.empty?("nonexistent_xyz_qq")"#,
            r#"File.readable_real?("Cargo.toml")"#,
            r#"File.writable_real?("Cargo.toml")"#,
            r#"File.executable_real?("Cargo.toml")"#,
            r#"File.executable_real?("/bin/sh")"#,
            r#"FileTest.empty?("Cargo.toml")"#,
        ]);
    }

    #[test]
    fn file_ftype() {
        run_tests(&[
            r#"File.ftype("Cargo.toml")"#,
            r#"File.ftype(".")"#,
            r#"File.ftype("/dev/null")"#,
        ]);
    }

    #[test]
    fn file_owned_grpowned() {
        run_tests(&[
            r#"File.owned?("Cargo.toml")"#,
            r#"File.grpowned?("Cargo.toml")"#,
            r#"File.owned?("/etc/hostname")"#,
            r#"FileTest.owned?("Cargo.toml")"#,
            r#"FileTest.grpowned?("Cargo.toml")"#,
        ]);
    }

    #[test]
    fn file_mode_predicates() {
        run_tests(&[
            r#"File.setuid?("Cargo.toml")"#,
            r#"File.setgid?("Cargo.toml")"#,
            r#"File.sticky?("Cargo.toml")"#,
            r#"File.sticky?("/tmp")"#,
            r#"FileTest.setuid?("Cargo.toml")"#,
            r#"FileTest.setgid?("Cargo.toml")"#,
            r#"FileTest.sticky?("Cargo.toml")"#,
        ]);
    }

    #[test]
    fn file_world_readable_writable() {
        // Cargo.toml is typically owner-writable but world-readable; the
        // exact mode bits depend on the workspace umask, so compare against
        // CRuby (which runs the same code path on the same file).
        run_tests(&[
            r#"File.world_readable?("Cargo.toml").nil?"#,
            r#"File.world_writable?("Cargo.toml").nil?"#,
            r#"File.world_readable?("/dev/null").nil?"#,
            r#"FileTest.world_readable?("Cargo.toml").nil?"#,
            r#"FileTest.world_writable?("Cargo.toml").nil?"#,
        ]);
    }

    #[test]
    fn file_type_predicates() {
        run_tests(&[
            r#"File.socket?("Cargo.toml")"#,
            r#"File.chardev?("Cargo.toml")"#,
            r#"File.chardev?("/dev/null")"#,
            r#"File.blockdev?("Cargo.toml")"#,
            r#"File.pipe?("Cargo.toml")"#,
            r#"FileTest.chardev?("/dev/null")"#,
            r#"FileTest.pipe?("Cargo.toml")"#,
        ]);
    }

    #[test]
    fn file_identical() {
        run_tests(&[
            r#"File.identical?("Cargo.toml", "Cargo.toml")"#,
            r#"File.identical?("Cargo.toml", "README.md")"#,
            r#"File.identical?("Cargo.toml", "nonexistent_xyz")"#,
            r#"FileTest.identical?("Cargo.toml", "Cargo.toml")"#,
        ]);
    }

    #[test]
    fn file_realdirpath() {
        run_tests(&[
            r#"File.realdirpath(".") == File.realpath(".")"#,
            r#"File.realdirpath("Cargo.toml") == File.realpath("Cargo.toml")"#,
            // Tail component need not exist (parent must, though).
            r#"File.realdirpath("./no_such_file_xyz").end_with?("/no_such_file_xyz")"#,
            r#"File.realdirpath("..", "/tmp")"#,
        ]);
    }

    #[test]
    fn file_time_methods_class() {
        // The exact times will not match across two separate processes, so
        // assert on the returned class and the relative ordering instead.
        run_tests(&[
            r#"File.atime("Cargo.toml").is_a?(Time)"#,
            r#"File.mtime("Cargo.toml").is_a?(Time)"#,
            r#"File.ctime("Cargo.toml").is_a?(Time)"#,
            // mtime <= atime usually holds on a freshly checked-out repo,
            // but is not guaranteed on every filesystem; skip ordering and
            // just validate the type.
            r#"File.atime("Cargo.toml").class.name"#,
        ]);
    }

    #[test]
    fn file_binwrite_basic() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_binwrite_#{Process.pid}_#{rand(100000)}"
            begin
              n = File.binwrite(path, "hello")
              [n, File.binread(path)]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_binwrite_with_offset() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_binwrite_off_#{Process.pid}_#{rand(100000)}"
            begin
              File.binwrite(path, "0123456789")
              File.binwrite(path, "AB", 2)
              File.binread(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_truncate() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_trunc_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "hello world")
              File.truncate(path, 5)
              File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_rename() {
        run_test_once(
            r#"
            base = "/tmp/monoruby_test_rename_#{Process.pid}_#{rand(100000)}"
            from = base + ".from"
            to   = base + ".to"
            begin
              File.write(from, "payload")
              File.rename(from, to)
              [File.exist?(from), File.exist?(to), File.read(to)]
            ensure
              File.unlink(from) rescue nil
              File.unlink(to)   rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_link_readlink() {
        run_test_once(
            r#"
            base = "/tmp/monoruby_test_link_#{Process.pid}_#{rand(100000)}"
            target = base + ".target"
            sym    = base + ".sym"
            hard   = base + ".hard"
            begin
              File.write(target, "payload")
              File.symlink(target, sym)
              File.link(target, hard)
              [
                File.readlink(sym) == target,
                File.read(hard),
                File.identical?(target, hard),
                File.identical?(target, sym),
              ]
            ensure
              [hard, sym, target].each { |p| File.unlink(p) rescue nil }
            end
            "#,
        );
    }

    #[test]
    fn file_utime_roundtrip() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_utime_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "x")
              t = Time.at(1_700_000_000)
              File.utime(t, t, path)
              [File.atime(path).to_i, File.mtime(path).to_i]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_open_with_integer_mode() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_intmode_#{Process.pid}_#{rand(100000)}"
            begin
              f = File.new(path, File::WRONLY | File::CREAT | File::TRUNC)
              f.write("ok")
              f.close
              File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_open_with_mode_kw() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_modekw_#{Process.pid}_#{rand(100000)}"
            begin
              f = File.new(path, mode: "w")
              f.write("ok")
              f.close
              File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_lutime() {
        // lutime updates timestamps on the symlink itself rather than its
        // target. Verify the call returns the count of paths processed and
        // that the target's mtime is unchanged afterwards (only the link's
        // metadata moves).
        run_test_once(
            r##"
            base = "/tmp/monoruby_test_lutime_#{Process.pid}_#{rand(100000)}"
            target = base + ".target"
            sym    = base + ".sym"
            begin
              File.write(target, "x")
              File.symlink(target, sym)
              orig = File.mtime(target).to_i
              t = Time.at(1_700_000_000)
              n = File.lutime(t, t, sym)
              [n, File.mtime(target).to_i == orig]
            ensure
              [sym, target].each { |p| File.unlink(p) rescue nil }
            end
            "##,
        );
    }

    #[test]
    fn file_chown_unchanged() {
        // Pass nil/nil so chown is a no-op (kernel leaves uid/gid alone)
        // and only validates the path. CRuby and monoruby should both
        // return the count of paths processed.
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_chown_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "x")
              n = File.chown(nil, nil, path)
              [n, File.owned?(path)]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_lchown_unchanged() {
        run_test_once(
            r#"
            base = "/tmp/monoruby_test_lchown_#{Process.pid}_#{rand(100000)}"
            target = base + ".target"
            sym    = base + ".sym"
            begin
              File.write(target, "x")
              File.symlink(target, sym)
              n = File.lchown(nil, nil, sym)
              n
            ensure
              [sym, target].each { |p| File.unlink(p) rescue nil }
            end
            "#,
        );
    }

    #[test]
    fn file_mkfifo() {
        run_test_once(
            r##"
            path = "/tmp/monoruby_test_mkfifo_#{Process.pid}_#{rand(100000)}"
            begin
              File.mkfifo(path)
              [File.pipe?(path), File.ftype(path)]
            ensure
              File.unlink(path) rescue nil
            end
            "##,
        );
    }

    #[test]
    fn file_birthtime_class() {
        // birthtime is unsupported on some filesystems and raises
        // NotImplementedError. Run inside `rescue` so monoruby/CRuby agree
        // on the rescued shape regardless of the underlying FS.
        run_test_once(
            r#"
            t = File.birthtime("Cargo.toml") rescue :unsupported
            t == :unsupported || t.is_a?(Time)
            "#,
        );
    }

    // ----- error patterns --------------------------------------------------

    #[test]
    fn file_ftype_nonexistent_raises() {
        run_test_error(r#"File.ftype("monoruby_no_such_file_xyz_qq")"#);
    }

    #[test]
    fn file_readlink_not_a_link_raises() {
        run_test_error(r#"File.readlink("Cargo.toml")"#);
    }

    #[test]
    fn file_truncate_nonexistent_raises() {
        run_test_error(r#"File.truncate("monoruby_no_such_file_xyz", 0)"#);
    }

    #[test]
    fn file_rename_nonexistent_raises() {
        run_test_error(
            r#"File.rename("monoruby_no_such_file_xyz", "/tmp/monoruby_target_qq")"#,
        );
    }

    #[test]
    fn file_link_existing_target_raises() {
        // Target already exists → EEXIST.
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_link_eexist_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "x")
              raised = false
              begin
                File.link("Cargo.toml", path)
              rescue SystemCallError
                raised = true
              end
              raised
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_chown_on_missing_path_raises() {
        run_test_error(r#"File.chown(nil, nil, "monoruby_no_such_file_xyz_qq")"#);
    }

    #[test]
    fn file_mkfifo_existing_raises() {
        // Calling mkfifo on a path that already exists returns EEXIST.
        run_test_error(r#"File.mkfifo("Cargo.toml")"#);
    }

    #[test]
    fn file_utime_missing_path_raises() {
        run_test_error(
            r#"File.utime(Time.now, Time.now, "monoruby_no_such_file_xyz_qq")"#,
        );
    }

    #[test]
    fn file_realdirpath_missing_parent_raises() {
        run_test_error(
            r#"File.realdirpath("/no_such_directory_xyz_qq/file")"#,
        );
    }

    #[test]
    fn file_binwrite_negative_offset_raises() {
        run_test_error(
            r##"
            path = "/tmp/monoruby_test_binwneg_#{Process.pid}"
            File.binwrite(path, "x", -5)
            "##,
        );
    }

    #[test]
    fn file_binread_negative_length_raises() {
        run_test_error(r#"File.binread("Cargo.toml", -1)"#);
    }

    #[test]
    fn file_truncate_negative_length_raises() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_trunc_neg_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "hello")
              raised = false
              begin
                File.truncate(path, -1)
              rescue ArgumentError, Errno::EINVAL
                raised = true
              end
              raised
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    /// `File.write(name, string, offset=nil, **opts)` — CRuby's full
    /// arity. Before this was `(name, string)` (2 args max), so any
    /// keyword (`mode:`, `perm:`, `encoding:`) raised ArgumentError
    /// before reaching the I/O. The fix is registration-only: the
    /// extra args are accepted for compatibility and ignored.
    #[test]
    fn file_write_accepts_perm_kwarg() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_filewrite_perm_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "hello", perm: 0o600)
              File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_filewrite_kw_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "data", mode: "w", encoding: "UTF-8")
              File.read(path)
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    #[test]
    fn file_stat_fields_and_predicates() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_stat_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "rubinius")
              s = File.stat(path)
              [
                s.class.name,
                s.size,
                s.file?,
                s.directory?,
                s.ino.is_a?(Integer),
                s.mode.is_a?(Integer),
                s.nlink.is_a?(Integer),
                s.uid.is_a?(Integer),
                s.gid.is_a?(Integer),
                s.ftype,
                s.zero?,
                s.size?,
                s.mtime.is_a?(Time),
                s.blksize.is_a?(Integer),
                s.blocks.is_a?(Integer),
                File::Stat.new(path).file?,
                File.lstat(path).file?,
              ]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
        // Directory ftype / predicate.
        run_test_once(r#"[File.stat("/tmp").directory?, File.stat("/tmp").ftype]"#);
    }

    #[test]
    fn file_stat_missing_raises() {
        run_test_error(r#"File.stat("/nonexistent_monoruby_stat_path_xyz")"#);
        run_test_error(r#"File::Stat.new("/nonexistent_monoruby_stat_path_xyz")"#);
    }

    #[test]
    fn file_instance_stat_method() {
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_instat_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "rubinius")
              f = File.open(path)
              begin
                s = f.stat
                [s.class.name, s.size, s.file?]
              ensure
                f.close
              end
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }


    #[test]
    fn file_open_integer_flags_no_truncate() {
        // WRONLY|CREAT without TRUNC must not truncate an existing
        // file; bare WRONLY opens an existing file without creating.
        run_test_once(
            r#"
            path = "/tmp/monoruby_test_flags_#{Process.pid}_#{rand(100000)}"
            begin
              File.write(path, "hello")
              File.open(path, File::WRONLY | File::CREAT) { |f| f.write("A") }
              a = File.read(path)
              File.open(path, File::WRONLY) { |f| f.write("B") }
              b = File.read(path)
              [a, b]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    /// `File.new` / `File.open` given a raw file descriptor. The fresh File
    /// is reachable only from a Rust local while `init_io_encodings` and
    /// the `File::new() does not take block` warning re-enter Ruby, which
    /// is what the rooting guards. Also covers the encoding-mode default
    /// derived from the fd's access mode when no String mode is passed.
    #[test]
    fn file_from_fd_modes_and_block() {
        run_test_once(
            r#"
            path = "/tmp/mono_cov_file_fd_#{Process.pid}"
            begin
              File.write(path, "hello")
              # Read-only fd, no mode argument -> "r".
              f = File.new(IO.sysopen(path, "r"))
              a = [f.class, f.read]
              f.close
              # Write-only fd -> "w"; read-write fd -> "r+".
              f = File.new(IO.sysopen(path, "w"))
              b = f.class
              f.close
              f = File.new(IO.sysopen(path, "r+"))
              c = f.class
              f.close
              # File.open(fd) with a block closes the File at block exit.
              d = File.open(IO.sysopen(path, "r")) { |io| io.read }
              # File.new(fd) with a block warns and ignores the block.
              f = File.new(IO.sysopen(path, "r")) { |io| :never_called }
              e = [f.class, f.read]
              f.close
              [a, b, c, d, e]
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }

    /// `File.new(path)` given a block warns and ignores it, returning the
    /// open File (the block form is `File.open`).
    #[test]
    fn file_new_with_block_warns() {
        run_test_once(
            r#"
            path = "/tmp/mono_cov_file_new_#{Process.pid}"
            begin
              File.write(path, "hello")
              f = File.new(path, "r") { |io| :never_called }
              res = [f.class, f.read]
              f.close
              res
            ensure
              File.unlink(path) rescue nil
            end
            "#,
        );
    }
}
