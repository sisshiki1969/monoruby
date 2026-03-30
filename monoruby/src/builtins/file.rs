use super::*;
use std::{
    fs::File,
    io::{Seek, SeekFrom, Write},
};
use std::path::Path;

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
    globals.define_builtin_class_func(file, "write", file_write, 2);
    globals.define_builtin_class_func_with(file, "read", file_read, 1, 4, false);
    globals.define_builtin_class_func_with(file, "binread", binread, 1, 3, false);
    globals.define_builtin_class_func_rest(file, "join", join);
    globals.define_builtin_class_func_with(file, "expand_path", expand_path, 1, 2, false);
    globals.define_builtin_class_func_with(file, "dirname", dirname, 1, 2, false);
    globals.define_builtin_class_func_with(file, "basename", basename, 1, 2, false);
    globals.define_builtin_class_func(file, "extname", extname, 1);
    globals.define_builtin_class_func(file, "path", path, 1);
    globals.define_builtin_class_func_with(file, "realpath", realpath, 1, 2, false);
    globals.define_builtin_class_func_with(file, "open", open, 1, 4, false);
    globals.define_builtin_class_func_with(file, "new", open, 1, 4, false);

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

    globals.define_builtin_func_rest(file, "write", write);
    globals.define_builtin_func(file, "size", file_instance_size, 0);

    globals.define_builtin_class_func_with(file, "umask", umask, 0, 1, false);
    globals.define_builtin_class_funcs_with(file, "fnmatch", &["fnmatch?"], fnmatch, 2, 3, false);
    globals.define_builtin_class_func_with(file, "absolute_path", absolute_path, 1, 2, false);
    globals.define_builtin_class_func(file, "absolute_path?", absolute_path_, 1);
    globals.define_builtin_class_func(file, "split", file_split, 1);
    globals.define_builtin_class_func_rest(file, "delete", delete);
    globals.define_builtin_class_func_rest(file, "unlink", delete);
    globals.define_builtin_class_func_rest(file, "chmod", chmod);
    globals.define_builtin_class_func(file, "symlink", file_symlink, 2);
    globals.define_builtin_class_func(file, "readlines", readlines, 1);

    globals.define_builtin_class_func(file, "size", file_size, 1);
    globals.define_builtin_module_func(file_test, "size", file_size, 1);

    globals.define_builtin_class_func(file, "size?", file_size_, 1);
    globals.define_builtin_module_func(file_test, "size?", file_size_, 1);

    globals.define_builtin_singleton_func(
        globals.get_load_path(),
        "resolve_feature_path",
        resolve_feature_path,
        1,
    );
}

///
/// ### File.write
/// - write(path, string, opt={}) -> Integer
/// - [NOT SUPPORTED] write(path, string, offset=nil, opt={}) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/write.html]
#[monoruby_builtin]
fn file_write(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let name = lfp.arg(0).coerce_to_string(vm, globals)?;
    let mut file = match File::create(&name) {
        Ok(file) => file,
        Err(err) => return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_sysopen", &name)),
    };
    let val = lfp.arg(1);
    let len = if let Some(s) = val.is_rstring() {
        if let Err(err) = file.write_all(&s) {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_write", &name));
        };
        s.len()
    } else {
        let v = val.to_s(&globals.store).into_bytes();
        if let Err(err) = file.write_all(&v) {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_write", &name));
        };
        v.len()
    };

    Ok(Value::integer(len as i64))
}

///
/// ### IO.read
///
/// - read(path, [NOT SUPPORTED]**opt) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/read.html]
#[monoruby_builtin]
fn file_read(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let filename_str = filename.to_string_lossy();
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_sysopen", &filename_str));
        }
    };
    let mut contents = vec![];
    match std::io::Read::read_to_end(&mut file, &mut contents) {
        Ok(_) => {}
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_read", &filename_str));
        }
    };
    Ok(Value::string_from_vec(contents))
}

///
/// ### IO.binread
///
/// - binread(path, length = nil, offset = 0) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/binread.html]
#[monoruby_builtin]
fn binread(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let length = if let Some(arg1) = lfp.try_arg(1) {
        Some(arg1.coerce_to_int(vm, globals)?)
    } else {
        None
    };
    let offset = if let Some(arg2) = lfp.try_arg(2) {
        Some(arg2.coerce_to_int(vm, globals)?)
    } else {
        None
    };
    let filename_str = filename.to_string_lossy().to_string();
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_sysopen", &filename_str));
        }
    };
    if let Some(offset) = offset {
        match file.seek(SeekFrom::Start(offset as _)) {
            Ok(_) => {}
            Err(err) => return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_seek", &filename_str)),
        };
    }
    if let Some(length) = length {
        let mut contents = vec![0; length as usize];
        if let Err(err) = std::io::Read::read_exact(&mut file, &mut contents) {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_read", &filename_str));
        };
        Ok(Value::bytes(contents))
    } else {
        let mut contents = vec![];
        if let Err(err) = std::io::Read::read_to_end(&mut file, &mut contents) {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_io_read", &filename_str));
        };
        Ok(Value::bytes(contents))
    }
}

///
/// ### File.join
///
/// - join(*item) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/join.html]
#[monoruby_builtin]
fn join(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    fn flatten(
        vm: &mut Executor,
        globals: &mut Globals,
        path: &mut String,
        val: Value,
        seen: &mut Vec<u64>,
    ) -> Result<()> {
        match val.try_array_ty() {
            Some(ainfo) => {
                let id = val.id();
                if seen.contains(&id) {
                    return Err(MonorubyErr::argumenterr("recursive array"));
                }
                seen.push(id);
                for v in ainfo.iter().cloned() {
                    flatten(vm, globals, path, v, seen)?;
                }
                seen.pop();
            }
            None => {
                if !path.is_empty() && !path.ends_with('/') {
                    path.push('/');
                }
                let s = val.coerce_to_path_rstring(vm, globals)?;
                let s = s.to_str()?;
                path.push_str(if !path.is_empty() && !s.is_empty() && s.starts_with('/') {
                    &s[1..]
                } else if path.is_empty() && s.is_empty() {
                    "/"
                } else {
                    &s
                });
            }
        }
        Ok(())
    }
    let mut path = String::new();
    let mut seen = vec![];
    for v in lfp.arg(0).as_array().iter().cloned() {
        flatten(vm, globals, &mut path, v, &mut seen)?;
    }
    Ok(Value::string(path))
}

///
/// ### File.expand_path
/// - expand_path(path, default_dir = '.') -> String
/// TODO: support ~USER
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/expand_path.html]
#[monoruby_builtin]
fn expand_path(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(err) => {
            return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "."));
        }
    };
    let arg0 = to_path(vm, globals, lfp.arg(0))?;
    let path = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        let mut path = to_path(vm, globals, arg1)?;
        path.push(arg0);
        path
    } else {
        arg0
    };

    let mut res_path = std::path::PathBuf::new();
    res_path.push(current_dir);

    extend(&mut res_path, path)?;

    #[cfg(windows)]
    let res_path = PathBuf::from(
        std::env::var("HOMEDRIVE")
            .or_else(|_| Err(RubyError::internal("Failed to get home drive.")))?,
    )
    .join(res_path);

    Ok(Value::string(conv_pathbuf(&res_path)))
}

///
/// ### File.dirname
/// - dirname(filename, [NOT SUPPRTED]level=1) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/dirname.html]
#[monoruby_builtin]
fn dirname(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn basename(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let filename = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    let suffix = if let Some(arg1) = lfp.try_arg(1) {
        let s = arg1.coerce_to_string(vm, globals)?;
        if s.is_empty() {
            None
        } else {
            Some(s.to_string())
        }
    } else {
        None
    };
    if filename.is_empty() {
        return Ok(Value::string_from_str(""));
    }
    let filename = filename.to_str()?.to_string();
    let basename = if let Some(s) = filename.split('/').rev().find(|s| !s.is_empty()) {
        s
    } else {
        "/"
    };
    if let Some(suffix) = suffix {
        if basename.ends_with(&suffix) {
            return Ok(Value::string_from_str(
                &basename[..basename.len() - suffix.len()],
            ));
        }
    }
    Ok(Value::string_from_str(basename))
}

///
/// ### File.directory?
/// - directory?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/directory=3f.html]
#[monoruby_builtin]
fn directory_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    match to_canonicalized_path(vm, globals, lfp.arg(0), "1st arg") {
        Ok(path) => Ok(Value::bool(path.is_dir())),
        Err(_) => Ok(Value::bool(false)),
    }
}

///
/// ### File.symlink?
/// - symlink?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/symlink=3f.html]
#[monoruby_builtin]
fn symlink_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match to_canonicalized_path(vm, globals, lfp.arg(0), "1st arg") {
        Ok(path) => Ok(Value::bool(path.is_symlink())),
        Err(_) => Ok(Value::bool(false)),
    }
}

///
/// ### File.extname
/// - extname(filename) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/extname.html]
#[monoruby_builtin]
fn extname(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let extname = match filename.extension() {
        Some(ostr) => format!(".{}", ostr.to_string_lossy()),
        None => "".to_string(),
    };
    Ok(Value::string(extname))
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

///
/// ### File.path
/// - path(filename) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/path.html]
#[monoruby_builtin]
fn path(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::string(to_path_str(vm, globals, lfp.arg(0))?))
}

///
/// ### File.realpath
/// - realpath(pathname, basedir = nil) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/realpath.html]
#[monoruby_builtin]
fn realpath(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut pathname = if let Some(arg1) = lfp.try_arg(1) {
        let path_str = arg1.coerce_to_string(vm, globals)?;
        let path = std::path::PathBuf::from(&path_str);
        match path.canonicalize() {
            Ok(path) => path,
            Err(err) => {
                return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_file_s_realpath", &path_str));
            }
        }
    } else {
        match std::env::current_dir() {
            Ok(path) => path,
            Err(err) => {
                return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "."));
            }
        }
    };
    pathname.push(std::path::PathBuf::from(lfp.arg(0).coerce_to_string(vm, globals)?));
    let pathname_str = pathname.to_string_lossy().to_string();
    match pathname.canonicalize() {
        Ok(file) => Ok(Value::string(file.to_string_lossy().to_string())),
        Err(err) => Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_file_s_realpath", &pathname_str)),
    }
}

///
/// ### File.open
///
/// - open(path, mode = "r", [NOT SUPPORTED] perm = 0666) -> File
/// - open(path, mode = "r", [NOT SUPPORTED] perm = 0666) {|file| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/new.html]
#[monoruby_builtin]
fn open(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // If the first argument is an Integer, treat it as a file descriptor.
    if let Some(fd) = lfp.arg(0).try_fixnum() {
        let name = format!("fd {}", fd);
        // SAFETY: fd is a valid file descriptor provided by the user (e.g. from IO.sysopen).
        let io_inner = IoInner::from_raw_fd(fd as i32, name);
        let res = Value::new_io_with_class(io_inner, FILE_CLASS);
        if let Some(bh) = lfp.block() {
            return vm.invoke_block_once(globals, bh, &[res]);
        }
        return Ok(res);
    }

    let mode = if let Some(arg1) = lfp.try_arg(1) {
        arg1.coerce_to_string(vm, globals)?
    } else {
        "r".to_string()
    };
    let mut opt = File::options();
    // Strip encoding suffix (e.g. ":UTF-8") and normalize the mode string:
    // remove 'b' (binary) flag since it has no effect on Unix.
    let mode_base = mode.split(':').next().unwrap().replace('b', "");
    let opt = match mode_base.as_str() {
        "r" => opt.read(true),
        "w" => opt.write(true).create(true).truncate(true),
        "a" => opt.write(true).create(true).append(true),
        "r+" | "+r" => opt.read(true).write(true),
        "w+" | "+w" => opt.read(true).write(true).create(true).truncate(true),
        "a+" | "+a" => opt.read(true).write(true).create(true).append(true),
        _ => {
            return Err(MonorubyErr::argumenterr(format!(
                "Invalid access mode {}",
                mode
            )));
        }
    };
    let path = to_path_str(vm, globals, lfp.arg(0))?;
    let file = match opt.open(&path) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(&globals.store, &err, "rb_sysopen", &path));
        }
    };
    let res = Value::new_file(file, path);
    if let Some(bh) = lfp.block() {
        return vm.invoke_block_once(globals, bh, &[res]);
    }
    Ok(res)
}

///
/// ### IO#write
/// - write(*str) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/write.html]
#[monoruby_builtin]
fn write(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let data = lfp.arg(0).as_array();
    let mut self_ = lfp.self_val();
    let mut count = 0i64;
    for s in data.iter() {
        if let Some(bytes) = s.is_rstring_inner() {
            count += bytes.len() as i64;
            self_.as_io_inner_mut().write(bytes)?;
        } else {
            let s_str = vm.invoke_tos(globals, *s)?;
            let bytes = s_str.expect_bytes(&globals.store)?;
            count += bytes.len() as i64;
            self_.as_io_inner_mut().write(bytes)?;
        }
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
    match globals.search_lib(&file_name) {
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
        let mask = arg0.coerce_to_int(vm, globals)? as u32;
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
fn fnmatch(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let pattern = lfp.arg(0).coerce_to_string(vm, globals)?;
    let path_str = lfp.arg(1).coerce_to_string(vm, globals)?;
    let flags = if let Some(arg2) = lfp.try_arg(2) {
        arg2.coerce_to_int(vm, globals)? as u32
    } else {
        0
    };
    let fnm_dotmatch = 0x0004;
    let fnm_pathname = 0x0002;
    let dotmatch = flags & fnm_dotmatch != 0;
    let pathname = flags & fnm_pathname != 0;
    let result = fnmatch_pattern(&pattern, &path_str, dotmatch, pathname);
    Ok(Value::bool(result))
}

/// Simple glob-style pattern matching.
fn fnmatch_pattern(pattern: &str, string: &str, dotmatch: bool, pathname: bool) -> bool {
    fn match_inner(pat: &[u8], s: &[u8], dotmatch: bool, pathname: bool) -> bool {
        let mut pi = 0;
        let mut si = 0;
        let mut star_pi = None;
        let mut star_si = None;

        while si < s.len() {
            if pi < pat.len() && pat[pi] == b'*' {
                // Don't match dot at start unless dotmatch
                if si == 0 && s[si] == b'.' && !dotmatch {
                    return false;
                }
                // In pathname mode, * doesn't match /
                star_pi = Some(pi);
                star_si = Some(si);
                pi += 1;
                continue;
            }
            if pi < pat.len() && pat[pi] == b'?' {
                if pathname && s[si] == b'/' {
                    return false;
                }
                if si == 0 && s[si] == b'.' && !dotmatch {
                    return false;
                }
                pi += 1;
                si += 1;
                continue;
            }
            if pi < pat.len() && pat[pi] == b'[' {
                if let Some((matched, new_pi)) = match_bracket(&pat[pi..], s[si]) {
                    if matched {
                        pi = pi + new_pi;
                        si += 1;
                        continue;
                    }
                }
                if let Some(spi) = star_pi {
                    pi = spi + 1;
                    let ssi = star_si.unwrap() + 1;
                    if pathname && s[star_si.unwrap()] == b'/' {
                        return false;
                    }
                    star_si = Some(ssi);
                    si = ssi;
                    continue;
                }
                return false;
            }
            if pi < pat.len() && (pat[pi] == s[si] || pat[pi] == b'\\' && pi + 1 < pat.len() && pat[pi + 1] == s[si]) {
                if pat[pi] == b'\\' {
                    pi += 1;
                }
                pi += 1;
                si += 1;
                continue;
            }
            if let Some(spi) = star_pi {
                pi = spi + 1;
                let ssi = star_si.unwrap() + 1;
                if pathname && s[star_si.unwrap()] == b'/' {
                    return false;
                }
                star_si = Some(ssi);
                si = ssi;
                continue;
            }
            return false;
        }
        while pi < pat.len() && pat[pi] == b'*' {
            pi += 1;
        }
        pi == pat.len()
    }

    fn match_bracket(pat: &[u8], ch: u8) -> Option<(bool, usize)> {
        if pat.is_empty() || pat[0] != b'[' {
            return None;
        }
        let mut i = 1;
        let negate = if i < pat.len() && (pat[i] == b'^' || pat[i] == b'!') {
            i += 1;
            true
        } else {
            false
        };
        let mut matched = false;
        while i < pat.len() && pat[i] != b']' {
            if i + 2 < pat.len() && pat[i + 1] == b'-' {
                if ch >= pat[i] && ch <= pat[i + 2] {
                    matched = true;
                }
                i += 3;
            } else {
                if ch == pat[i] {
                    matched = true;
                }
                i += 1;
            }
        }
        if i < pat.len() && pat[i] == b']' {
            Some((matched ^ negate, i + 1))
        } else {
            None
        }
    }

    match_inner(pattern.as_bytes(), string.as_bytes(), dotmatch, pathname)
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
    let file_name = to_path_str(vm, globals, lfp.arg(0))?;
    if Path::new(&file_name).is_absolute() {
        return Ok(Value::string(file_name));
    }
    let base = if let Some(arg1) = lfp.try_arg(1)
        && !arg1.is_nil()
    {
        std::path::PathBuf::from(to_path_str(vm, globals, arg1)?)
    } else {
        match std::env::current_dir() {
            Ok(dir) => dir,
            Err(err) => return Err(MonorubyErr::errno_with_msg(&globals.store, &err, ".")),
        }
    };
    let mut result = base;
    result.push(&file_name);
    Ok(Value::string(conv_pathbuf(&result)))
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
    let file_name = lfp.arg(0).coerce_to_string(vm, globals)?;
    Ok(Value::bool(file_name.starts_with('/')))
}

///
/// ### File.split
/// - split(pathname) -> [dirname, basename]
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/split.html]
#[monoruby_builtin]
fn file_split(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let mut dir = match filename.parent() {
        Some(ostr) => conv_pathbuf(ostr),
        None => "".to_string(),
    };
    if dir.is_empty() {
        dir = ".".to_string();
    }
    let base = match filename.file_name() {
        Some(ostr) => ostr.to_string_lossy().to_string(),
        None => {
            if filename.as_os_str() == "/" {
                "/".to_string()
            } else {
                "".to_string()
            }
        }
    };
    Ok(Value::array2(
        Value::string(dir),
        Value::string(base),
    ))
}

// Utils

fn extend(path: &mut std::path::PathBuf, extend: std::path::PathBuf) -> Result<()> {
    for elem in extend.components() {
        match elem {
            std::path::Component::CurDir => {}
            std::path::Component::Normal(comp) if comp == "~" => {
                path.clear();
                let home_dir = match dirs::home_dir() {
                    Some(dir) => dir,
                    None => {
                        return Err(MonorubyErr::runtimeerr("Failed to get home directory."));
                    }
                };
                path.push(home_dir);
            }
            std::path::Component::Normal(comp) => path.push(comp),
            std::path::Component::ParentDir => {
                path.pop();
            }
            std::path::Component::RootDir => {
                path.clear();
                path.push(std::path::Component::RootDir);
            }
            _ => {}
        };
    }
    Ok(())
}

/// Convert `file` to canonicalized PathBuf.
fn to_canonicalized_path(
    vm: &mut Executor,
    globals: &mut Globals,
    file: Value,
    msg: &str,
) -> Result<std::path::PathBuf> {
    let path = to_path(vm, globals, file)?;
    match path.canonicalize() {
        Ok(file) => Ok(file),
        Err(_) => Err(MonorubyErr::argumenterr(format!(
            "{} is an invalid filename. {:?}",
            msg, path
        ))),
    }
}

/// Convert `file` to PathBuf.
fn to_path(vm: &mut Executor, globals: &mut Globals, file: Value) -> Result<std::path::PathBuf> {
    let file = to_path_str(vm, globals, file)?;
    let mut path = std::path::PathBuf::new();
    for p in std::path::PathBuf::from(file).iter() {
        if p == ".." && path.file_name().is_some() {
            path.pop();
        } else {
            path.push(p);
        };
    }
    Ok(path)
}

fn to_path_str(vm: &mut Executor, globals: &mut Globals, val: Value) -> Result<String> {
    Ok(val
        .coerce_to_path_rstring(vm, globals)?
        .to_str()?
        .to_string())
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
        let path = arg.coerce_to_str(vm, globals)?;
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
    let mode = args[0].coerce_to_int(_vm, globals)? as u32;
    let mut count = 0i64;
    for arg in args[1..].iter() {
        let path = arg.coerce_to_str(_vm, globals)?;
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(mode))
            .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_chmod", &path))?;
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
    let old = lfp.arg(0).coerce_to_str(vm, globals)?;
    let new = lfp.arg(1).coerce_to_str(vm, globals)?;
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
fn readlines(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let content = std::fs::read_to_string(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    let lines: Vec<Value> = content
        .lines()
        .map(|l| Value::string(format!("{}\n", l)))
        .collect();
    Ok(Value::array_from_vec(lines))
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
    let path = to_path(vm, globals, lfp.arg(0))?;
    let path_str = path.to_string_lossy();
    let metadata = std::fs::metadata(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_size", &path_str))?;
    Ok(Value::integer(metadata.len() as i64))
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
    let path = to_path(vm, globals, lfp.arg(0))?;
    match std::fs::metadata(&path) {
        Ok(metadata) => {
            let size = metadata.len();
            if size == 0 {
                Ok(Value::nil())
            } else {
                Ok(Value::integer(size as i64))
            }
        }
        Err(_) => Ok(Value::nil()),
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
fn file_instance_size(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let io = self_.as_io_inner();
    match io.name() {
        Some(name) => {
            let metadata = std::fs::metadata(name)
                .map_err(|e| MonorubyErr::runtimeerr(format!("{}: {}", name, e)))?;
            Ok(Value::integer(metadata.len() as i64))
        }
        None => Err(MonorubyErr::runtimeerr("size not available for this IO")),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn join() {
        run_test(r##"File.join("a","b")"##);
        run_test(r##"File.join("a/","b")"##);
        run_test(r##"File.join("a/","/b")"##);
        run_test(r##"File.join("a","/b")"##);
        run_test(r##"File.join("a",["b",["c",["d"]]])"##);
        run_test(r##"File.join("", "a",["b",["c",["d"]]])"##);
        run_test(r##"File.join("","","","a")"##);
        run_test(r##"File.join([])"##);
        run_test(r##"File.join"##);
    }

    #[test]
    fn expand_path() {
        run_test(r##"File.expand_path("..")"##);
        run_test(r##"File.expand_path("..", "/tmp")"##);
        run_test(r##"File.expand_path("~")"##);
    }

    #[test]
    fn dirname() {
        run_test(r##"File.dirname("dir/file.ext")"##);
        run_test(r##"File.dirname("file.ext")"##);
        run_test(r##"File.dirname("foo/bar/")"##);

        run_test(r##"File.basename("dir/file.ext")"##);
        run_test(r##"File.basename("file.ext")"##);
        run_test(r##"File.basename("foo/bar/")"##);
        run_test(r##"File.basename("")"##);
        run_test(r##"File.basename("/")"##);
        run_test(r##"File.basename("//")"##);
        run_test(r##"File.basename("..")"##);
        run_test(r##"File.basename("/..")"##);
        run_test(r##"File.basename("/../")"##);
        run_test(r##"File.basename("/../.")"##);

        run_test(r##"File.extname("foo/foo.txt")"##);
        run_test(r##"File.extname("foo/foo.tar.gz")"##);
        run_test(r##"File.extname("foo/bar")"##);
        run_test(r##"File.extname("foo/.bar")"##);
        run_test(r##"File.extname("foo.txt/bar")"##);
        run_test(r##"File.extname(".foo")"##);
        run_test(r##"File.extname("foo.")"##);
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
    fn open() {
        run_test(r##"$LOAD_PATH.resolve_feature_path("pp")"##);
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
        run_test(r##"File.fnmatch("cat", "cat")"##);
        run_test(r##"File.fnmatch("cat", "category")"##);
        run_test(r##"File.fnmatch("c*", "cats")"##);
        run_test(r##"File.fnmatch("c?t", "cat")"##);
        run_test(r##"File.fnmatch("c?t", "cot")"##);
        run_test(r##"File.fnmatch("c?t", "ct")"##);
        run_test(r##"File.fnmatch("c[ao]t", "cat")"##);
        run_test(r##"File.fnmatch("c[ao]t", "cot")"##);
        run_test(r##"File.fnmatch("c[ao]t", "cut")"##);
        run_test(r##"File.fnmatch?("cat", "cat")"##);
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
        run_test_once(
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
}
