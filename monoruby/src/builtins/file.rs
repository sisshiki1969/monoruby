use super::*;
use std::path::Path;
use std::{
    fs::File,
    io::{Seek, SeekFrom, Write},
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
    globals.define_builtin_class_func(file, "write", file_write, 2);
    globals.define_builtin_class_func_with(file, "binwrite", file_binwrite, 2, 3, false);
    globals.define_builtin_class_func_with(file, "read", file_read, 1, 4, false);
    globals.define_builtin_class_func_with(file, "binread", file_binread, 1, 3, false);

    // IO class methods that share semantics with File.* class methods.
    globals.define_builtin_class_func(IO_CLASS, "write", file_write, 2);
    globals.define_builtin_class_func_with(IO_CLASS, "binwrite", file_binwrite, 2, 3, false);
    globals.define_builtin_class_func_with(IO_CLASS, "binread", file_binread, 1, 3, false);
    globals.define_builtin_class_func(IO_CLASS, "try_convert", io_try_convert, 1);
    globals.define_builtin_class_func_rest(file, "join", file_join);
    globals.define_builtin_class_func_with(file, "expand_path", file_expand_path, 1, 2, false);
    globals.define_builtin_class_func_with(file, "dirname", file_dirname, 1, 2, false);
    globals.define_builtin_class_func_with(file, "basename", file_basename, 1, 2, false);
    globals.define_builtin_class_func(file, "extname", file_extname, 1);
    globals.define_builtin_class_func(file, "path", file_path, 1);
    globals.define_builtin_class_func_with(file, "realpath", realpath, 1, 2, false);
    globals.define_builtin_class_func_with(file, "open", open, 1, 4, false);
    globals.define_builtin_class_func_with(file, "new", open, 1, 4, false);
    globals.define_builtin_class_func_with(IO_CLASS, "open", open, 1, 4, false);

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

    globals.define_builtin_func_rest(file, "write", write);
    globals.define_builtin_funcs(file, "path", &["to_path"], path, 0);
    globals.define_builtin_func(file, "size", size, 0);
    globals.define_builtin_func(file, "flock", flock_, 1);

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
fn file_write(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let name = lfp.arg(0).coerce_to_string(vm, globals)?;
    let mut file = match File::create(&name) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &name,
            ));
        }
    };
    let val = lfp.arg(1);
    let len = if let Some(s) = val.is_rstring() {
        if let Err(err) = file.write_all(&s) {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_io_write",
                &name,
            ));
        };
        s.len()
    } else {
        let v = val.to_s(&globals.store).into_bytes();
        if let Err(err) = file.write_all(&v) {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_io_write",
                &name,
            ));
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
                &filename_str,
            ));
        }
    };
    let mut contents = vec![];
    match std::io::Read::read_to_end(&mut file, &mut contents) {
        Ok(_) => {}
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_io_read",
                &filename_str,
            ));
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
            return Err(MonorubyErr::argumenterr(format!("negative offset {}", n)));
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
                &filename_str,
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
/// ### IO.binwrite / File.binwrite
/// - binwrite(path, string, offset = nil) -> Integer
///
/// Writes `string` to the file at `path` in binary mode and returns the
/// number of bytes written. With no `offset`, the file is created if missing
/// and truncated to the length of `string`. With an `offset`, the file is
/// created if missing but **not** truncated; bytes are written starting at
/// `offset`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/binwrite.html]
#[monoruby_builtin]
fn file_binwrite(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let name = lfp.arg(0).coerce_to_string(vm, globals)?;
    let val = lfp.arg(1);
    let bytes = if let Some(s) = val.is_rstring() {
        s.to_vec()
    } else {
        val.to_s(&globals.store).into_bytes()
    };
    let offset = if let Some(arg2) = lfp.try_arg(2)
        && !arg2.is_nil()
    {
        Some(arg2.coerce_to_int_i64(vm, globals)?)
    } else {
        None
    };
    let mut file = match offset {
        None => match File::create(&name) {
            Ok(f) => f,
            Err(err) => {
                return Err(MonorubyErr::errno_with_path(
                    &globals.store,
                    &err,
                    "rb_sysopen",
                    &name,
                ));
            }
        },
        Some(off) => {
            let mut f = match std::fs::OpenOptions::new()
                .write(true)
                .create(true)
                .open(&name)
            {
                Ok(f) => f,
                Err(err) => {
                    return Err(MonorubyErr::errno_with_path(
                        &globals.store,
                        &err,
                        "rb_sysopen",
                        &name,
                    ));
                }
            };
            if let Err(err) = f.seek(SeekFrom::Start(off as u64)) {
                return Err(MonorubyErr::errno_with_path(
                    &globals.store,
                    &err,
                    "rb_io_seek",
                    &name,
                ));
            }
            f
        }
    };
    if let Err(err) = file.write_all(&bytes) {
        return Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "rb_io_write",
            &name,
        ));
    }
    Ok(Value::integer(bytes.len() as i64))
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
fn file_expand_path(
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
fn directory_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
fn flock_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let op = lfp.arg(0).coerce_to_i64(&_globals.store)? as i32;
    let self_ = lfp.self_val();
    let fd = self_.as_io_inner().fileno()?;
    let r = unsafe { libc::flock(fd, op) };
    if r == 0 {
        return Ok(Value::integer(0));
    }
    let errno = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
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
                return Err(MonorubyErr::errno_with_path(
                    &globals.store,
                    &err,
                    "rb_file_s_realpath",
                    &path_str,
                ));
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
    pathname.push(std::path::PathBuf::from(
        lfp.arg(0).coerce_to_string(vm, globals)?,
    ));
    let pathname_str = pathname.to_string_lossy().to_string();
    match pathname.canonicalize() {
        Ok(file) => Ok(Value::string(file.to_string_lossy().to_string())),
        Err(err) => Err(MonorubyErr::errno_with_path(
            &globals.store,
            &err,
            "rb_file_s_realpath",
            &pathname_str,
        )),
    }
}

///
/// ### File.open
///
/// - open(path, mode = "r", [NOT SUPPORTED] perm = 0666) -> File
/// - open(path, mode = "r", [NOT SUPPORTED] perm = 0666) {|file| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/new.html]
/// Translate a CRuby-style flags integer (e.g. `File::WRONLY | File::CREAT`)
/// into the mode string monoruby's open path understands. The bits map to:
/// access mode in the low 2 bits (RDONLY=0/WRONLY=1/RDWR=2), `O_CREAT`=0o100,
/// `O_TRUNC`=0o1000, `O_APPEND`=0o2000. Other flags (EXCL, NONBLOCK, …) pass
/// through silently — `open` handles their effect via mode-string semantics.
fn mode_string_from_flags(flags: i64) -> String {
    const O_CREAT: i64 = 0o100;
    const O_TRUNC: i64 = 0o1000;
    const O_APPEND: i64 = 0o2000;
    let access = flags & 0b11;
    let create = flags & O_CREAT != 0;
    let trunc = flags & O_TRUNC != 0;
    let append = flags & O_APPEND != 0;
    match access {
        // RDONLY
        0 => "r".to_string(),
        // WRONLY
        1 => {
            if append {
                "a".to_string()
            } else if trunc || create {
                "w".to_string()
            } else {
                "w".to_string()
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

#[monoruby_builtin]
fn open(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
        let name = format!("fd {}", fd);
        // SAFETY: fd has been validated as a valid file descriptor above.
        let io_inner = IoInner::from_raw_fd(fd_i32, name);
        let mut res = Value::new_io_with_class(io_inner, FILE_CLASS);
        if let Some(bh) = lfp.block() {
            let r = vm.invoke_block_once(globals, bh, &[res]);
            // Match CRuby File.open(...) {|io| ... }: close at block exit.
            // Holding the underlying fd open across blocks defeats `flock`
            // (rubygems' open_with_flock relies on this) and leaks fds.
            let _ = res.as_io_inner_mut().close();
            return r;
        }
        return Ok(res);
    }

    // Resolve the open mode. Each later position overrides the earlier one
    // when it provides an explicit `:mode` (matching CRuby's option-Hash
    // precedence): trailing options Hash > explicit mode arg > default "r".
    let mut mode = "r".to_string();
    if let Some(arg1) = lfp.try_arg(1) {
        if arg1.is_nil() {
            // explicit nil => keep default
        } else if let Some(n) = arg1.try_fixnum() {
            mode = mode_string_from_flags(n);
        } else if arg1.is_rstring().is_some() {
            mode = arg1.coerce_to_string(vm, globals)?;
        } else if let Some(h) = arg1.try_hash_ty() {
            if let Some(m) =
                h.get(Value::symbol(IdentId::get_id("mode")), vm, globals)?
                && !m.is_nil()
            {
                if let Some(n) = m.try_fixnum() {
                    mode = mode_string_from_flags(n);
                } else {
                    mode = m.coerce_to_string(vm, globals)?;
                }
            }
        }
    }
    // Look at later args (perm, opts) for a Hash with :mode.
    for i in 2..4 {
        if let Some(arg) = lfp.try_arg(i)
            && let Some(h) = arg.try_hash_ty()
            && let Some(m) = h.get(Value::symbol(IdentId::get_id("mode")), vm, globals)?
            && !m.is_nil()
        {
            if let Some(n) = m.try_fixnum() {
                mode = mode_string_from_flags(n);
            } else {
                mode = m.coerce_to_string(vm, globals)?;
            }
        }
    }
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
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &path,
            ));
        }
    };
    let mut res = Value::new_file(file, path);
    if let Some(bh) = lfp.block() {
        let r = vm.invoke_block_once(globals, bh, &[res]);
        // CRuby File.open(...) {|io| ... } closes the file at block exit.
        let _ = res.as_io_inner_mut().close();
        return r;
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
    let path_str = lfp.arg(1).coerce_to_string(vm, globals)?;
    let flags = if let Some(arg2) = lfp.try_arg(2) {
        arg2.coerce_to_int_i64(vm, globals)? as u32
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
            if pi < pat.len()
                && (pat[pi] == s[si]
                    || pat[pi] == b'\\' && pi + 1 < pat.len() && pat[pi + 1] == s[si])
            {
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
fn file_split(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    Ok(Value::array2(Value::string(dir), Value::string(base)))
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
    let mode = args[0].coerce_to_int_i64(_vm, globals)? as u32;
    let mut count = 0i64;
    for arg in args[1..].iter() {
        let path = arg.coerce_to_str(_vm, globals)?;
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
fn readlines(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
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
    let metadata = std::fs::metadata(&path).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_size", &path_str)
    })?;
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
/// ### File#path / File#to_path
/// - path -> String
/// - to_path -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/i/path.html]
#[monoruby_builtin]
fn path(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let io = self_.as_io_inner();
    match io.name() {
        Some(name) => Ok(Value::string_from_str(name)),
        None => Err(MonorubyErr::runtimeerr("path not available for this IO")),
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
    match io.name() {
        Some(name) => {
            let metadata = std::fs::metadata(name)
                .map_err(|e| MonorubyErr::runtimeerr(format!("{}: {}", name, e)))?;
            Ok(Value::integer(metadata.len() as i64))
        }
        None => Err(MonorubyErr::runtimeerr("size not available for this IO")),
    }
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
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_ftype", &path_str)
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
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_readlink", &path_str)
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
        return Err(MonorubyErr::argumenterr(format!(
            "negative length {}",
            length
        )));
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

/// Convert `val` (a Time, Integer, or Float) to a `libc::timeval`.
fn value_to_timeval(
    vm: &mut Executor,
    globals: &mut Globals,
    val: Value,
) -> Result<libc::timeval> {
    if let Some(rv) = val.try_rvalue()
        && rv.ty() == ObjTy::TIME
    {
        let to_f = IdentId::get_id("to_f");
        let f = vm.invoke_method_inner(globals, to_f, val, &[], None, None)?;
        if let Some(f) = f.try_float() {
            let secs = f.floor() as i64;
            let usec = ((f - f.floor()) * 1_000_000.0) as i64;
            return Ok(libc::timeval {
                tv_sec: secs,
                tv_usec: usec,
            });
        }
    }
    if let Some(f) = val.try_float() {
        let secs = f.floor() as i64;
        let usec = ((f - f.floor()) * 1_000_000.0) as i64;
        return Ok(libc::timeval {
            tv_sec: secs,
            tv_usec: usec,
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
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_stat", &path_str)
    })?;
    let t = f(&metadata).map_err(|e| {
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_time", &path_str)
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
        MonorubyErr::errno_with_path(&globals.store, &e, "rb_file_s_stat", &path_str)
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
    file_time_attr(vm, globals, lfp, |m| m.created())
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
    let path_str = to_path_str(vm, globals, lfp.arg(0))?;
    let mut joined = if Path::new(&path_str).is_absolute() {
        std::path::PathBuf::from(&path_str)
    } else {
        let base = if let Some(arg1) = lfp.try_arg(1)
            && !arg1.is_nil()
        {
            std::path::PathBuf::from(to_path_str(vm, globals, arg1)?)
        } else {
            std::env::current_dir().map_err(|e| {
                MonorubyErr::errno_with_msg(&globals.store, &e, ".")
            })?
        };
        let mut p = base;
        p.push(&path_str);
        p
    };
    // If the full path exists, canonicalize it and we're done.
    if let Ok(canon) = joined.canonicalize() {
        return Ok(Value::string(conv_pathbuf(&canon)));
    }
    // Otherwise canonicalize the parent directory and append the basename.
    let basename = match joined.file_name() {
        Some(n) => n.to_owned(),
        None => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &std::io::Error::from_raw_os_error(libc::ENOENT),
                "rb_file_s_realdirpath",
                &joined.to_string_lossy(),
            ));
        }
    };
    joined.pop();
    let parent = joined.canonicalize().map_err(|e| {
        MonorubyErr::errno_with_path(
            &globals.store,
            &e,
            "rb_file_s_realdirpath",
            &joined.to_string_lossy(),
        )
    })?;
    let mut out = parent;
    out.push(basename);
    Ok(Value::string(conv_pathbuf(&out)))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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
}
