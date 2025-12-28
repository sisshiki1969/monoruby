use super::*;
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
    globals.define_builtin_class_func(file, "read", file_read, 1);
    globals.define_builtin_class_func_with(file, "binread", binread, 1, 3, false);
    globals.define_builtin_class_func_rest(file, "join", join);
    globals.define_builtin_class_func_with(file, "expand_path", expand_path, 1, 2, false);
    globals.define_builtin_class_func(file, "directory?", directory_, 1);
    globals.define_builtin_class_func(file, "symlink?", symlink_, 1);
    globals.define_builtin_class_func(file, "dirname", dirname, 1);
    globals.define_builtin_class_func_with(file, "basename", basename, 1, 2, false);
    globals.define_builtin_class_func(file, "extname", extname, 1);
    globals.define_builtin_class_func(file, "exist?", exist, 1);
    globals.define_builtin_module_func(file_test, "exist?", exist, 1);
    globals.define_builtin_class_func(file, "file?", file_, 1);
    globals.define_builtin_module_func(file_test, "file?", file_, 1);
    globals.define_builtin_class_func(file, "path", path, 1);
    globals.define_builtin_class_func_with(file, "realpath", realpath, 1, 2, false);
    globals.define_builtin_class_func_with(file, "open", open, 1, 3, false);

    globals.define_builtin_func_rest(file, "write", write);
}

///
/// ### File.write
/// - write(path, string, opt={}) -> Integer
/// - [NOT SUPPORTED] write(path, string, offset=nil, opt={}) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/write.html]
#[monoruby_builtin]
fn file_write(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.arg(0);
    let name = self_.expect_str(globals)?;
    let mut file = match File::create(name) {
        Ok(file) => file,
        Err(err) => return Err(MonorubyErr::runtimeerr(format!("{}: {:?}", name, err))),
    };
    let val = lfp.arg(1);
    let len = if let Some(s) = val.is_rstring() {
        if let Err(err) = file.write_all(&s) {
            return Err(MonorubyErr::runtimeerr(err));
        };
        s.len()
    } else {
        let v = val.to_s(&globals.store).into_bytes();
        if let Err(err) = file.write_all(&v) {
            return Err(MonorubyErr::runtimeerr(err));
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
fn file_read(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(_) => {
            return Err(MonorubyErr::runtimeerr(format!(
                "Can not open file. {:?}",
                &filename
            )));
        }
    };
    let mut contents = String::new();
    match std::io::Read::read_to_string(&mut file, &mut contents) {
        Ok(file) => file,
        Err(_) => {
            return Err(MonorubyErr::runtimeerr("Could not read the file."));
        }
    };
    Ok(Value::string(contents))
}

///
/// ### IO.binread
///
/// - binread(path, length = nil, offset = 0) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/binread.html]
#[monoruby_builtin]
fn binread(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let filename = to_path(vm, globals, lfp.arg(0))?;
    let length = if let Some(arg1) = lfp.try_arg(1) {
        Some(arg1.coerce_to_i64(globals)?)
    } else {
        None
    };
    let offset = if let Some(arg2) = lfp.try_arg(2) {
        Some(arg2.coerce_to_i64(globals)?)
    } else {
        None
    };
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(_) => {
            return Err(MonorubyErr::runtimeerr(format!(
                "Can not open file. {:?}",
                &filename
            )));
        }
    };
    if let Some(offset) = offset {
        match file.seek(SeekFrom::Start(offset as _)) {
            Ok(_) => {}
            Err(err) => return Err(MonorubyErr::cant_load(Some(err), &filename)),
        };
    }
    if let Some(length) = length {
        let mut contents = vec![0; length as usize];
        if std::io::Read::read_exact(&mut file, &mut contents).is_err() {
            return Err(MonorubyErr::runtimeerr("Could not read the file."));
        };
        Ok(Value::bytes(contents))
    } else {
        let mut contents = vec![];
        if std::io::Read::read_to_end(&mut file, &mut contents).is_err() {
            return Err(MonorubyErr::runtimeerr("Could not read the file."));
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
fn join(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn flatten(globals: &Globals, path: &mut String, val: Value) -> Result<()> {
        match val.try_array_ty() {
            Some(ainfo) => {
                for v in ainfo.iter().cloned() {
                    flatten(globals, path, v)?;
                }
            }
            None => {
                if !path.is_empty() && !path.ends_with('/') {
                    path.push('/');
                }
                let s = val.expect_str(globals)?;
                path.push_str(if !path.is_empty() && !s.is_empty() && s.starts_with('/') {
                    &s[1..]
                } else if path.is_empty() && s.is_empty() {
                    "/"
                } else {
                    s
                });
            }
        }
        Ok(())
    }
    let mut path = String::new();
    for v in lfp.arg(0).as_array().iter().cloned() {
        flatten(globals, &mut path, v)?;
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
fn expand_path(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(err) => {
            return Err(MonorubyErr::runtimeerr(err));
        }
    };
    let arg0 = to_path(vm, globals, lfp.arg(0))?;
    let path = if let Some(arg1) = lfp.try_arg(1) {
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
fn dirname(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn basename(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let filename = lfp.arg(0).coerce_to_path_rstring(vm, globals)?;
    let suffix = if let Some(arg1) = lfp.try_arg(1) {
        let s = arg1.expect_str(globals)?;
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
fn directory_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn symlink_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn extname(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn exist(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let b = to_canonicalized_path(vm, globals, lfp.arg(0), "1st arg").is_ok();
    Ok(Value::bool(b))
}

///
/// ### File.file?
/// - file?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/file=3f.html]
#[monoruby_builtin]
fn file_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    match to_canonicalized_path(vm, globals, lfp.arg(0), "1st arg") {
        Ok(path) => Ok(Value::bool(path.is_file())),
        Err(_) => Ok(Value::bool(false)),
    }
}

///
/// ### File.path
/// - path(filename) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/path.html]
#[monoruby_builtin]
fn path(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::string(to_path_str(vm, globals, lfp.arg(0))?))
}

///
/// ### File.realpath
/// - realpath(pathname, basedir = nil) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/realpath.html]
#[monoruby_builtin]
fn realpath(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut pathname = if let Some(arg1) = lfp.try_arg(1) {
        let path = std::path::PathBuf::from(arg1.expect_string(globals)?);
        match path.canonicalize() {
            Ok(path) => path,
            Err(err) => {
                return Err(MonorubyErr::argumenterr(format!(
                    "{}:{}",
                    arg1.as_str(),
                    err
                )));
            }
        }
    } else {
        match std::env::current_dir() {
            Ok(path) => path,
            Err(_) => {
                return Err(MonorubyErr::runtimeerr("Failed to get current directory."));
            }
        }
    };
    pathname.push(std::path::PathBuf::from(lfp.arg(0).expect_string(globals)?));
    match pathname.canonicalize() {
        Ok(file) => Ok(Value::string(file.to_string_lossy().to_string())),
        Err(err) => Err(MonorubyErr::argumenterr(format!(
            "{}:{}",
            pathname.to_string_lossy(),
            err
        ))),
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
fn open(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mode = if let Some(arg1) = lfp.try_arg(1) {
        arg1.expect_string(globals)?
    } else {
        "r".to_string()
    };
    let mut opt = File::options();
    let opt = match mode.split(':').next().unwrap() {
        "r" | "rb" => opt.read(true),
        "w" | "wb" => opt.write(true).create(true).truncate(true),
        "a" | "ab" => opt.write(true).create(true).append(true),
        "r+" | "r+b" => opt.read(true).write(true),
        "w+" | "w+b" => opt.read(true).write(true).create(true).truncate(true),
        "a+" | "a+b" => opt.read(true).write(true).create(true).append(true),
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
            return Err(MonorubyErr::runtimeerr(format!("{}: {:?}", path, err)));
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
fn write(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let data = lfp.arg(0).as_array();
    let mut self_ = lfp.self_val();
    let mut count = 0i64;
    for s in data.iter() {
        let s = s.expect_bytes(globals)?;
        count += s.len() as i64;
        self_.as_io_inner_mut().write(s)?;
    }
    Ok(Value::integer(count))
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
}
