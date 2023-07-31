use crate::*;
use std::{fs::File, io::Write};

//
// File class
//

pub(super) fn init(globals: &mut Globals) {
    let io_class = globals
        .get_constant(OBJECT_CLASS, IdentId::get_id("IO"))
        .unwrap()
        .as_class();
    let klass = globals
        .define_class_by_str("File", io_class, OBJECT_CLASS)
        .id();
    globals.define_builtin_class_func(klass, "write", write, 2);
    globals.define_builtin_class_func(klass, "read", read, 1);
    globals.define_builtin_class_func(klass, "join", join, -1);
    globals.define_builtin_class_func(klass, "expand_path", expand_path, -1);
    globals.define_builtin_class_func(klass, "dirname", dirname, 1);
    globals.define_builtin_class_func(klass, "exist?", exist, 1);
}

///
/// ### File.write
/// - write(path, string, opt={}) -> Integer
/// - write(path, string, offset=nil, opt={}) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/write.html]
#[monoruby_builtin]
fn write(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let name = match arg[0].unpack() {
        RV::String(bytes) => String::from_utf8(bytes.to_vec()).unwrap(),
        _ => {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                arg[0],
                STRING_CLASS,
            ));
        }
    };
    let mut file = match File::create(&name) {
        Ok(file) => file,
        Err(err) => return Err(MonorubyErr::runtimeerr(format!("{}: {:?}", name, err))),
    };
    let bytes = globals.to_s(arg[1]).into_bytes();
    match file.write_all(&bytes) {
        Ok(_) => {}
        Err(err) => return Err(MonorubyErr::runtimeerr(err)),
    };
    Ok(Value::integer(bytes.len() as i64))
}

///
/// ### IO.read
/// - read(path, [NOT SUPPORTED]**opt) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/read.html]
#[monoruby_builtin]
fn read(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments(len, 1)?;
    let filename = string_to_path(arg[0], globals)?;
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
/// ### File.join
/// - join(*item) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/join.html]
#[monoruby_builtin]
fn join(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    fn flatten(
        vm: &mut Executor,
        globals: &mut Globals,
        path: &mut String,
        val: Value,
    ) -> Result<()> {
        match val.is_array() {
            Some(ainfo) => {
                for v in ainfo.iter().cloned() {
                    flatten(vm, globals, path, v)?;
                }
            }
            None => {
                if !path.is_empty() && !path.ends_with('/') {
                    path.push('/');
                }
                let s = val.expect_string(globals)?;
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
    let len = lfp.arg_len();
    let mut path = String::new();
    for i in 0..len {
        flatten(vm, globals, &mut path, arg[i])?;
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
fn expand_path(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 1..=2)?;
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(err) => {
            return Err(MonorubyErr::runtimeerr(err));
        }
    };
    let home_dir = match dirs::home_dir() {
        Some(dir) => dir,
        None => {
            return Err(MonorubyErr::runtimeerr("Failed to get home directory."));
        }
    };
    let path = if len == 1 {
        string_to_path(arg[0], globals)?
    } else {
        let mut path = string_to_path(arg[1], globals)?;
        let rel_path = string_to_path(arg[0], globals)?;
        path.push(rel_path);
        path
    };

    let mut res_path = std::path::PathBuf::new();
    res_path.push(current_dir);

    for elem in path.components() {
        match elem {
            std::path::Component::CurDir => {}
            std::path::Component::Normal(comp) if comp == "~" => {
                res_path.clear();
                res_path.push(home_dir.clone());
            }
            std::path::Component::Normal(comp) => res_path.push(comp),
            std::path::Component::ParentDir => {
                res_path.pop();
            }
            std::path::Component::RootDir => {
                res_path.clear();
                res_path.push(std::path::Component::RootDir);
            }
            _ => {}
        };
    }
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
/// - dirname(filename, level=1) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/dirname.html]
#[monoruby_builtin]
fn dirname(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let filename = string_to_path(arg[0], globals)?;
    let mut dirname = match filename.parent() {
        Some(ostr) => conv_pathbuf(&ostr.to_path_buf()),
        None => "".to_string(),
    };
    if dirname.is_empty() {
        dirname += "."
    };
    Ok(Value::string(dirname))
}

///
/// ### File.exist?
/// - exist?(path) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/exist=3f.html]
#[monoruby_builtin]
fn exist(_vm: &mut Executor, globals: &mut Globals, _lfp: LFP, arg: Arg) -> Result<Value> {
    let b = string_to_canonicalized_path(globals, arg[0], "1st arg").is_ok();
    Ok(Value::bool(b))
}

// Utils

/// Convert `file` to canonicalized PathBuf.
fn string_to_canonicalized_path(
    globals: &mut Globals,
    file: Value,
    msg: &str,
) -> Result<std::path::PathBuf> {
    let path = string_to_path(file, globals)?;
    match path.canonicalize() {
        Ok(file) => Ok(file),
        Err(_) => Err(MonorubyErr::argumenterr(format!(
            "{} is an invalid filename. {:?}",
            msg, path
        ))),
    }
}

/// Convert `file` to PathBuf.
fn string_to_path(file: Value, globals: &mut Globals) -> Result<std::path::PathBuf> {
    let file = file.expect_string(globals)?;
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

#[cfg(not(windows))]
fn conv_pathbuf(dir: &std::path::PathBuf) -> String {
    dir.to_string_lossy().to_string()
}
#[cfg(windows)]
fn conv_pathbuf(dir: &std::path::PathBuf) -> String {
    dir.to_string_lossy()
        .replace("\\\\?\\", "")
        .replace('\\', "/")
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
    }

    #[test]
    fn read() {
        run_test(r##"File.read("../LICENSE-MIT")"##);
        run_test(r##"File.exist?("../LICENSE-MIT")"##);
        run_test(r##"File.exist?("../LICENCE-MIT")"##);
    }
}
