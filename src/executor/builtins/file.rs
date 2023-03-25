use crate::*;
use std::{fs::File, io::Write};

//
// File class
//

pub(super) fn init(globals: &mut Globals, class_id: ClassId) {
    globals.define_builtin_class_func(class_id, "write", write, 2);
    globals.define_builtin_class_func(class_id, "expand_path", expand_path, -1);
    globals.define_builtin_class_func(class_id, "dirname", dirname, 1);
}

///
/// ### File.write
/// - write(path, string, opt={}) -> Integer
/// - write(path, string, offset=nil, opt={}) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/write.html]
extern "C" fn write(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let name = match arg[0].unpack() {
        RV::String(bytes) => String::from_utf8(bytes.to_vec()).unwrap(),
        _ => {
            globals.err_no_implicit_conversion(arg[0], STRING_CLASS);
            return None;
        }
    };
    let mut file = File::create(name).unwrap();
    let bytes = arg[1].to_s(globals).into_bytes();
    file.write_all(&bytes).unwrap();
    Some(Value::new_integer(bytes.len() as i64))
}

///
/// ### File.expand_path
/// - expand_path(path, default_dir = '.') -> String
/// TODO: support ~USER
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/expand_path.html]
extern "C" fn expand_path(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 1..=2)?;
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(err) => {
            globals.err_runtime(err.to_string());
            return None;
        }
    };
    let home_dir = match dirs::home_dir() {
        Some(dir) => dir,
        None => {
            globals.err_runtime("Failed to get home directory.".to_string());
            return None;
        }
    };
    let path = if len == 1 {
        string_to_path(globals, arg[0])?
    } else {
        let mut path = string_to_path(globals, arg[1])?;
        let rel_path = string_to_path(globals, arg[0])?;
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

    Some(Value::new_string(conv_pathbuf(&res_path)))
}

///
/// ### File.dirname
/// - dirname(filename, level=1) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/File/s/dirname.html]
extern "C" fn dirname(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Option<Value> {
    let filename = string_to_path(globals, arg[0])?;
    let mut dirname = match filename.parent() {
        Some(ostr) => conv_pathbuf(&ostr.to_path_buf()),
        None => "".to_string(),
    };
    if dirname.is_empty() {
        dirname += "."
    };
    Some(Value::new_string(dirname))
}

// Utils

/// Convert Ruby String value`string` to PathBuf.
fn string_to_path(globals: &mut Globals, string: Value) -> Option<std::path::PathBuf> {
    let file = string.expect_string(globals)?;
    let mut path = std::path::PathBuf::new();
    for p in std::path::PathBuf::from(file).iter() {
        if p == ".." && path.file_name().is_some() {
            path.pop();
        } else {
            path.push(p);
        };
    }
    Some(path)
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
    fn test_expand_path() {
        run_test(r##"File.expand_path("..")"##);
        run_test(r##"File.expand_path("..", "/tmp")"##);
        run_test(r##"File.expand_path("~")"##);
    }

    #[test]
    fn test_dirname() {
        run_test(r##"File.dirname("dir/file.ext")"##);
        run_test(r##"File.dirname("file.ext")"##);
        run_test(r##"File.dirname("foo/bar/")"##);
    }
}
