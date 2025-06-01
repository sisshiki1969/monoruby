use std::path::PathBuf;

use super::*;

//
// Dir class
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_class_under_obj("Dir").id();
    globals.define_builtin_class_func_with_kw(klass, "glob", glob, 1, 2, false, &["base", "sort"]);
    globals.define_builtin_class_func(klass, "home", home, 0);
    globals.define_builtin_class_funcs(klass, "pwd", &["getwd"], pwd, 0);
    globals.define_builtin_class_func_with(klass, "chdir", chdir, 0, 1, false);
}

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
    Name(globset::Glob),
    Parent,
    Current,
    None,
}

///
/// ### Dir.[]
///
/// - glob(pattern, [NOT SUPPORTED] flags = 0, base: nil, [NOT SUPPORTED] sort: true) -> [String]
/// - glob(pattern, [NOT SUPPORTED] flags = 0, base: nil, [NOT SUPPORTED] sort: true) {|file| ...} -> nil
///
/// #### Arguments
///
/// 0: pattern
/// 1: flags
/// 2: base
/// 3: sort
///
/// [https://docs.ruby-lang.org/ja/latest/method/Dir/s/=5b=5d.html]
#[monoruby_builtin]
fn glob(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let pat_val = lfp.arg(0);
    let base = if let Some(base) = lfp.try_arg(2) {
        if base.is_nil() {
            None
        } else {
            Some(base.expect_string(globals)?)
        }
    } else {
        None
    };
    let mut pattern = pat_val
        .expect_str(globals)?
        .split(std::path::MAIN_SEPARATOR_STR)
        .peekable();
    let path = if pattern.peek() == Some(&"") {
        pattern.next();
        if pattern.peek().is_none() {
            return Ok(Value::array_empty());
        }
        PathPair::new(PathBuf::from("/"), PathBuf::from("/"))
    } else if let Some(base) = base {
        let mut path = std::env::current_dir().unwrap();
        path.push(base);
        let path = match path.canonicalize() {
            Ok(path) => path,
            Err(_) => return Ok(Value::array_empty()),
        };
        PathPair::new(path, PathBuf::new())
    } else {
        PathPair::new(std::env::current_dir().unwrap(), PathBuf::new())
    };

    let glob: Vec<_> = pattern
        .map(|s| match s {
            "." => PathComponent::Current,
            ".." => PathComponent::Parent,
            "" => PathComponent::None,
            s => PathComponent::Name(globset::Glob::new(s).unwrap()),
        })
        .collect();

    let mut matches = vec![];
    traverse_dir(path, glob, &mut matches)?;
    matches.sort();
    Ok(Value::array_from_iter(
        matches.into_iter().map(Value::string),
    ))
}

fn traverse_dir(
    mut path: PathPair,
    mut glob_rest: Vec<PathComponent>,
    matches: &mut Vec<String>,
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
            PathComponent::Name(glob) => {
                let glob = glob.compile_matcher();
                for entry in std::fs::read_dir(&path.path).unwrap() {
                    let entry = entry.unwrap();
                    let name = entry.file_name().to_string_lossy().to_string();
                    if name.starts_with('.') && !glob.glob().glob().starts_with('.') {
                        continue;
                    }
                    if glob.is_match_candidate(&globset::Candidate::new(&name)) {
                        let mut path = path.clone();
                        path.push(&name);
                        traverse_dir(path, glob_rest.clone(), matches)?;
                    }
                }
                return Ok(());
            }
            PathComponent::Parent => {
                path.parent();
            }
            PathComponent::Current => {
                path.current();
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
fn home(_: &mut Executor, _: &mut Globals, _: Lfp) -> Result<Value> {
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
fn pwd(_: &mut Executor, _: &mut Globals, _: Lfp) -> Result<Value> {
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
fn chdir(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
        std::env::set_current_dir(old_pwd).unwrap();
        res
    } else {
        std::env::set_current_dir(path).unwrap();
        Ok(Value::integer(0))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn glob() {
        run_test(r#"Dir.glob("LIC*")"#);
        run_test(r#"Dir.glob("*.rb")"#);
        run_test(r#"Dir.glob("Cargo?????")"#);
        run_test(r#"Dir.glob("d{a,c}*")"#);
        run_test(r#"Dir.glob("/*")"#);
        run_test(r#"Dir.glob("././././C*")"#);
        run_test(r#"Dir.glob("../../../../*")"#);
        run_test(r#"Dir.glob("../*")"#);
        run_test(r#"Dir.glob("monoruby/src/builtins/*.rs")"#);
        run_test(r#"Dir.glob("monoruby/src/**/*.rs")"#);
        run_test(r#"Dir.glob("/")"#);
        run_test(r#"Dir.glob(".")"#);
        run_test(r#"Dir.glob("")"#);
        run_test(r#"Dir.glob("*", base: "src/builtins")"#);
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
        "##,
        );
    }
}
