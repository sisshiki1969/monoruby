extern crate clap;

use clap::*;
use std::fs::*;
use std::io::Read;
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None, trailing_var_arg = true)]
struct Cli {
    /// one line of script. Several -e's allowed. Omit [programfile]
    #[arg(short, num_args(0..))]
    exec: Option<String>,

    /// print the version number, then turn on verbose mode
    #[arg(short)]
    verbose: bool,

    /// program file and arguments
    args: Vec<String>,
}

fn main() {
    let cli = Cli::parse();
    if cli.verbose {
        println!("{} {}", crate_name!(), crate_version!());
    }
    match cli.exec {
        Some(command) => {
            parse_and_output(command);
            return;
        }
        None => {}
    }

    let file = if cli.args.is_empty() {
        parse_and_output("a=3; if a==3 then 0 else 1 end".to_string());
        return;
    } else {
        &cli.args[0]
    };

    let absolute_path = match std::path::Path::new(file).canonicalize() {
        Ok(path) => path,
        Err(ioerr) => {
            eprintln!("{}: {}.", file, ioerr);
            return;
        }
    };

    let program = match load_file(&absolute_path) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{}: {}.", file, err);
            return;
        }
    };

    parse_and_output(program);
}

fn parse_and_output(program: String) {
    match ruruby_parse::Parser::parse_program(program, Path::new("")) {
        Ok(res) => {
            println!("{:#?}", res.node);
            println!("{}", res.source_info.get_location(&ruruby_parse::Loc(0, 0)));
        }
        Err(err) => panic!("{:?}\n{}", err.kind, err.source_info.get_location(&err.loc)),
    };
}

fn load_file(path: &Path) -> Result<String, String> {
    let mut file_body = String::new();
    match OpenOptions::new().read(true).open(path) {
        Ok(mut file) => match file.read_to_string(&mut file_body) {
            Ok(_) => {}
            Err(ioerr) => return Err(format!("{}", ioerr)),
        },
        Err(ioerr) => return Err(format!("{}", ioerr)),
    };
    Ok(file_body)
}

#[cfg(test)]
mod tests {
    use super::*;
    use reqwest::StatusCode;

    fn fetch_file(url: &str) -> String {
        let res = match reqwest::blocking::get(url) {
            Ok(res) => res,
            Err(err) => panic!("{:?}", err),
        };
        if res.status() != StatusCode::OK {
            panic!("{:?}", res.status());
        };
        res.text().unwrap()
    }

    #[test]
    fn yamanote() {
        let code = fetch_file(
            "https://raw.githubusercontent.com/mame/yamanote-quine/master/yamanote-quine-inner-circle.rb",
        );
        parse_and_output(code);
        let code = fetch_file(
            "https://raw.githubusercontent.com/mame/yamanote-quine/master/yamanote-quine-outer-circle.rb",
        );
        parse_and_output(code);
    }

    #[test]
    fn aobench() {
        let code = fetch_file(
            "https://raw.githubusercontent.com/ruby/ruby/master/benchmark/app_aobench.rb",
        );
        parse_and_output(code);
    }

    #[test]
    fn optcarrot() {
        let code = fetch_file(
            "https://raw.githubusercontent.com/mame/optcarrot/master/lib/optcarrot/ppu.rb",
        );
        parse_and_output(code);
    }

    #[test]
    fn samples() {
        fn read_dir<P: AsRef<Path>>(
            path: P,
            files: &mut Vec<std::path::PathBuf>,
        ) -> std::io::Result<()> {
            for entry in std::fs::read_dir(path)?.filter_map(|entry| entry.ok()) {
                if entry.file_type()?.is_file() && entry.path().extension() == Some("rb".as_ref()) {
                    files.push(entry.path());
                } else if entry.file_type().unwrap().is_dir() {
                    read_dir(entry.path(), files)?;
                }
            }
            Ok(())
        }

        let mut total: usize = 0;
        let mut read: usize = 0;
        let mut accepted: usize = 0;
        let mut files = vec![];
        read_dir("../.samples", &mut files).unwrap();
        for f in files {
            total += 1;
            let f = f.canonicalize().unwrap().to_string_lossy().to_string();
            //println!("testing {f}");
            let code = match std::fs::read_to_string(f.clone()) {
                Ok(code) => code,
                Err(err) => {
                    println!("{}: {err}", f);
                    continue;
                }
            };
            read += 1;
            match ruruby_parse::Parser::parse_program(code, f.clone()) {
                Ok(_) => {
                    accepted += 1;
                }
                Err(err) => {
                    println!("{:?}", err.kind);
                    println!("{}", err.source_info.get_location(&err.loc));
                }
            };
        }
        println!();
        println!("total:{total} read:{read} accepted:{accepted}");
    }
}
