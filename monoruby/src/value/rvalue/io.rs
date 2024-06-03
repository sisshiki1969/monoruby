use std::{
    io::{IsTerminal, Read, Write},
    rc::Rc,
};

use super::*;

#[derive(Debug)]
pub struct FileDescriptor {
    file: std::fs::File,
    name: String,
}

#[derive(Debug)]
pub enum IoInner {
    Stdin(std::io::Stdin),
    Stdout(std::io::Stdout),
    Stderr(std::io::Stderr),
    File(Rc<FileDescriptor>),
}

impl std::clone::Clone for IoInner {
    fn clone(&self) -> Self {
        match self {
            Self::Stdin(_) => Self::Stdin(std::io::stdin()),
            Self::Stdout(_) => Self::Stdout(std::io::stdout()),
            Self::Stderr(_) => Self::Stderr(std::io::stderr()),
            Self::File(file) => Self::File(file.clone()),
        }
    }
}

impl std::fmt::Display for IoInner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Stdin(_) => write!(f, "#<IO:<STDIN>>"),
            Self::Stdout(_) => write!(f, "#<IO:<STDOUT>>"),
            Self::Stderr(_) => write!(f, "#<IO:<STDERR>>"),
            Self::File(file) => write!(f, "#<File:{}>", file.name),
        }
    }
}

impl IoInner {
    pub(super) fn stdin() -> Self {
        Self::Stdin(std::io::stdin())
    }

    pub(super) fn stdout() -> Self {
        Self::Stdout(std::io::stdout())
    }

    pub(super) fn stderr() -> Self {
        Self::Stderr(std::io::stderr())
    }

    pub(super) fn file(file: std::fs::File, name: String) -> Self {
        Self::File(Rc::new(FileDescriptor { file, name }))
    }

    pub fn write(&mut self, data: &[u8]) -> Result<()> {
        match self {
            Self::Stdin(_) => Err(MonorubyErr::argumenterr("can't write to $stdin")),
            Self::Stdout(stdout) => match stdout.write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::Stderr(stderr) => match stderr.write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::File(file) => {
                let file = &mut Rc::get_mut(file).unwrap().file;
                match file.write(data) {
                    Ok(_) => Ok(()),
                    Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
                }
            }
        }
    }

    pub fn read(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
        match self {
            Self::Stdin(stdin) => {
                if let Some(length) = length {
                    let buf = match stdin.bytes().take(length).collect() {
                        Ok(buf) => buf,
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    };
                    Ok(buf)
                } else {
                    let mut buf = vec![];
                    match stdin.read_to_end(&mut buf) {
                        Ok(_) => {}
                        Err(e) => return Err(MonorubyErr::rangeerr(e.to_string())),
                    }
                    Ok(buf)
                }
            }
            Self::Stdout(_) => return Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr(_) => return Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
                let file = &mut Rc::get_mut(file).unwrap().file;
                if let Some(length) = length {
                    let buf = match file.bytes().take(length).collect() {
                        Ok(buf) => buf,
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    };
                    Ok(buf)
                } else {
                    let mut buf = vec![];
                    file.read_to_end(&mut buf)
                        .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                    Ok(buf)
                }
            }
        }
    }

    pub fn isatty(&self) -> bool {
        match self {
            Self::Stdin(stdio) => stdio.is_terminal(),
            Self::Stdout(stdout) => stdout.is_terminal(),
            Self::Stderr(stderr) => stderr.is_terminal(),
            Self::File(file) => file.file.is_terminal(),
        }
    }
}
