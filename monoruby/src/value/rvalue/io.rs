use std::{
    io::{BufRead, IsTerminal, Read, Write},
    rc::Rc,
};

use super::*;

#[derive(Debug)]
pub struct FileDescriptor {
    reader: std::io::BufReader<std::fs::File>,
    name: String,
}

#[derive(Debug)]
pub enum IoInner {
    Stdin,
    Stdout,
    Stderr,
    File(Rc<FileDescriptor>),
}

impl std::clone::Clone for IoInner {
    fn clone(&self) -> Self {
        match self {
            Self::Stdin => Self::Stdin,
            Self::Stdout => Self::Stdout,
            Self::Stderr => Self::Stderr,
            Self::File(file) => Self::File(file.clone()),
        }
    }
}

impl std::fmt::Display for IoInner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Stdin => write!(f, "#<IO:<STDIN>>"),
            Self::Stdout => write!(f, "#<IO:<STDOUT>>"),
            Self::Stderr => write!(f, "#<IO:<STDERR>>"),
            Self::File(file) => write!(f, "#<File:{}>", file.name),
        }
    }
}

impl IoInner {
    pub(super) fn stdin() -> Self {
        Self::Stdin
    }

    pub(super) fn stdout() -> Self {
        Self::Stdout
    }

    pub(super) fn stderr() -> Self {
        Self::Stderr
    }

    pub(super) fn file(file: std::fs::File, name: String) -> Self {
        Self::File(Rc::new(FileDescriptor {
            reader: std::io::BufReader::new(file),
            name,
        }))
    }

    pub fn write(&mut self, data: &[u8]) -> Result<()> {
        match self {
            Self::Stdin => Err(MonorubyErr::argumenterr("can't write to $stdin")),
            Self::Stdout => match std::io::stdout().write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::Stderr => match std::io::stderr().write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::File(file) => {
                let _ = Rc::get_mut(file)
                    .unwrap()
                    .reader
                    .get_mut()
                    .write(data)
                    .map_err(|e| MonorubyErr::rangeerr(e.to_string()))?;
                Ok(())
            }
        }
    }

    pub fn read(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
        match self {
            Self::Stdin => {
                if let Some(length) = length {
                    let buf = match std::io::stdin().bytes().take(length).collect() {
                        Ok(buf) => buf,
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    };
                    Ok(buf)
                } else {
                    let mut buf = vec![];
                    match std::io::stdin().read_to_end(&mut buf) {
                        Ok(_) => {}
                        Err(e) => return Err(MonorubyErr::runtimeerr(e.to_string())),
                    }
                    Ok(buf)
                }
            }
            Self::Stdout => return Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => return Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
                let file = &mut Rc::get_mut(file).unwrap().reader;
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

    pub fn read_line(&mut self) -> Result<Option<String>> {
        match self {
            Self::Stdin => {
                let mut buf = String::new();
                std::io::stdin()
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                Ok(Some(buf))
            }
            Self::Stdout => return Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => return Err(MonorubyErr::argumenterr("can't read from $stderr")),
            Self::File(file) => {
                let file = &mut Rc::get_mut(file).unwrap().reader;
                let mut buf = String::new();
                let size = file
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                if size == 0 {
                    return Ok(None);
                }
                Ok(Some(buf))
            }
        }
    }

    pub fn isatty(&self) -> bool {
        match self {
            Self::Stdin => std::io::stdin().is_terminal(),
            Self::Stdout => std::io::stdout().is_terminal(),
            Self::Stderr => std::io::stderr().is_terminal(),
            Self::File(_) => false,
        }
    }
}
