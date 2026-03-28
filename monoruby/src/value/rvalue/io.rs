use std::{
    io::{BufRead, IsTerminal, Read, Write},
    os::fd::{AsRawFd, FromRawFd},
    rc::Rc,
};

use super::*;

#[derive(Debug)]
pub struct FileDescriptor {
    reader: std::io::BufReader<std::fs::File>,
    name: String,
}

#[derive(Debug)]
pub struct PopenDescriptor {
    child: std::process::Child,
    pub(crate) reader: Option<std::io::BufReader<std::process::ChildStdout>>,
    pub(crate) writer: Option<std::process::ChildStdin>,
}

#[derive(Debug)]
pub enum IoInner {
    Stdin,
    Stdout,
    Stderr,
    File(Rc<FileDescriptor>),
    Popen(Rc<PopenDescriptor>),
    Closed,
}

impl std::clone::Clone for IoInner {
    fn clone(&self) -> Self {
        match self {
            Self::Stdin => Self::Stdin,
            Self::Stdout => Self::Stdout,
            Self::Stderr => Self::Stderr,
            Self::File(file) => Self::File(file.clone()),
            Self::Popen(popen) => Self::Popen(popen.clone()),
            Self::Closed => Self::Closed,
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
            Self::Popen(_) => write!(f, "#<IO:popen>"),
            Self::Closed => write!(f, "#<IO:(closed)>"),
        }
    }
}

impl IoInner {
    pub fn flush(&mut self) -> Result<()> {
        let res = match self {
            Self::Stdin => return Ok(()),
            Self::Stdout => std::io::stdout().flush(),
            Self::Stderr => std::io::stderr().flush(),
            Self::File(file) => file.reader.get_ref().flush(),
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                if let Some(ref mut writer) = popen.writer {
                    writer.flush().map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                }
                return Ok(());
            }
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
        };
        res.map_err(|err| MonorubyErr::runtimeerr(err.to_string()))
    }

    pub fn is_closed(&self) -> bool {
        matches!(self, Self::Closed)
    }

    /// Close the IO. Returns (exit_status, pid) for Popen, None for others.
    pub fn close(&mut self) -> Result<Option<(i32, u32)>> {
        if self.is_closed() {
            return Err(MonorubyErr::ioerr("closed stream"));
        }
        let popen_result = if let Self::Popen(popen) = self {
            let popen = Rc::get_mut(popen).unwrap();
            popen.reader = None;
            popen.writer = None;
            popen.child.stdout.take();
            let pid = popen.child.id();
            let status = popen.child.wait().ok().and_then(|s| s.code()).unwrap_or(0);
            Some((status, pid))
        } else {
            None
        };
        *self = Self::Closed;
        Ok(popen_result)
    }

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

    pub(crate) fn popen(mut child: std::process::Child) -> Self {
        let reader = child.stdout.take().map(std::io::BufReader::new);
        let writer = child.stdin.take();
        Self::Popen(Rc::new(PopenDescriptor {
            child,
            reader,
            writer,
        }))
    }

    pub(crate) fn pid(&self) -> Option<u32> {
        match self {
            Self::Popen(popen) => Some(popen.child.id()),
            _ => None,
        }
    }

    pub(crate) fn from_raw_fd(fd: i32, name: String) -> Self {
        // SAFETY: fd is a valid file descriptor obtained from pipe().
        let file = unsafe { std::fs::File::from_raw_fd(fd) };
        Self::File(Rc::new(FileDescriptor {
            reader: std::io::BufReader::new(file),
            name,
        }))
    }

    pub fn write(&mut self, data: &[u8]) -> Result<()> {
        match self {
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
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
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                if let Some(ref mut writer) = popen.writer {
                    writer
                        .write(data)
                        .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                    Ok(())
                } else {
                    Err(MonorubyErr::ioerr("not opened for writing"))
                }
            }
        }
    }

    pub fn read(&mut self, length: Option<usize>) -> Result<Vec<u8>> {
        match self {
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
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
            Self::Stdout => Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => Err(MonorubyErr::argumenterr("can't read from $stderr")),
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
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                if let Some(length) = length {
                    let buf: std::result::Result<Vec<u8>, _> =
                        reader.bytes().take(length).collect();
                    buf.map_err(|e| MonorubyErr::ioerr(e.to_string()))
                } else {
                    let mut buf = vec![];
                    reader
                        .read_to_end(&mut buf)
                        .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                    Ok(buf)
                }
            }
        }
    }

    pub fn read_line(&mut self) -> Result<Option<String>> {
        match self {
            Self::Closed => return Err(MonorubyErr::ioerr("closed stream")),
            Self::Stdin => {
                let mut buf = String::new();
                std::io::stdin()
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
                Ok(Some(buf))
            }
            Self::Stdout => Err(MonorubyErr::argumenterr("can't read from $stdin")),
            Self::Stderr => Err(MonorubyErr::argumenterr("can't read from $stderr")),
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
            Self::Popen(popen) => {
                let popen = Rc::get_mut(popen).unwrap();
                let reader = popen
                    .reader
                    .as_mut()
                    .ok_or_else(|| MonorubyErr::ioerr("not opened for reading"))?;
                let mut buf = String::new();
                let size = reader
                    .read_line(&mut buf)
                    .map_err(|e| MonorubyErr::ioerr(e.to_string()))?;
                if size == 0 {
                    return Ok(None);
                }
                Ok(Some(buf))
            }
        }
    }

    pub fn fileno(&self) -> Result<i32> {
        match self {
            Self::Stdin => Ok(0),
            Self::Stdout => Ok(1),
            Self::Stderr => Ok(2),
            Self::File(file) => Ok(file.reader.get_ref().as_raw_fd()),
            Self::Popen(popen) => {
                if let Some(ref stdout) = popen.child.stdout {
                    Ok(stdout.as_raw_fd())
                } else if let Some(ref reader) = popen.reader {
                    Ok(reader.get_ref().as_raw_fd())
                } else {
                    Err(MonorubyErr::ioerr("closed stream"))
                }
            }
            Self::Closed => Err(MonorubyErr::ioerr("closed stream")),
        }
    }

    pub fn isatty(&self) -> bool {
        match self {
            Self::Stdin => std::io::stdin().is_terminal(),
            Self::Stdout => std::io::stdout().is_terminal(),
            Self::Stderr => std::io::stderr().is_terminal(),
            Self::File(_) | Self::Popen(_) | Self::Closed => false,
        }
    }
}
