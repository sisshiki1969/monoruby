use std::io::{IsTerminal, Write};

use super::*;

#[derive(Debug)]
pub enum IoInner {
    Stdin(std::io::Stdin),
    Stdout(std::io::Stdout),
    Stderr(std::io::Stderr),
    Io {},
}

impl std::clone::Clone for IoInner {
    fn clone(&self) -> Self {
        match self {
            Self::Stdin(_) => Self::Stdin(std::io::stdin()),
            Self::Stdout(_) => Self::Stdout(std::io::stdout()),
            Self::Stderr(_) => Self::Stderr(std::io::stderr()),
            Self::Io {} => Self::Io {},
        }
    }
}

impl std::fmt::Display for IoInner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Self::Stdin(_) => "<STDIN>",
            Self::Stdout(_) => "<STDOUT>",
            Self::Stderr(_) => "<STDERR>",
            Self::Io {} => "fd 0",
        };
        write!(f, "#<IO:{}>", s)
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

    pub fn write(&mut self, data: &[u8]) -> Result<()> {
        match self {
            Self::Stdin(_) => Err(MonorubyErr::argumenterr("can't write to $stdin")),
            Self::Stdout(stdout) => match stdout.write(data) {
                Ok(_) => Ok(()),
                Err(e) => Err(MonorubyErr::rangeerr(e.to_string())),
            },
            Self::Stderr(_) => Err(MonorubyErr::argumenterr("can't write to $stderr")),
            Self::Io {} => Err(MonorubyErr::argumenterr("can't write to fd 0")),
        }
    }

    pub fn isatty(&self) -> bool {
        match self {
            Self::Stdin(stdio) => stdio.is_terminal(),
            Self::Stdout(stdout) => stdout.is_terminal(),
            Self::Stderr(stderr) => stderr.is_terminal(),
            Self::Io {} => false,
        }
    }
}
