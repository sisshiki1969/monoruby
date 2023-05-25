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
}
