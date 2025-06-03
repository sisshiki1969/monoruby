use super::*;

#[derive(Debug, Clone)]
pub struct ExceptionInner(MonorubyErr);

impl std::ops::Deref for ExceptionInner {
    type Target = MonorubyErr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ExceptionInner {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ExceptionInner {
    pub fn new(err: MonorubyErr) -> Self {
        ExceptionInner(err)
    }

    pub fn set_kind(&mut self, kind: MonorubyErrKind) {
        self.kind = kind;
    }

    pub fn set_message(&mut self, msg: String) {
        self.message = msg;
    }

    pub fn kind(&self) -> MonorubyErrKind {
        self.kind.clone()
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn trace(&self) -> &[(Option<(Loc, SourceInfoRef)>, Option<FuncId>)] {
        &self.trace
    }

    pub fn trace_location(&self, store: &Store) -> Vec<String> {
        self.trace
            .iter()
            .map(|(source_loc, fid)| {
                if let Some((loc, source)) = source_loc {
                    store.location(fid.clone(), source, *loc)
                } else {
                    store.internal_location(fid.unwrap())
                }
            })
            .collect()
    }

    pub fn get_error_message(&self) -> String {
        format!("{} ({:?})", self.message, self.kind)
    }
}
