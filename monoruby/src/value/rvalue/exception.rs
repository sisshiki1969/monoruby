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

impl alloc::GC<RValue> for ExceptionInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.0.mark(alloc);
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
        self.mesg = None;
        self.default_message_class = None;
        self.name_span = None;
        self.class_span = None;
        self.given_message = true;
    }

    pub fn kind(&self) -> MonorubyErrKind {
        self.kind.clone()
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    /// The message for the core's own renderings (`debug`, the Rust-side
    /// `inspect`), which cannot call `#to_s`: a message object that is not
    /// a String is shown as the core renders it.
    pub fn debug_message(&self, store: &Store) -> String {
        match self.mesg {
            Some(mesg) if mesg.is_rstring_inner().is_none() => mesg.to_s(store),
            _ => self.message.clone(),
        }
    }

    pub fn trace(&self) -> &[TraceEntry] {
        &self.trace
    }

    /// The backtrace lines, as the Strings `#backtrace` answers.
    pub fn trace_values(&self, store: &Store) -> Vec<Value> {
        self.trace
            .iter()
            .map(|entry| store.trace_entry_value(entry))
            .collect()
    }

    pub fn trace_location(&self, store: &Store) -> Vec<String> {
        self.trace
            .iter()
            .map(|entry| store.trace_entry_location(entry))
            .collect()
    }

    pub fn get_error_message(&self) -> String {
        format!("{} ({:?})", self.message, self.kind)
    }
}
