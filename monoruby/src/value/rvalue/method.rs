use super::*;

#[derive(Debug, Clone, PartialEq, Hash)]
#[repr(C)]
pub struct MethodInner {
    receiver: Value,
    func_id: FuncId,
}

impl MethodInner {
    pub fn new(receiver: Value, func_id: FuncId) -> Self {
        Self { receiver, func_id }
    }

    pub fn receiver(&self) -> Value {
        self.receiver
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn to_s(&self, globals: &Globals) -> String {
        format!(
            "#<Method: {}#{}()>",
            globals.get_class_name(self.receiver.class()),
            globals[self.func_id()].name().unwrap()
        )
    }
}
