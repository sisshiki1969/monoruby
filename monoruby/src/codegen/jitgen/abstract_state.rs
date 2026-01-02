use super::*;

#[derive(Debug, Clone)]
pub(super) struct ResultState {
    ret: ReturnValue,
    class_version_guard: bool,
    side_effect_guard: bool,
}

#[derive(Debug, Clone, Copy)]
enum ReturnValue {
    UD,
    Const(Value),
    Class(ClassId),
    Value,
}

impl ResultState {
    fn return_class(&self) -> Option<ClassId> {
        match self.ret {
            ReturnValue::Const(v) => Some(v.class()),
            ReturnValue::Class(class) => Some(class),
            _ => None,
        }
    }

    fn const_folded(&self) -> Option<Value> {
        if !self.side_effect_guard {
            return None;
        }
        if let ReturnValue::Const(v) = self.ret {
            return Some(v);
        }
        None
    }

    fn join(&mut self, other: &Self) {
        self.class_version_guard &= other.class_version_guard;
        self.side_effect_guard &= other.side_effect_guard;

        match (&self.ret, &other.ret) {
            (ReturnValue::UD, _) => {
                self.ret = other.ret;
                return;
            }
            (_, ReturnValue::UD) => return,
            (ReturnValue::Const(l), ReturnValue::Const(r)) if l.id() == r.id() => {
                return;
            }
            _ => {}
        }

        if let Some(class) = self.return_class()
            && other.return_class() == Some(class)
        {
            self.ret = ReturnValue::Class(class);
            return;
        }
        self.ret = ReturnValue::Value;
    }
}
