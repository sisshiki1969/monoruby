use super::*;

///
/// Call stack.
///
///                                            bp
/// | ret_reg | prev_pc | args_len | prev_bp | arg0 |..| local0 |..| temp0 |..
///
#[derive(Debug, Clone)]
pub struct Stack {
    stack: Vec<Value>,
    bp: usize,
    args_len: usize,
}

impl std::ops::Index<u16> for Stack {
    type Output = Value;
    fn index(&self, reg: u16) -> &Value {
        let i = reg as usize + self.bp;
        &self.stack[i]
    }
}

impl std::ops::IndexMut<u16> for Stack {
    fn index_mut(&mut self, reg: u16) -> &mut Value {
        let i = reg as usize + self.bp;
        &mut self.stack[i]
    }
}

impl std::ops::Index<usize> for Stack {
    type Output = Value;
    fn index(&self, i: usize) -> &Value {
        &self.stack[i]
    }
}

impl std::ops::IndexMut<usize> for Stack {
    fn index_mut(&mut self, i: usize) -> &mut Value {
        &mut self.stack[i]
    }
}

impl Stack {
    pub(super) fn new() -> Self {
        Self {
            stack: Vec::with_capacity(4096),
            bp: 0,
            args_len: 0,
        }
    }

    fn push_u64(&mut self, v: u64) {
        self.stack.push(Value::from_unchecked(v));
    }

    fn push_u32(&mut self, v1: u32, v2: u32) {
        self.stack
            .push(Value::from_unchecked((v1 as u64) << 32 | (v2 as u64)));
    }

    fn reg_slice(&self, reg: u16, len: usize) -> std::ops::Range<usize> {
        let start = self.bp + reg as usize;
        start..start + len
    }

    pub(super) fn push_frame(
        &mut self,
        args: u16,
        args_len: usize,
        bc_func: &BcFunc,
        cur_fn: BcFuncId,
        pc: usize,
        ret: Option<u16>,
    ) {
        let args = self.reg_slice(args, args_len);
        let local_num = bc_func.local_num();
        let reg_num = bc_func.reg_num;
        let ret = match ret {
            Some(r) => r + 1,
            None => 0,
        };
        self.push_u64(ret as u64);
        self.push_u32(cur_fn.0 as u32, pc as u32);
        self.push_u64(self.args_len as u64);
        self.push_u64(self.bp as u64);
        self.bp = self.stack.len();
        self.args_len = args_len;
        let new_len = self.stack.len() + local_num + reg_num as usize;
        self.stack.extend_from_within(args);
        self.stack.resize(new_len, Value::nil());
    }

    pub(super) fn pop_frame(&mut self) -> (bool, BcFuncId, usize, Option<u16>) {
        let old_bp = self.bp;
        let ret = match self.stack[old_bp - 4].get() as u16 {
            0 => None,
            r => Some(r - 1),
        };
        let fn_pc = self.stack[old_bp - 3].get() as usize;
        let cur_fn = fn_pc >> 32;
        let pc = fn_pc as u32 as usize;
        self.args_len = self.stack[old_bp - 2].get() as usize;
        self.bp = self.stack[old_bp - 1].get() as usize;
        self.stack.truncate(old_bp - 4);
        (self.bp == 0, BcFuncId(cur_fn), pc, ret)
    }
}
