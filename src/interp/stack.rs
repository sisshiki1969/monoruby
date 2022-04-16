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
    reg_base: usize,
    args_len: usize,
}

impl std::ops::Index<BcReg> for Stack {
    type Output = Value;
    fn index(&self, reg: BcReg) -> &Value {
        let i = self.get_index(reg);
        &self.stack[i]
    }
}

impl std::ops::IndexMut<BcReg> for Stack {
    fn index_mut(&mut self, reg: BcReg) -> &mut Value {
        let i = self.get_index(reg);
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
            reg_base: 0,
            args_len: 0,
        }
    }

    fn get_index(&self, reg: BcReg) -> usize {
        match reg {
            BcReg::Temp(i) => self.reg_base + i.0 as usize,
            BcReg::Local(i) => self.bp + i.0 as usize,
        }
    }

    fn push_u64(&mut self, v: u64) {
        self.stack.push(Value::from_unchecked(v));
    }

    fn push_u32(&mut self, v1: u32, v2: u32) {
        self.stack
            .push(Value::from_unchecked((v1 as u64) << 32 | (v2 as u64)));
    }

    /*fn args(&self) -> &[Value] {
        &self.stack[self.bp..self.bp + self.args_len]
    }*/

    fn reg_slice(&self, reg: BcTemp, len: usize) -> std::ops::Range<usize> {
        let start = self.reg_base + reg.0 as usize;
        start..start + len
    }

    pub(super) fn set_reg_base(&mut self, local_num: usize) {
        self.reg_base = self.bp + local_num;
    }

    pub(super) fn push_frame(
        &mut self,
        args: BcTemp,
        args_len: usize,
        bc_func: &BcFunc,
        cur_fn: BcFuncId,
        pc: usize,
        ret: Option<BcReg>,
    ) {
        let args = self.reg_slice(args, args_len);
        let local_num = bc_func.local_num();
        let reg_num = bc_func.reg_num;
        let ret = match ret {
            Some(r) => self.get_index(r) + 1,
            None => 0,
        };
        self.push_u64(ret as u64);
        self.push_u32(cur_fn.0 as u32, pc as u32);
        self.push_u64(self.args_len as u64);
        self.push_u64(self.bp as u64);
        self.bp = self.stack.len();
        self.reg_base = self.bp + local_num;
        self.args_len = args_len;
        let new_len = self.stack.len() + local_num + reg_num as usize;
        self.stack.extend_from_within(args);
        self.stack.resize(new_len, Value::nil());
    }

    pub(super) fn pop_frame(&mut self) -> (bool, BcFuncId, usize, Option<usize>) {
        let old_bp = self.bp;
        let ret = match self.stack[old_bp - 4].get() as usize {
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
