//!
//! The `[]` / `[]=` **emission primitives** — the Array fast paths the
//! `Array#[]` / `Array#[]=` inline generators drive, kept here beside the
//! numeric ones (`state/binop.rs`) so the register-file and guard internals
//! they touch stay private to `state`.
//!
//! Both arches lower the same `AsmInst::ArrayIndex` / `ArrayIndexAssign`, so
//! there is one copy of each primitive; the per-arch asm lives in
//! `Codegen::array_index` / `array_index_assign`.
//!

use super::*;
use crate::codegen::jitgen::asmir::ArrayIndexKind;

impl AbstractFrame {

pub(crate) fn array_integer_index(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            ir.push(AsmInst::ArrayIndex {
                kind: ArrayIndexKind::U16(idx),
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            ir.push(AsmInst::ArrayIndex {
                kind: ArrayIndexKind::Fixnum,
            });
        }
        self.def_rax2acc(ir, dst);
    }

    ///
    /// Aray index assign operation.
    ///
    /// ### in
    /// - rsi: index Fixnum
    /// - rdx: result Value
    ///
    /// ### destroy
    /// - caller save registers except fpr's
    ///
pub(crate) fn array_integer_index_assign(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
    ) {
        self.load_array_ty(ir, store, base, GP::Rdi);
        if let Some(idx) = self.is_u16(idx) {
            self.load(ir, src, GP::Rdx);
            let using_fpr = self.get_using_fpr(ir);
            let error = ir.new_error(self);
            ir.push(AsmInst::ArrayIndexAssign {
                kind: ArrayIndexKind::U16(idx),
                using_fpr,
                error,
            });
        } else {
            self.load_fixnum(ir, idx, GP::Rsi);
            self.load(ir, src, GP::Rdx);
            let using_fpr = self.get_using_fpr(ir);
            let error = ir.new_error(self);
            ir.push(AsmInst::ArrayIndexAssign {
                kind: ArrayIndexKind::Fixnum,
                using_fpr,
                error,
            });
        }
    }
}
