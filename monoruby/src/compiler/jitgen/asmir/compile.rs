use super::*;

mod binary_op;
mod constants;
mod defined;
mod definition;
mod index;
mod method_call;
mod variables;

impl Codegen {
    ///
    /// Generate machine code for *inst*.
    ///
    pub(super) fn gen_asmir(
        &mut self,
        store: &Store,
        ctx: &mut JitContext,
        labels: &SideExitLabels,
        inst: AsmInst,
    ) {
        match inst {
            AsmInst::BcIndex(_) => {}
            AsmInst::Label(label) => {
                let label = ctx.resolve_label(&mut self.jit, label);
                self.jit.bind_label(label);
            }
            AsmInst::AccToStack(r) => {
                self.store_r15(r);
            }
            AsmInst::RegToAcc(r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq r15, R(r);
                );
            }
            AsmInst::RegToStack(r, slot) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [r14 - (conv(slot))], R(r);
                );
            }
            AsmInst::StackToReg(slot, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), [r14 - (conv(slot))];
                );
            }
            AsmInst::LitToReg(v, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq R(r), (v.id());
                );
            }
            AsmInst::I32ToReg(i, r) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movl R(r), (i);
                );
            }
            AsmInst::RegMove(src, dst) => {
                let src = src as u64;
                let dst = dst as u64;
                monoasm!( &mut self.jit,
                    movq R(dst), R(src);
                );
            }
            AsmInst::RegAdd(r, i) => {
                if i != 0 {
                    let r = r as u64;
                    monoasm! { &mut self.jit,
                        addq R(r), (i);
                    }
                }
            }
            AsmInst::RegSub(r, i) => {
                if i != 0 {
                    let r = r as u64;
                    monoasm! { &mut self.jit,
                        subq R(r), (i);
                    }
                }
            }
            AsmInst::RegToRSPOffset(r, ofs) => {
                let r = r as u64;
                monoasm!( &mut self.jit,
                    movq [rsp + (ofs)], R(r);
                );
            }

            AsmInst::XmmMove(l, r) => self.xmm_mov(l, r),
            AsmInst::XmmSwap(l, r) => self.xmm_swap(l, r),
            AsmInst::XmmBinOp {
                kind,
                mode,
                dst,
                using_xmm,
            } => self.float_binop(kind, using_xmm, dst, mode),
            AsmInst::XmmUnOp { kind, dst } => match kind {
                UnOpK::Neg => {
                    let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                    monoasm!( &mut self.jit,
                        xorps xmm(dst.enc()), [rip + imm];
                    );
                }
                UnOpK::Pos => {}
            },

            AsmInst::NumToXmm(reg, x, side_exit) => {
                self.numeric_val_to_f64(reg, x, labels[side_exit]);
            }
            AsmInst::F64ToXmm(f, x) => {
                let f = self.jit.const_f64(f);
                monoasm!( &mut self.jit,
                    movq  xmm(x.enc()), [rip + f];
                );
            }
            AsmInst::IntToXmm(r, x, side_exit) => {
                self.integer_val_to_f64(r, x, labels[side_exit]);
            }
            AsmInst::FloatToXmm(reg, x, side_exit) => {
                self.float_to_f64(reg, x, labels[side_exit]);
            }
            AsmInst::I64ToBoth(i, r, x) => {
                let f = self.jit.const_f64(i as f64);
                monoasm! {&mut self.jit,
                    movq [r14 - (conv(r))], (Value::integer(i).id());
                    movq xmm(x.enc()), [rip + f];
                }
            }
            AsmInst::XmmToStack(x, slots) => self.xmm_to_stack(x, &[slots]),
            AsmInst::LitToStack(v, slot) => self.literal_to_stack(slot, v),
            AsmInst::DeepCopyLit(v, using_xmm) => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                  movq rdi, (v.id());
                  movq rax, (Value::value_deep_copy);
                  call rax;
                );
                self.xmm_restore(using_xmm);
            }

            AsmInst::GuardFloat(r, deopt) => {
                let deopt = labels[deopt];
                self.guard_float(r, deopt);
            }
            AsmInst::GuardFixnum(r, deopt) => {
                let deopt = labels[deopt];
                self.guard_fixnum(r, deopt)
            }
            AsmInst::GuardArrayTy(r, deopt) => {
                let deopt = labels[deopt];
                self.guard_array_ty(r, deopt)
            }
            AsmInst::GuardClassVersion(fid, cached_version, callid, using_xmm, deopt, error) => {
                let deopt = labels[deopt];
                let error = labels[error];
                self.guard_class_version(fid, cached_version, callid, using_xmm, deopt, error);
            }
            AsmInst::GuardClass(r, class, deopt) => {
                let deopt = labels[deopt];
                self.guard_class(r, class, deopt);
            }

            AsmInst::HandleError(error) => {
                let error = labels[error];
                self.handle_error(error);
            }
            AsmInst::Deopt(deopt) => {
                let deopt = labels[deopt];
                monoasm!( &mut self.jit,
                    jmp deopt;
                );
            }
            AsmInst::RecompileDeopt { position, deopt } => {
                let deopt = labels[deopt];
                self.recompile_and_deopt(position, deopt)
            }
            AsmInst::CheckBOP { deopt } => {
                let deopt = labels[deopt];
                let bop_flag = self.bop_redefined_flags;
                let l1 = self.jit.label();
                monoasm!(
                    &mut self.jit,
                    cmpl [rip + bop_flag], 0;
                    jnz l1;
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                l1:
                    movq rdi, (Value::symbol_from_str("_bop_guard").id());
                    jmp  deopt;
                );
                self.jit.select_page(0);
            }
            AsmInst::WriteBack(wb) => self.gen_write_back(&wb),
            AsmInst::XmmSave(using_xmm) => self.xmm_save(using_xmm),
            AsmInst::XmmRestore(using_xmm) => self.xmm_restore(using_xmm),
            AsmInst::ExecGc(wb) => self.execute_gc(Some(&wb)),
            AsmInst::SetArguments { callid, callee_fid } => {
                let meta = store[callee_fid].meta();
                let offset = store[callee_fid].get_offset();
                let args = store[callid].args;
                self.jit_set_arguments(callid, args, offset, meta);
            }

            AsmInst::Ret => {
                self.epilogue();
            }
            AsmInst::MethodRet(pc) => {
                monoasm! { &mut self.jit,
                    movq r13, ((pc + 1).as_ptr());
                };
                self.method_return();
            }
            AsmInst::BlockBreak => {
                self.block_break();
                self.epilogue();
            }
            AsmInst::Raise => {
                let raise = self.entry_raise;
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, rax;
                    movq rax, (runtime::raise_err);
                    call rax;
                    jmp  raise;
                };
            }
            AsmInst::EnsureEnd => {
                let raise = self.entry_raise;
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rax, (runtime::check_err);
                    call rax;
                    testq rax, rax;
                    jne  raise;
                };
            }
            AsmInst::Br(dest) => {
                let dest = ctx.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    jmp dest;
                );
            }
            AsmInst::CondBr(brkind, dest) => {
                let dest = ctx.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    orq rax, 0x10;
                    cmpq rax, (FALSE_VALUE);
                );
                match brkind {
                    BrKind::BrIf => monoasm!( &mut self.jit, jne dest;),
                    BrKind::BrIfNot => monoasm!( &mut self.jit, jeq dest;),
                }
            }
            AsmInst::NilBr(dest) => {
                let dest = ctx.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    cmpq rax, (NIL_VALUE);
                    jeq  dest;
                );
            }
            AsmInst::CheckLocal(dest) => {
                let dest = ctx.resolve_label(&mut self.jit, dest);
                monoasm!( &mut self.jit,
                    testq rax, rax;
                    jnz  dest;
                );
            }
            AsmInst::OptCase {
                max,
                min,
                else_dest,
                branch_table,
            } => {
                // generate a jump table.
                let jump_table = self.jit.const_align8();
                for bbid in branch_table.iter() {
                    let dest_label = ctx.basic_block_labels.get(&bbid).cloned().unwrap();
                    let dest_label = ctx.resolve_label(&mut self.jit, dest_label);
                    self.jit.abs_address(dest_label);
                }

                let else_dest = ctx.basic_block_labels.get(&else_dest).cloned().unwrap();
                let else_dest = ctx.resolve_label(&mut self.jit, else_dest);
                monoasm! {&mut self.jit,
                    sarq rdi, 1;
                    cmpq rdi, (max);
                    jgt  else_dest;
                    subq rdi, (min);
                    jlt  else_dest;
                    lea  rax, [rip + jump_table];
                    jmp  [rax + rdi * 8];
                };
            }

            AsmInst::ImmediateEvict { evict } => {
                let patch_point = self.jit.get_current_address();
                let return_addr = self.asm_return_addr_table.get(&evict).unwrap();
                self.return_addr_table
                    .entry(*return_addr)
                    .and_modify(|e| e.0 = Some(patch_point));
            }
            AsmInst::AttrWriter {
                ivar_id,
                using_xmm,
                error,
            } => {
                self.attr_writer(using_xmm, labels[error], ivar_id);
            }
            AsmInst::AttrReader { ivar_id } => {
                self.attr_reader(ivar_id);
            }
            AsmInst::SendCached {
                callid,
                callee_fid,
                recv_class,
                error,
                evict,
            } => {
                let error = labels[error];
                let return_addr = self.send_cached(store, callid, callee_fid, recv_class, error);
                self.set_deopt_with_return_addr(return_addr, evict, labels[evict]);
            }
            AsmInst::SendNotCached {
                self_class,
                callid,
                pc,
                error,
                evict,
            } => {
                let error = labels[error];
                let return_addr = self.send_not_cached(store, callid, self_class, pc, error);
                self.set_deopt_with_return_addr(return_addr, evict, labels[evict]);
            }
            AsmInst::Yield {
                callid,
                using_xmm,
                error,
                evict,
            } => {
                let error = labels[error];
                let return_addr = self.gen_yield(store, callid, using_xmm, error);
                self.set_deopt_with_return_addr(return_addr, evict, labels[evict]);
            }

            AsmInst::Not => {
                self.not_rdi_to_rax();
            }
            AsmInst::GenericUnOp { func, using_xmm } => {
                self.xmm_save(using_xmm);
                self.call_unop(func);
                self.xmm_restore(using_xmm);
            }

            AsmInst::GenericBinOp { kind, using_xmm } => {
                self.generic_binop(kind, using_xmm);
            }
            AsmInst::IntegerBinOp {
                kind,
                mode,
                using_xmm,
                deopt,
                error,
            } => {
                let deopt = labels[deopt];
                let error = labels[error];
                self.integer_binop(&mode, kind, deopt, error, using_xmm);
            }

            AsmInst::GenericCmp { kind, using_xmm } => {
                self.generic_cmp(&kind, using_xmm);
            }
            AsmInst::IntegerCmp { kind, mode } => self.integer_cmp(kind, mode),
            AsmInst::IntegerCmpBr {
                mode,
                kind,
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx.resolve_label(&mut self.jit, branch_dest);
                self.cmp_integer(&mode);
                self.condbr_int(kind, branch_dest, brkind);
            }
            AsmInst::FloatCmp { kind, mode } => {
                monoasm! { &mut self.jit,
                    xorq rax, rax;
                };
                self.cmp_float(&mode);
                self.setflag_float(kind);
            }
            AsmInst::FloatCmpBr {
                kind,
                mode,
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx.resolve_label(&mut self.jit, branch_dest);
                self.cmp_float(&mode);
                self.condbr_float(kind, branch_dest, brkind);
            }

            AsmInst::GuardBaseClass { base_class, deopt } => {
                let deopt = labels[deopt];
                let cached_base_class = self.jit.const_i64(base_class.id() as _);
                monoasm! { &mut self.jit,
                    cmpq rax, [rip + cached_base_class];  // rax: base_class
                    jne  deopt;
                }
            }
            AsmInst::LoadFloatConstant {
                fdst,
                f,
                cached_version,
                deopt,
            } => {
                let deopt = labels[deopt];
                self.load_float_constant(fdst, deopt, f, cached_version);
            }
            AsmInst::LoadGenericConstant {
                cached_val,
                cached_version,
                deopt,
            } => {
                let deopt = labels[deopt];
                self.load_generic_constant(deopt, cached_val, cached_version);
            }
            AsmInst::StoreConstant { id, using_xmm } => {
                self.store_constant(id, using_xmm);
            }

            AsmInst::GenericIndex {
                base,
                idx,
                pc,
                using_xmm,
            } => {
                self.generic_index(using_xmm, base, idx, pc);
            }
            AsmInst::ArrayU16Index { idx } => {
                self.gen_array_u16_index(idx);
            }
            AsmInst::ArrayIndex => {
                self.gen_array_index();
            }
            AsmInst::GenericIndexAssign {
                src,
                base,
                idx,
                pc,
                using_xmm,
            } => {
                self.generic_index_assign(using_xmm, base, idx, src, pc);
            }
            AsmInst::ArrayU16IndexAssign {
                idx,
                using_xmm,
                error,
            } => {
                self.gen_array_u16_index_assign(using_xmm, labels[error], idx);
                //self.handle_error(labels[error]);
            }
            AsmInst::ArrayIndexAssign { using_xmm, error } => {
                self.gen_array_index_assign(using_xmm, labels[error]);
                //self.handle_error(labels[error]);
            }

            AsmInst::NewArray { callid, using_xmm } => {
                self.new_array(callid, using_xmm);
            }
            AsmInst::NewLambda(func_id, using_xmm) => {
                self.new_lambda(func_id, using_xmm);
            }
            AsmInst::NewHash(args, len, using_xmm) => {
                self.new_hash(args, len, using_xmm);
            }
            AsmInst::NewRange {
                start,
                end,
                exclude_end,
                using_xmm,
            } => {
                self.load_rdi(start);
                self.load_rsi(end);
                self.new_range(exclude_end, using_xmm);
            }

            AsmInst::BlockArgProxy { ret, outer } => {
                self.get_method_lfp(outer);
                self.block_arg_proxy();
                self.store_rax(ret);
            }
            AsmInst::BlockArg {
                ret,
                outer,
                using_xmm,
                error,
            } => {
                self.get_method_lfp(outer);
                self.block_arg(using_xmm);
                self.handle_error(labels[error]);
                self.store_rax(ret);
            }

            AsmInst::LoadDynVar { src } => self.load_dyn_var(src),
            AsmInst::StoreDynVar { dst, src } => self.store_dyn_var(dst, src),

            AsmInst::LoadIVar {
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
            } => self.load_ivar(name, cached_ivarid, is_object_ty, is_self_cached, using_xmm),
            AsmInst::StoreIVar {
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
                error,
            } => self.store_ivar(
                name,
                cached_ivarid,
                is_object_ty,
                is_self_cached,
                using_xmm,
                labels[error],
            ),

            AsmInst::LoadCVar { name, using_xmm } => {
                self.load_cvar(name, using_xmm);
            }
            AsmInst::CheckCVar { name, using_xmm } => {
                self.check_cvar(name, using_xmm);
            }
            AsmInst::StoreCVar {
                name,
                src,
                using_xmm,
            } => {
                self.store_cvar(name, src, using_xmm);
            }

            AsmInst::LoadGVar { name, using_xmm } => self.load_gvar(name, using_xmm),
            AsmInst::StoreGVar {
                name,
                src,
                using_xmm,
            } => self.store_gvar(name, src, using_xmm),
            AsmInst::LoadSVar { id, using_xmm } => self.load_svar(id, using_xmm),

            AsmInst::ClassDef {
                base,
                superclass,
                dst,
                name,
                func_id,
                is_module,
                using_xmm,
                error,
            } => {
                self.class_def(
                    base,
                    superclass,
                    dst,
                    name,
                    func_id,
                    is_module,
                    using_xmm,
                    labels[error],
                );
            }
            AsmInst::SingletonClassDef {
                base,
                dst,
                func_id,
                using_xmm,
                error,
            } => {
                self.singleton_class_def(base, dst, func_id, using_xmm, labels[error]);
            }
            AsmInst::MethodDef {
                name,
                func_id,
                using_xmm,
            } => {
                self.method_def(name, func_id, using_xmm);
            }
            AsmInst::SingletonMethodDef {
                obj,
                name,
                func_id,
                using_xmm,
            } => {
                self.singleton_method_def(obj, name, func_id, using_xmm);
            }

            AsmInst::ExpandArray {
                dst,
                len,
                using_xmm,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    lea rsi, [r14 - (conv(dst))];
                    movq rdx, (len);
                    movq rax, (runtime::expand_array);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            AsmInst::ConcatStr {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_string(arg, len, using_xmm);
            }
            AsmInst::ConcatRegexp {
                arg,
                len,
                using_xmm,
            } => {
                self.concat_regexp(arg, len, using_xmm);
            }
            AsmInst::AliasMethod {
                new,
                old,
                using_xmm,
            } => {
                self.xmm_save(using_xmm);
                monoasm!( &mut self.jit,
                    movq rdi, rbx;
                    movq rsi, r12;
                    movq rdx, [r14 - (LFP_SELF)];
                    movl rcx, (new.get());
                    movl r8, (old.get());
                    movq r9, [r14 - (LFP_META)];
                    movq rax, (runtime::alias_method);
                    call rax;
                );
                self.xmm_restore(using_xmm);
            }
            AsmInst::DefinedYield { dst, using_xmm } => self.defined_yield(dst, using_xmm),
            AsmInst::DefinedConst {
                dst,
                siteid,
                using_xmm,
            } => self.defined_const(dst, siteid, using_xmm),
            AsmInst::DefinedMethod {
                dst,
                recv,
                name,
                using_xmm,
            } => self.defined_method(dst, recv, name, using_xmm),
            AsmInst::DefinedGvar {
                dst,
                name,
                using_xmm,
            } => self.defined_gvar(dst, name, using_xmm),

            AsmInst::DefinedIvar {
                dst,
                name,
                using_xmm,
            } => self.defined_ivar(dst, name, using_xmm),

            AsmInst::GenericCondBr {
                brkind,
                branch_dest,
            } => {
                let branch_dest = ctx.resolve_label(&mut self.jit, branch_dest);
                self.cond_br(branch_dest, brkind);
            }

            AsmInst::Inline(proc) => (proc.proc)(self, labels),
        }
    }

    fn set_deopt_with_return_addr(
        &mut self,
        return_addr: CodePtr,
        evict: AsmEvict,
        evict_label: DestLabel,
    ) {
        self.asm_return_addr_table.insert(evict, return_addr);
        self.return_addr_table
            .insert(return_addr, (None, evict_label));
    }

    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version, and if the version is changed, call `find_method` and
    /// compare obtained FuncId and cached FuncId.
    /// If different, jump to `deopt`.
    /// If identical, update the cached version and go on.
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### out
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers except rdi
    /// - stack
    ///
    fn guard_class_version(
        &mut self,
        cached_fid: FuncId,
        cached_version: u32,
        callid: CallSiteId,
        using_xmm: UsingXmm,
        deopt: DestLabel,
        error: DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let global_version = self.class_version;
        let unmatch = self.jit.label();
        let exit = self.jit.label();
        let fail = self.jit.label();
        let cached_version = self.jit.data_i32(cached_version as i32);
        monoasm! { &mut self.jit,
            movl rax, [rip + cached_version];
            cmpl [rip + global_version], rax;
            jne  unmatch;
        exit:
        }

        self.jit.select_page(1);
        self.jit.bind_label(unmatch);
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            pushq rdi;
            pushq r13;
            movq rcx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (callid.get());  // CallSiteId
            movq rax, (runtime::find_method2);
            call rax;   // rax <- Option<FuncId>
            popq r13;
            popq rdi;
            movl rax, rax;
        }
        self.xmm_restore(using_xmm);
        self.handle_error(error);
        monoasm! { &mut self.jit,
            cmpl rax, (cached_fid.get());
            jne  fail;
            movl rax, [rip + global_version];
            movl [rip + cached_version], rax;
            jmp  exit;
        fail:
            movq rdi, (Value::symbol_from_str("__version_guard").id());
            jmp  deopt;
        }
        self.jit.select_page(0);
    }

    ///
    /// Get method lfp.
    ///
    /// ### in
    /// - r14: lfp
    ///
    /// ### out
    /// - rax: method lfp
    ///
    fn get_method_lfp(&mut self, outer: usize) {
        if outer == 0 {
            monoasm! { &mut self.jit,
                movq rax, r14;
            };
        } else {
            monoasm!( &mut self.jit,
                movq rax, [r14];
            );
            for _ in 0..outer - 1 {
                monoasm!( &mut self.jit,
                    movq rax, [rax];
                );
            }
        }
    }

    fn new_array(&mut self, callid: CallSiteId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movl rdx, (callid.get());
            lea  rcx, [r14 - (LFP_SELF)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_array);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_lambda(&mut self, func_id: FuncId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movl rdx, (func_id.get());
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::gen_lambda);
            call rax;
        };
        self.restore_lfp();
        self.xmm_restore(using_xmm);
    }

    fn new_hash(&mut self, args: SlotId, len: usize, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            lea  rdi, [r14 - (conv(args))];
            movq rsi, (len);
            movq rax, (runtime::gen_hash);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn new_range(&mut self, exclude_end: bool, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, rbx; // &mut Executor
            movq rcx, r12; // &mut Globals
            movl r8, (exclude_end as u32);
            movq rax, (runtime::gen_range);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    fn concat_string(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [r14 - (conv(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_string);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    fn concat_regexp(&mut self, arg: SlotId, len: u16, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            lea rdx, [r14 - (conv(arg))];
            movq rcx, (len);
            movq rax, (runtime::concatenate_regexp);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }

    ///
    /// Get block handler of a current method frame.
    ///
    /// ### in
    /// - rax: method lfp
    ///
    /// ### out
    /// - rax: block handler
    ///
    fn block_arg_proxy(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, [rax - (LFP_BLOCK)];
            xorq rdi, rdi;
            movq rsi, 0b10;
            testq rax, 0b1;
            cmovneq rdi, rsi;
            addq rax, rdi;
        };
    }

    ///
    /// Get a block argument of current frame.
    ///
    fn block_arg(&mut self, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, [rax - (LFP_BLOCK)];
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::block_arg);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }
}
