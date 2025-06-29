use super::*;

impl BytecodeGen {
    pub(super) fn handle_arguments(
        &mut self,
        arglist: ArgList,
        method: impl Into<Option<IdentId>>,
        recv: BcReg,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<CallSite> {
        if arglist.forwarding {
            self.handle_delegate(arglist, method, recv, dst, loc)
        } else {
            self.handle_no_delegate(arglist, method, recv, dst, loc)
        }
    }

    pub(super) fn handle_super_arguments(
        &mut self,
        arglist: Option<ArgList>,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<CallSite> {
        if let Some(arglist) = arglist {
            self.handle_arguments(arglist, None, BcReg::Self_, dst, loc)
        } else {
            Ok(self.handle_super_delegate(dst, loc))
        }
    }

    pub(super) fn handle_no_delegate(
        &mut self,
        mut arglist: ArgList,
        method: impl Into<Option<IdentId>>,
        recv: BcReg,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<CallSite> {
        let (args, pos_num, splat_pos) = self.positional_args(&mut arglist)?;

        let kw = self.keyword_arg(&mut arglist)?;

        let block_arg = self.sp().into();
        let block_fid = if let Some(box block) = std::mem::take(&mut arglist.block) {
            self.block_arg(block, loc)?
        } else {
            None
        };
        let block_arg = if block_arg == self.sp().into() {
            None
        } else {
            Some(block_arg)
        };

        let callsite = CallSite::new(
            method, pos_num, kw, splat_pos, block_fid, block_arg, args, recv, dst,
        );
        Ok(callsite)
    }

    fn handle_delegate(
        &mut self,
        arglist: ArgList,
        name: impl Into<Option<IdentId>>,
        recv: BcReg,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<CallSite> {
        let name = name.into();
        assert!(arglist.kw_args.is_empty());
        let (_, mother_args, outer) = self.mother.clone();
        let (args, pos_num, splat_pos) = if !arglist.args.is_empty() {
            let (args, mut len, mut splat_pos) = self.ordinary_args(arglist.args)?;
            if let Some(rest_pos) = mother_args.is_rest() {
                let dst = self.push().into();
                let src = BcLocal(rest_pos).into();
                if outer == 0 {
                    self.emit_mov(dst, src);
                } else {
                    self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
                }
                splat_pos.push(len);
                len += 1;
            }
            (args, len, splat_pos)
        } else if let Some(rest_pos) = mother_args.is_rest() {
            let pos_start = if outer == 0 {
                BcLocal(rest_pos).into()
            } else {
                let dst = self.push().into();
                let src = BcLocal(rest_pos).into();
                self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
                dst
            };
            (pos_start, 1, vec![0])
        } else {
            (BcLocal(0).into(), 0, vec![])
        };

        let kw = if let Some(kw_rest) = mother_args.kw_rest {
            let kw_start = if outer == 0 {
                BcLocal(kw_rest.0 - 1).into()
            } else {
                self.load_dynvar(kw_rest, outer, loc)
            };
            let hash_splat_pos = vec![kw_start];
            Some(KeywordArgs {
                kw_start,
                kw_args: RubyMap::default(),
                hash_splat_pos,
            })
        } else {
            None
        };

        let block = self.push().into();
        self.emit(BytecodeInst::BlockArgProxy(block, outer), loc);

        Ok(CallSite::new(
            name,
            pos_num,
            kw,
            splat_pos,
            None,
            Some(block),
            args,
            recv,
            dst,
        ))
    }

    fn handle_super_delegate(&mut self, dst: Option<BcReg>, loc: Loc) -> CallSite {
        let (_, mother_args, outer) = self.mother.clone();
        let pos_len = mother_args.total_positional_args();
        let splat_pos = if let Some(rest_pos) = mother_args.is_rest() {
            vec![rest_pos as usize]
        } else {
            vec![]
        };
        let pos_start = if outer == 0 {
            BcLocal(0).into()
        } else {
            let args = self.sp().into();
            for i in 0..pos_len {
                let dst = self.push().into();
                let src = BcLocal(i as _).into();
                self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
            }
            args
        };
        let kw_list = &mother_args.kw_names;
        let kw = if kw_list.is_empty() && mother_args.kw_rest.is_none() {
            None
        } else {
            let kw_start = if outer == 0 {
                BcLocal(pos_len as u16).into()
            } else {
                self.sp().into()
            };

            let mut kw_args = RubyMap::default();
            for (i, name) in kw_list.iter().enumerate() {
                kw_args.insert(*name, i);
                if outer != 0 {
                    let dst = self.push().into();
                    let src = BcLocal((pos_len + i) as u16).into();
                    self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
                }
            }

            let hash_splat_pos = if let Some(kw_rest) = mother_args.kw_rest {
                let kw_rest = if outer == 0 {
                    BcLocal(kw_rest.0 - 1).into()
                } else {
                    self.load_dynvar(kw_rest, outer, loc)
                };
                vec![kw_rest]
            } else {
                vec![]
            };
            Some(KeywordArgs {
                kw_start,
                kw_args,
                hash_splat_pos,
            })
        };

        let block = self.push().into();
        self.emit(BytecodeInst::BlockArgProxy(block, outer), loc);

        CallSite::new(
            None,
            pos_len,
            kw,
            splat_pos,
            None,
            Some(block),
            pos_start,
            BcReg::Self_,
            dst,
        )
    }

    fn load_dynvar(&mut self, slot_id: SlotId, outer: usize, loc: Loc) -> BcReg {
        let dst = self.push().into();
        let src = BcLocal(slot_id.0 - 1).into();
        self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
        dst
    }

    ///
    /// Handle ordinary arguments.
    ///
    fn positional_args(&mut self, arglist: &mut ArgList) -> Result<(BcReg, usize, Vec<usize>)> {
        if arglist.args.len() == 1
            //&& arglist.block.is_none()
            && arglist.kw_args.is_empty()
            && arglist.hash_splat.is_empty()
            && !arglist.forwarding
        {
            if let NodeKind::LocalVar(0, ident) = &arglist.args[0].kind {
                // in the case of "f(a)"
                if let Some(local) = self.refer_local(ident) {
                    return Ok((local, 1, vec![]));
                }
            } /*  else if let NodeKind::Splat(box node) = &arglist.args[0].kind {
                  // in the case of "f(*a)"
                  if let NodeKind::LocalVar(0, ident) = &node.kind {
                      if let Some(local) = self.refer_local(ident) {
                          return Ok((local, 1, vec![0]));
                      }
                  }
              }*/
        };

        self.ordinary_args(std::mem::take(&mut arglist.args))
    }

    fn keyword_arg(&mut self, arglist: &mut ArgList) -> Result<Option<KeywordArgs>> {
        let kw_args_list = std::mem::take(&mut arglist.kw_args);
        let hash_splat = std::mem::take(&mut arglist.hash_splat);
        if kw_args_list.is_empty() && hash_splat.is_empty() {
            Ok(None)
        } else {
            let mut kw_args = RubyMap::default();
            let kw_start = self.sp().into();
            let mut hash_splat_pos = vec![];
            for (id, (name, node)) in kw_args_list.into_iter().enumerate() {
                self.push_expr(node)?;
                kw_args.insert(IdentId::get_id_from_string(name), id);
            }
            for node in hash_splat {
                hash_splat_pos.push(self.push_expr(node)?.into());
            }
            Ok(Some(KeywordArgs {
                kw_start,
                kw_args,
                hash_splat_pos,
            }))
        }
    }

    fn block_arg(&mut self, block: Node, loc: Loc) -> Result<Option<Functions>> {
        match block.kind {
            NodeKind::Lambda(box block) => return Ok(Some(self.handle_block(vec![], block))),
            NodeKind::LocalVar(0, proc_local) => {
                let dst = self.push().into();
                if let Some(local) = self.refer_local(&proc_local) {
                    self.emit_mov(dst, local);
                } else {
                    self.emit(BytecodeInst::BlockArgProxy(dst, 0), loc);
                }
            }
            NodeKind::LocalVar(outer, proc_local) => {
                let proc_local = IdentId::get_id_from_string(proc_local);
                let dst = self.push().into();
                if let Some(src) = self.refer_dynamic_local(outer, proc_local) {
                    let src = src.into();
                    self.emit(BytecodeInst::LoadDynVar { dst, src, outer }, loc);
                } else {
                    assert_eq!(Some(proc_local), self.outer_block_param_name(outer));
                    self.emit(BytecodeInst::BlockArgProxy(dst, outer), loc);
                }
            }
            _ => {
                self.push_expr(block)?;
            }
        }
        Ok(None)
    }
}
