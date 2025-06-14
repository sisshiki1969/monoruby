use super::*;

impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    pub(super) fn parse_comp_stmt(&mut self) -> Result<Node, LexerErr> {
        // COMP_STMT : (STMT (TERM STMT)*)? (TERM+)?
        self.peek()?;
        let loc = self.loc();
        let mut nodes = vec![];

        loop {
            if self.peek()?.check_stmt_end() {
                let node = Node::new_comp_stmt(nodes, loc);
                return Ok(node);
            }

            let node = self.parse_stmt()?;
            nodes.push(node);
            if !self.consume_term()? {
                break;
            }
        }
        let node = Node::new_comp_stmt(nodes, loc);
        Ok(node)
    }

    fn parse_stmt(&mut self) -> Result<Node, LexerErr> {
        // STMT : EXPR
        // | ALIAS-STMT
        // | UNDEF-STMT
        // | STMT [no-term] if EXPR
        // | STMT [no-term] unless EXPR
        // | STMT [no-term] while EXPR
        // | STMT [no-term] until EXPR
        // | STMT [no-term] rescue EXPR
        // | STMT - NORET-STMT [no-term] rescue EXPR
        // | VAR [no term] = UNPARENTHESIZED-METHOD-CALL
        // | PRIMARY :: CONST [no term] = UNPARENTHESIZED-METHOD-CALL
        // | :: CONST [no term] = UNPARENTHESIZED-METHOD-CALL
        // | PRIMARY [no term] (.|::) LOCAL-VAR [no term] = UNPARENTHESIZED-METHOD-CALL
        // | PRIMARY [no term] . CONST [no term] = UNPARENTHESIZED-METHOD-CALL
        // | VAR [no term] <assign-op> UNPARENTHESIZED-METHOD-CALL
        // | PRIMARY [no term] [INDEX-LIST] [no term] <assign-op> UNPARENTHESIZED-METHOD-CALL
        // | LHS [no term] = MRHS
        // | * LHS [no term] = (UNPARENTHESIZED-METHOD-CALL | ARG)
        // | MLHS [no term] = MRHS
        let mut node = self.parse_expr()?;
        loop {
            if self.consume_reserved_no_skip_line_term(Reserved::If)? {
                // STMT : STMT if EXPR
                let loc = self.prev_loc();
                let cond = self.parse_expr()?;
                node = Node::new_if(cond, node, Node::new_comp_stmt(vec![], loc), loc);
            } else if self.consume_reserved_no_skip_line_term(Reserved::Unless)? {
                // STMT : STMT unless EXPR
                let loc = self.prev_loc();
                let cond = self.parse_expr()?;
                node = Node::new_if(cond, Node::new_comp_stmt(vec![], loc), node, loc);
            } else if self.consume_reserved_no_skip_line_term(Reserved::While)? {
                // STMT : STMT while EXPR
                let loc = self.prev_loc();
                let cond = self.parse_expr()?;
                let loc = loc.merge(self.prev_loc());
                node = Node::new_while_postfix(cond, node, true, loc);
            } else if self.consume_reserved_no_skip_line_term(Reserved::Until)? {
                // STMT : STMT until EXPR
                let loc = self.prev_loc();
                let cond = self.parse_expr()?;
                let loc = loc.merge(self.prev_loc());
                node = Node::new_while_postfix(cond, node, false, loc);
            } else if self.consume_reserved_no_skip_line_term(Reserved::Rescue)? {
                // STMT : STMT rescue EXPR
                let rescue = self.parse_expr()?;
                node = Node::new_begin(node, vec![RescueEntry::new_postfix(rescue)], None, None);
            } else {
                break;
            }
        }
        // STMT : EXPR
        Ok(node)
    }

    pub(super) fn parse_expr(&mut self) -> Result<Node, LexerErr> {
        // EXPR : NOT
        // | EXPR [no term] and NOT
        // | EXPR [no term] or NOT
        let mut node = self.parse_not()?;
        loop {
            if self.consume_reserved_no_skip_line_term(Reserved::And)? {
                let rhs = self.parse_not()?;
                node = Node::new_binop(BinOp::LAnd, node, rhs);
            } else if self.consume_reserved_no_skip_line_term(Reserved::Or)? {
                let rhs = self.parse_not()?;
                node = Node::new_binop(BinOp::LOr, node, rhs);
            } else {
                return Ok(node);
            }
        }
    }

    fn parse_not(&mut self) -> Result<Node, LexerErr> {
        // NOT : ARG
        // | UNPARENTHESIZED-METHOD
        // | ! UNPARENTHESIZED-METHOD
        // | not NOT
        let node = self.parse_arg(false)?;
        if self.consume_punct_no_term(Punct::Comma)? {
            // EXPR : MLHS `=' MRHS
            return self.parse_mul_assign(node);
        }
        if self.consume_punct(Punct::FatArrow)? {
            return Err(LexerErr(
                ParseErrKind::SyntaxError(
                    "`=>` is not allowed here. (Pattern match is not supported yet)".to_string(),
                ),
                self.prev_loc,
            ));
        }
        Ok(node)
    }

    fn parse_mul_assign(&mut self, first_lhs: Node) -> Result<Node, LexerErr> {
        // EXPR : MLHS `=' MRHS
        let loc = first_lhs.loc;
        let mut splat_flag = matches!(first_lhs.kind, NodeKind::Splat(_));
        let mut mlhs = vec![first_lhs];
        let old = self.suppress_acc_assign;
        self.suppress_acc_assign = true;
        loop {
            if self.peek_punct_no_term(Punct::Assign) {
                for n in &mlhs {
                    loc.merge(n.loc());
                }
                mlhs.push(Node::new_discard(loc));
                break;
            }
            let node = if self.consume_punct(Punct::Mul)? {
                if splat_flag {
                    let loc = self.prev_loc();
                    return Err(error_unexpected(
                        loc,
                        "Multiple splat operators are not allowed.",
                    ));
                }
                splat_flag = true;
                let node = self.parse_method_call()?;
                let loc = node.loc;
                Node::new_splat(node, loc)
            } else {
                self.parse_method_call()?
            };
            mlhs.push(node);
            if !self.consume_punct_no_term(Punct::Comma)? {
                break;
            }
        }
        self.suppress_acc_assign = old;
        if !self.consume_punct_no_term(Punct::Assign)? {
            let loc = self.loc();
            return Err(error_unexpected(loc, "Expected '='."));
        }

        let mrhs = self.parse_mul_assign_rhs_if_allowed()?;
        for lhs in &mut mlhs {
            let mut node = self.check_lhs(std::mem::take(lhs))?;
            std::mem::swap(lhs, &mut node);
        }

        Ok(Node::new_mul_assign(mlhs, mrhs))
    }

    /// Parse rhs of multiple assignment.
    /// If Parser.mul_assign_rhs is true, only a single assignment is allowed.
    fn parse_mul_assign_rhs_if_allowed(&mut self) -> Result<Vec<Node>, LexerErr> {
        if self.suppress_mul_assign {
            let node = vec![self.parse_arg(false)?];
            Ok(node)
        } else {
            let mrhs = self.parse_mul_assign_rhs(None, false)?;
            Ok(mrhs)
        }
    }

    /// Parse rhs of multiple assignment. cf: a,b,*c,d
    pub(super) fn parse_mul_assign_rhs(
        &mut self,
        term: impl Into<Option<Punct>>,
        allow_braceless_hash: bool,
    ) -> Result<Vec<Node>, LexerErr> {
        let term = term.into();
        let old = self.suppress_mul_assign;
        // multiple assignment must be suppressed in parsing arg list.
        self.suppress_mul_assign = true;

        let mut args = vec![];
        loop {
            if let Some(term) = term {
                if self.consume_punct(term)? {
                    self.suppress_mul_assign = old;
                    return Ok(args);
                }
            };
            if self.consume_punct(Punct::Mul)? {
                // splat argument
                let loc = self.prev_loc();
                let array = self.parse_arg(false)?;
                args.push(Node::new_splat(array, loc));
            } else {
                let node = self.parse_arg(allow_braceless_hash)?;
                args.push(node);
            }
            if !self.consume_punct(Punct::Comma)? {
                break;
            }
        }
        self.suppress_mul_assign = old;
        if let Some(term) = term {
            self.expect_punct(term)?;
        };
        Ok(args)
    }

    pub(super) fn parse_arg(&mut self, allow_braceless_hash: bool) -> Result<Node, LexerErr> {
        let next = self.peek()?;
        if matches!(next.kind, TokenKind::Reserved(_)) && self.lexer.has_trailing_colon(&next) {
            let next = self.get()?;
            if let TokenKind::Reserved(reserved) = next.kind {
                let name = reserved.as_str().to_string();
                return Ok(Node::new_identifier(name, next.loc));
            } else {
                unreachable!()
            }
        }
        if self.lexer.has_trailing_space(&next) && self.consume_reserved(Reserved::Defined)? {
            self.defined_mode = true;
            let node = self.parse_arg(false)?;
            self.defined_mode = false;
            return Ok(Node::new_defined(node));
        }
        let save = self.save_state();
        let arg = self.parse_arg_assign()?;
        if allow_braceless_hash {
            if self.consume_punct(Punct::FatArrow)?
                || (arg.is_symbol_key().is_some() && self.consume_punct(Punct::Colon)?)
            {
                self.restore_state(save);
                return self.parse_hash_literal(true);
            }
        }
        Ok(arg)
    }

    fn parse_arg_assign(&mut self) -> Result<Node, LexerErr> {
        let lhs = self.parse_arg_ternary()?;
        if self.is_line_term()? {
            return Ok(lhs);
        }
        if self.consume_punct_no_term(Punct::Assign)? {
            let mrhs = self.parse_mul_assign_rhs(None, false)?;
            let lhs = self.check_lhs(lhs)?;
            Ok(Node::new_mul_assign(vec![lhs], mrhs))
        } else if let Some(op) = self.consume_assign_op_no_term()? {
            // <lhs> <assign_op> <arg>
            self.parse_assign_op(lhs, op)
        } else {
            Ok(lhs)
        }
    }

    fn parse_arg_ternary(&mut self) -> Result<Node, LexerErr> {
        let cond = self.parse_arg_range()?;
        let loc = cond.loc();
        if self.consume_punct_no_term(Punct::Question)? {
            let then_ = self.parse_arg(false)?;
            if !self.consume_punct_no_term(Punct::Colon)? {
                let loc = self.loc();
                return Err(error_unexpected(loc, "Expect ':'."));
            };
            let else_ = self.parse_arg(false)?;
            let node = Node::new_if(cond, then_, else_, loc);
            Ok(node)
        } else {
            Ok(cond)
        }
    }

    fn parse_arg_range(&mut self) -> Result<Node, LexerErr> {
        let lhs = self.parse_arg_logical_or()?;
        if self.is_line_term()? {
            return Ok(lhs);
        }
        if self.consume_punct(Punct::Range2)? {
            let rhs = self.parse_arg_logical_or()?;
            let loc = lhs.loc().merge(rhs.loc());
            Ok(Node::new_range(lhs, rhs, false, loc))
        } else if self.consume_punct(Punct::Range3)? {
            let rhs = self.parse_arg_logical_or()?;
            let loc = lhs.loc().merge(rhs.loc());
            Ok(Node::new_range(lhs, rhs, true, loc))
        } else {
            Ok(lhs)
        }
    }

    fn parse_arg_logical_or(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_logical_and()?;
        while self.consume_punct_no_term(Punct::LOr)? {
            let rhs = self.parse_arg_logical_and()?;
            lhs = Node::new_binop(BinOp::LOr, lhs, rhs);
        }
        Ok(lhs)
    }

    fn parse_arg_logical_and(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_eq()?;
        while self.consume_punct_no_term(Punct::LAnd)? {
            let rhs = self.parse_arg_eq()?;
            lhs = Node::new_binop(BinOp::LAnd, lhs, rhs);
        }
        Ok(lhs)
    }

    // 4==4==4 => SyntaxError
    fn parse_arg_eq(&mut self) -> Result<Node, LexerErr> {
        let lhs = self.parse_arg_comp()?;
        if self.consume_punct_no_term(Punct::Eq)? {
            let rhs = self.parse_arg_comp()?;
            Ok(Node::new_binop(BinOp::Cmp(CmpKind::Eq), lhs, rhs))
        } else if self.consume_punct_no_term(Punct::Ne)? {
            let rhs = self.parse_arg_comp()?;
            Ok(Node::new_binop(BinOp::Cmp(CmpKind::Ne), lhs, rhs))
        } else if self.consume_punct_no_term(Punct::TEq)? {
            let rhs = self.parse_arg_comp()?;
            Ok(Node::new_binop(BinOp::Cmp(CmpKind::TEq), lhs, rhs))
        } else if self.consume_punct_no_term(Punct::Match)? {
            let rhs = self.parse_arg_comp()?;
            Ok(Node::new_binop(BinOp::Match, lhs, rhs))
        } else if self.consume_punct_no_term(Punct::Unmatch)? {
            let rhs = self.parse_arg_comp()?;
            let loc = lhs.loc().merge(rhs.loc());
            let node = Node::new_binop(BinOp::Match, lhs, rhs);
            Ok(Node::new_unop(UnOp::Not, node, loc))
        } else {
            Ok(lhs)
        }
    }

    fn parse_arg_comp(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_bitor()?;
        if self.is_line_term()? {
            return Ok(lhs);
        }
        loop {
            if self.consume_punct_no_term(Punct::Ge)? {
                let rhs = self.parse_arg_bitor()?;
                lhs = Node::new_binop(BinOp::Cmp(CmpKind::Ge), lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Gt)? {
                let rhs = self.parse_arg_bitor()?;
                lhs = Node::new_binop(BinOp::Cmp(CmpKind::Gt), lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Le)? {
                let rhs = self.parse_arg_bitor()?;
                lhs = Node::new_binop(BinOp::Cmp(CmpKind::Le), lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Lt)? {
                let rhs = self.parse_arg_bitor()?;
                lhs = Node::new_binop(BinOp::Cmp(CmpKind::Lt), lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Cmp)? {
                let rhs = self.parse_arg_bitor()?;
                lhs = Node::new_binop(BinOp::Compare, lhs, rhs);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_arg_bitor(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_bitand()?;
        loop {
            if self.consume_punct_no_term(Punct::BitOr)? {
                lhs = Node::new_binop(BinOp::BitOr, lhs, self.parse_arg_bitand()?);
            } else if self.consume_punct_no_term(Punct::BitXor)? {
                lhs = Node::new_binop(BinOp::BitXor, lhs, self.parse_arg_bitand()?);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_arg_bitand(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_shift()?;
        loop {
            if self.consume_punct_no_term(Punct::BitAnd)? {
                lhs = Node::new_binop(BinOp::BitAnd, lhs, self.parse_arg_shift()?);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_arg_shift(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_add()?;
        loop {
            if self.consume_punct_no_term(Punct::Shl)? {
                lhs = Node::new_binop(BinOp::Shl, lhs, self.parse_arg_add()?);
            } else if self.consume_punct_no_term(Punct::Shr)? {
                lhs = Node::new_binop(BinOp::Shr, lhs, self.parse_arg_add()?);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_arg_add(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_arg_mul()?;
        loop {
            if self.consume_punct_no_term(Punct::Plus)? {
                let rhs = self.parse_arg_mul()?;
                lhs = Node::new_binop(BinOp::Add, lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Minus)? {
                let rhs = self.parse_arg_mul()?;
                lhs = Node::new_binop(BinOp::Sub, lhs, rhs);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_arg_mul(&mut self) -> Result<Node, LexerErr> {
        let mut lhs = self.parse_unary_minus()?;
        if self.is_line_term()? {
            return Ok(lhs);
        }
        loop {
            if self.consume_punct_no_term(Punct::Mul)? {
                let rhs = self.parse_unary_minus()?;
                lhs = Node::new_binop(BinOp::Mul, lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Div)? {
                let rhs = self.parse_unary_minus()?;
                lhs = Node::new_binop(BinOp::Div, lhs, rhs);
            } else if self.consume_punct_no_term(Punct::Rem)? {
                let rhs = self.parse_unary_minus()?;
                lhs = Node::new_binop(BinOp::Rem, lhs, rhs);
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_unary_minus(&mut self) -> Result<Node, LexerErr> {
        let save = self.save_state();
        let lhs = if self.consume_punct(Punct::Minus)? {
            let save2 = self.save_state();
            let loc = self.prev_loc();
            match self.peek_no_term()?.kind {
                TokenKind::IntegerLit(_) | TokenKind::FloatLit(_) | TokenKind::BignumLit(_) => {
                    let _ = self.parse_primary()?;
                    if self.consume_punct_no_term(Punct::DMul)? {
                        self.restore_state(save2);
                        let lhs = self.parse_exponent()?;
                        let loc = loc.merge(lhs.loc());
                        let lhs = Node::new_unop(UnOp::Neg, lhs, loc);
                        return self.parse_accesory_assign(lhs);
                    } else {
                        self.restore_state(save);
                        let lhs = self.parse_exponent()?;
                        return Ok(lhs);
                    }
                }
                _ => {}
            };
            let lhs = self.parse_unary_minus()?;
            let loc = loc.merge(lhs.loc());
            Node::new_unop(UnOp::Neg, lhs, loc)
        } else {
            self.parse_exponent()?
        };
        self.parse_accesory_assign(lhs)
    }

    fn parse_accesory_assign(&mut self, lhs: Node) -> Result<Node, LexerErr> {
        if !self.suppress_acc_assign {
            if self.consume_punct_no_term(Punct::Assign)? {
                let mrhs = self.parse_mul_assign_rhs_if_allowed()?;
                let lhs = self.check_lhs(lhs)?;
                return Ok(Node::new_mul_assign(vec![lhs], mrhs));
            } else if let Some(op) = self.consume_assign_op_no_term()? {
                return self.parse_assign_op(lhs, op);
            }
        };
        Ok(lhs)
    }

    /// Parse assign-op.
    /// <lhs> <assign_op> <arg>
    fn parse_assign_op(&mut self, lhs: Node, op: BinOp) -> Result<Node, LexerErr> {
        match op {
            BinOp::LOr | BinOp::LAnd => {
                self.get()?;
                let rhs = self.parse_arg(false)?;
                let lhs = self.check_lhs(lhs)?;
                let node =
                    Node::new_binop(op, lhs.clone(), Node::new_mul_assign(vec![lhs], vec![rhs]));
                Ok(node)
            }
            _ => {
                self.get()?;
                let rhs = self.parse_arg(false)?;
                let lhs = self.check_lhs(lhs)?;
                Ok(Node::new_assign_op(op, lhs, rhs))
            }
        }
    }

    fn parse_exponent(&mut self) -> Result<Node, LexerErr> {
        let lhs = self.parse_unary()?;
        if self.consume_punct_no_term(Punct::DMul)? {
            let rhs = self.parse_exponent()?;
            Ok(Node::new_binop(BinOp::Exp, lhs, rhs))
        } else {
            Ok(lhs)
        }
    }

    fn parse_unary(&mut self) -> Result<Node, LexerErr> {
        if self.consume_punct(Punct::BitNot)? {
            let loc = self.prev_loc();
            let lhs = Node::new_unop(UnOp::BitNot, self.parse_unary()?, loc);
            Ok(lhs)
        } else if self.consume_punct(Punct::Not)? {
            let loc = self.prev_loc();
            let lhs = Node::new_unop(UnOp::Not, self.parse_unary()?, loc);
            Ok(lhs)
        } else if self.consume_punct(Punct::Plus)? {
            let loc = self.prev_loc();
            let lhs = Node::new_unop(UnOp::Pos, self.parse_unary()?, loc);
            Ok(lhs)
        } else {
            self.parse_method_call()
        }
    }

    fn parse_method_call(&mut self) -> Result<Node, LexerErr> {
        if self.consume_reserved(Reserved::Yield)? {
            return self.parse_yield();
        }
        // 一次式メソッド呼び出し
        // スコープ付き定数参照 :: 一次式 [行終端子禁止][空白類禁止] "::" 定数識別子
        //      ｜"::" 定数識別子
        let mut node = self.parse_primary()?;
        loop {
            node = if self.consume_punct(Punct::Dot)? {
                self.parse_primary_method(node, false)?
            } else if self.consume_punct_no_term(Punct::SafeNav)? {
                self.parse_primary_method(node, true)?
            } else if self.consume_punct_no_term(Punct::Scope)? {
                if let TokenKind::Const(_) = self.peek()?.kind {
                    let loc = node.loc;
                    let name = self.expect_const()?;
                    if let Some(arglist) = self.parse_arguments(true)? {
                        // Foo::Bar()
                        Node::new_mcall(node, name, arglist, false, self.prev_loc())
                    } else if let NodeKind::Const {
                        toplevel,
                        parent,
                        mut prefix,
                        name: parent_name,
                    } = node.kind
                    {
                        prefix.push(parent_name);
                        Node::new_const(name, toplevel, parent, prefix, self.prev_loc().merge(loc))
                    } else {
                        Node::new_const(
                            name,
                            false,
                            Some(Box::new(node)),
                            vec![],
                            self.prev_loc().merge(loc),
                        )
                    }
                } else {
                    self.parse_primary_method(node, false)?
                }
            } else if self.consume_punct_no_term(Punct::LBracket)? {
                let member_loc = self.prev_loc();
                let args = self.parse_mul_assign_rhs(Punct::RBracket, true)?;
                let member_loc = member_loc.merge(self.prev_loc());
                Node::new_array_member(node, args, member_loc)
            } else {
                return Ok(node);
            };
        }
    }

    fn parse_yield(&mut self) -> Result<Node, LexerErr> {
        let loc = self.prev_loc();
        let tok = self.peek_no_term()?;
        // TODO: This is not correct.
        if tok.is_term()
            || tok.kind == TokenKind::Reserved(Reserved::Unless)
            || tok.kind == TokenKind::Reserved(Reserved::If)
            || tok.check_stmt_end()
        {
            return Ok(Node::new_yield(ArgList::default(), loc));
        };
        let arglist = if let Some(arglist) = self.parse_arguments(false)? {
            arglist
        } else {
            ArgList::default()
        };
        Ok(Node::new_yield(arglist, loc))
    }

    /// Parse primary method call.
    ///
    /// `receiver`.
    /// `receiver`?
    /// `receiver`::
    pub(super) fn parse_primary_method(
        &mut self,
        receiver: Node,
        safe_nav: bool,
    ) -> Result<Node, LexerErr> {
        // 一次式メソッド呼出し : 省略可能実引数付きsuper
        //      ｜ 添字メソッド呼出し
        //      ｜ メソッド専用識別子
        //      ｜ メソッド識別子 ブロック
        //      ｜ メソッド識別子 括弧付き実引数 ブロック?
        //      ｜ 一次式 ［行終端子禁止］ "." メソッド名 括弧付き実引数? ブロック?
        //      ｜ 一次式 ［行終端子禁止］ "::" メソッド名 括弧付き実引数 ブロック?
        //      ｜ 一次式 ［行終端子禁止］ "::" 定数以外のメソッド名 ブロック?
        let loc = self.prev_loc;
        if let Some(arglist) = self.parse_arguments_paren(true)? {
            // Support https://docs.ruby-lang.org/ja/latest/doc/spec=2fcall.html#call_method
            // `receiver`.(..)
            // `receiver`?(..)
            // `receiver`::(..)
            let node = Node::new_mcall(receiver, "call".to_string(), arglist, false, loc);
            return Ok(node);
        };
        let (id, loc) = self.read_method_name(false)?;
        let arglist = if let Some(arglist) = self.parse_arguments(true)? {
            // `receiver`.method(..)
            arglist
        } else {
            match self.parse_block()? {
                Some(block) => ArgList::with_block(block),
                None => ArgList::default(),
            }
        };

        let receiver = match receiver.kind {
            NodeKind::Ident(id) => Node::new_fcall_noarg(id, false, loc),
            _ => receiver,
        };
        Ok(Node::new_mcall(receiver, id, arglist, safe_nav, loc))
    }

    pub(super) fn parse_begin(&mut self) -> Result<Node, LexerErr> {
        // begin式 :: "begin"  複合文  rescue節*  else節?  ensure節?  "end"
        // rescue節 :: "rescue" [行終端子禁止] 例外クラスリスト?  例外変数代入?  then節
        // 例外クラスリスト :: 演算子式 | 多重代入右辺
        // 例外変数代入 :: "=>" 左辺
        // ensure節 :: "ensure" 複合文
        let body = self.parse_comp_stmt()?;
        let mut rescue = vec![];
        loop {
            if !self.consume_reserved(Reserved::Rescue)? {
                break;
            };
            let mut assign = None;
            let mut exception = vec![];
            if !self.consume_term()? {
                if !self.peek_punct_no_term(Punct::FatArrow) {
                    exception = self.parse_mul_assign_rhs(None, false)?;
                };
                if self.consume_punct_no_term(Punct::FatArrow)? {
                    let lhs = self.parse_arg(false)?;
                    assign = Some(self.check_lhs(lhs)?);
                }
                self.parse_then()?;
            };
            let rescue_body = self.parse_comp_stmt()?;
            rescue.push(RescueEntry::new(exception, assign, rescue_body));
        }
        let else_ = if self.consume_reserved(Reserved::Else)? {
            Some(self.parse_comp_stmt()?)
        } else {
            None
        };
        let ensure = if self.consume_reserved(Reserved::Ensure)? {
            Some(self.parse_comp_stmt()?)
        } else {
            None
        };
        self.expect_reserved(Reserved::End)?;
        Ok(Node::new_begin(body, rescue, else_, ensure))
    }

    pub(super) fn parse_splat(&mut self, loc: Loc) -> Result<Node, LexerErr> {
        let first_lhs = self.parse_method_call()?;
        let loc = loc.merge(first_lhs.loc());
        let first_lhs = Node::new_splat(first_lhs, loc);
        if self.consume_punct_no_term(Punct::Comma)? {
            self.parse_mul_assign(first_lhs)
        } else if self.consume_punct_no_term(Punct::Assign)? {
            let mrhs = self.parse_mul_assign_rhs_if_allowed()?;
            let lhs = self.check_lhs(first_lhs)?;
            Ok(Node::new_mul_assign(vec![lhs], mrhs))
        } else {
            let loc = self.loc();
            return Err(error_unexpected(loc, "Expected '=' or ','."));
        }
    }

    /// Check whether `lhs` is a local variable or not.
    fn check_lhs(&mut self, lhs: Node) -> Result<Node, LexerErr> {
        let loc = lhs.loc();
        if let NodeKind::Ident(name) = lhs.kind {
            if name.starts_with('_') {
                let mut iter = name.chars();
                iter.next();
                if let Some(ch1) = iter.next() {
                    if ch1.is_ascii_digit() && iter.next().is_none() {
                        // lhs is numbered parameter literal
                        return Err(error_numbered_param(
                            lhs.loc,
                            (ch1 as u32 - '0' as u32) as u8,
                        ));
                    }
                }
            }
            let outer = self.add_local_var_if_new(&name);
            return Ok(Node::new_lvar(name, outer, lhs.loc));
        } else if let NodeKind::Const { .. } = lhs.kind {
            for c in self.scope.iter().rev() {
                match c.kind {
                    ScopeKind::Class | ScopeKind::Eval => return Ok(lhs),
                    ScopeKind::Method => {
                        return Err(error_unexpected(lhs.loc(), "Dynamic constant assignment."))
                    }
                    _ => {}
                }
            }
        } else if let NodeKind::Splat(lhs) = lhs.kind {
            return Ok(Node::new_splat(self.check_lhs(*lhs)?, loc));
        }
        Ok(lhs)
    }
}
