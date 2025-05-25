use super::*;

impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    pub(super) fn parse_primary(&mut self) -> Result<Node, LexerErr> {
        let tok = self.get()?;
        let loc = tok.loc();
        match tok.kind {
            TokenKind::Ident(name) => {
                match name.as_str() {
                    "true" => return Ok(Node::new_bool(true, loc)),
                    "false" => return Ok(Node::new_bool(false, loc)),
                    "nil" => return Ok(Node::new_nil(loc)),
                    "self" => return Ok(Node::new_self(loc)),
                    "__LINE__" => {
                        let line = self.lexer.get_line(loc.0);
                        return Ok(Node::new_integer(line as i64, loc));
                    }
                    "__FILE__" => {
                        let file = self.path.to_string_lossy().to_string();
                        return Ok(Node::new_string(file.into(), loc));
                    }
                    _ => {}
                };

                if !self.lexer.trailing_lparen()
                    && !self.is_var_command()
                    && let Some(outer) = self.is_local_var(&name)
                {
                    Ok(Node::new_lvar(name, outer, loc))
                } else {
                    if let Some(arglist) = self.parse_arguments(true)? {
                        Ok(Node::new_fcall(name, arglist, false, loc))
                    } else {
                        let node = Node::new_identifier(name, loc);
                        Ok(node)
                    }
                }
            }
            TokenKind::NumberedParam(i, name) => {
                if self.lexer.trailing_lparen() {
                    // _1()
                    return self.parse_func_call(name, loc);
                };
                self.check_outer_numbered_param(loc)?;
                // FUNCTION or COMMAND or LHS for assignment
                if let Ok(tok) = self.peek_no_term() {
                    match tok.kind {
                        // Multiple assignment
                        TokenKind::Punct(Punct::Comma) => return Err(error_numbered_param(loc, i)),
                        _ => {}
                    }
                };

                if let Some(arglist) = self.parse_arguments_unparen(true)? {
                    Ok(Node::new_fcall(name, arglist, false, loc))
                } else {
                    let node = Node::new_identifier(name, loc);
                    Ok(node)
                }
            }
            TokenKind::Const(name) => {
                if let Some(arglist) = self.parse_arguments(false)? {
                    Ok(Node::new_fcall(name, arglist, false, loc))
                } else {
                    Ok(Node::new_const(name, false, None, vec![], loc))
                }
            }
            TokenKind::InstanceVar(name) => Ok(Node::new_instance_var(name, loc)),
            TokenKind::ClassVar(name) => Ok(Node::new_class_var(name, loc)),
            TokenKind::GlobalVar(name) => Ok(Node::new_global_var(name, loc)),
            TokenKind::SpecialVar(id) => Ok(Node::new_special_var(id, loc)),
            TokenKind::IntegerLit(num) => Ok(Node::new_integer(num, loc)),
            TokenKind::BignumLit(num) => Ok(Node::new_bignum(num, loc)),
            TokenKind::FloatLit(num) => Ok(Node::new_float(num, loc)),
            TokenKind::ImaginaryLit(num) => Ok(Node::new_imaginary(num, loc)),
            TokenKind::StringLit(s) => self.parse_string_literal(s),
            TokenKind::CommandLit(s) => {
                let content = Node::new_string(s.into(), loc);
                Ok(Node::new_command(content))
            }
            TokenKind::OpenString(s, term, level) => {
                self.parse_interporation(s.into(), term, level)
            }
            TokenKind::OpenCommand(s, term, level) => {
                let content = self.parse_interporation(s.into(), term, level)?;
                Ok(Node::new_command(content))
            }
            TokenKind::Punct(punct) => match punct {
                Punct::Minus => match self.get()?.kind {
                    TokenKind::IntegerLit(num) => match num.checked_neg() {
                        Some(i) => Ok(Node::new_integer(i, loc)),
                        None => Ok(Node::new_bignum(-BigInt::from(num), loc)),
                    },
                    TokenKind::BignumLit(num) => Ok(Node::new_bignum(-num, loc)),
                    TokenKind::FloatLit(num) => Ok(Node::new_float(-num, loc)),
                    _ => {
                        return Err(error_unexpected(
                            loc,
                            format!("Unexpected token: {:?}", tok.kind),
                        ))
                    }
                },
                Punct::LParen => {
                    let old = self.suppress_mul_assign;
                    self.suppress_mul_assign = false;
                    let node = self.parse_comp_stmt()?;
                    self.expect_punct(Punct::RParen)?;
                    self.suppress_mul_assign = old;
                    Ok(node)
                }
                Punct::LBracket => {
                    // Array literal
                    let nodes = self.parse_mul_assign_rhs(Punct::RBracket, true)?;
                    let loc = loc.merge(self.prev_loc());
                    Ok(Node::new_array(nodes, loc))
                }
                Punct::LBrace => self.parse_hash_literal(false),
                Punct::Colon => self.parse_symbol(),
                Punct::Arrow => self.parse_lambda_literal(),
                Punct::Scope => {
                    let name = self.expect_const()?;
                    Ok(Node::new_const(name, true, None, vec![], loc))
                }
                Punct::Div => self.parse_regexp(),
                Punct::Rem => self.parse_percent_notation(),
                Punct::Question => self.parse_char_literal(),
                Punct::Shl => self.parse_heredocument(),
                Punct::Mul => self.parse_splat(loc),
                _ => Err(error_unexpected(
                    loc,
                    format!("Unexpected token: {:?}", tok.kind),
                )),
            },
            TokenKind::Reserved(reserved) => match reserved {
                Reserved::If => self.parse_if(),
                Reserved::Unless => self.parse_unless(),
                Reserved::For => self.parse_for(),
                Reserved::While => self.parse_while(true),
                Reserved::Until => self.parse_while(false),
                Reserved::Case => self.parse_case(),
                Reserved::Def => self.parse_def(),
                Reserved::Class => {
                    let loc = self.prev_loc();
                    if self.consume_punct(Punct::Shl)? {
                        self.parse_singleton_class(loc)
                    } else {
                        if self.is_method_context() {
                            return Err(error_unexpected(loc, "class definition in method body."));
                        }
                        self.parse_class(false)
                    }
                }
                Reserved::Module => {
                    if self.is_method_context() {
                        return Err(error_unexpected(
                            loc,
                            "SyntaxError: module definition in method body.",
                        ));
                    }
                    self.parse_class(true)
                }
                Reserved::Return => self.parse_return(),
                Reserved::Break => self.parse_break(),
                Reserved::Next => self.parse_next(),
                Reserved::Redo => self.parse_redo(),
                Reserved::Begin => self.parse_begin(),
                Reserved::Defined => {
                    if self.consume_punct_no_term(Punct::LParen)? {
                        self.defined_mode = true;
                        let node = self.parse_expr()?;
                        self.defined_mode = false;
                        self.expect_punct(Punct::RParen)?;
                        Ok(Node::new_defined(node))
                    } else {
                        let tok = self.get()?;
                        Err(error_unexpected(tok.loc, "expected '('.".to_string()))
                    }
                }
                Reserved::Alias => {
                    let new_name = self.alias_name()?;
                    let old_name = self.alias_name()?;
                    let loc = loc.merge(self.prev_loc());
                    Ok(Node::new_alias(new_name, old_name, loc))
                }
                Reserved::Super => self.parse_super(),
                _ => Err(error_unexpected(
                    loc,
                    format!("Unexpected token: {:?}", tok.kind),
                )),
            },
            TokenKind::Eof => Err(error_eof(loc)),
            _ => Err(error_unexpected(
                loc,
                format!("Unexpected token: {:?}", tok.kind),
            )),
        }
    }

    ///
    /// Parse function args.
    ///
    /// - `method`( arg0, arg1,... )
    /// - `method` do end
    /// - `method` {}
    /// - `method`
    fn parse_func_call(&mut self, method: String, loc: Loc) -> Result<Node, LexerErr> {
        let arglist = if let Some(arglist) = self.parse_arguments_paren(true)? {
            // PRIMARY-METHOD : FNAME ( ARGS ) BLOCK?
            arglist
        } else if let Some(block) = self.parse_block()? {
            // PRIMARY-METHOD : FNAME BLOCK
            ArgList::with_block(block)
        } else {
            ArgList::default()
        };
        Ok(Node::new_fcall(method, arglist, false, loc))
    }

    fn parse_super(&mut self) -> Result<Node, LexerErr> {
        let loc = self.prev_loc();
        let arglist = if let Some(arglist) = self.parse_arguments(true)? {
            arglist
        } else {
            return Ok(Node::new_super(None, loc));
        };
        let loc = self.prev_loc().merge(loc);
        Ok(Node::new_super(arglist, loc))
    }

    ///
    /// Parse arguments with parentheses.
    ///
    /// - (arg0, arg1, ...)
    pub(super) fn parse_arguments(
        &mut self,
        with_block: bool,
    ) -> Result<Option<ArgList>, LexerErr> {
        if let Some(arglist) = self.parse_arguments_paren(with_block)? {
            Ok(Some(arglist))
        } else if let Some(arglist) = self.parse_arguments_unparen(with_block)? {
            Ok(Some(arglist))
        } else if with_block && let Some(block) = self.parse_block()? {
            Ok(Some(ArgList::with_block(block)))
        } else {
            Ok(None)
        }
    }

    /// Parse block.
    ///     do |x| stmt end
    ///     { |x| stmt }
    pub(super) fn parse_block(&mut self) -> Result<Option<Box<Node>>, LexerErr> {
        let old_suppress_mul_flag = self.suppress_mul_assign;
        self.suppress_mul_assign = false;
        let res = self.parse_block_inner();
        self.suppress_mul_assign = old_suppress_mul_flag;
        res
    }

    fn parse_block_inner(&mut self) -> Result<Option<Box<Node>>, LexerErr> {
        let do_flag =
            if !self.suppress_do_block && self.consume_reserved_no_skip_line_term(Reserved::Do)? {
                true
            } else if self.consume_punct_no_term(Punct::LBrace)? {
                false
            } else {
                return Ok(None);
            };
        // BLOCK: do [`|' [BLOCK_VAR] `|'] COMPSTMT end
        let loc = self.prev_loc();
        self.scope.push(LvarScope::new_block(None));
        self.loop_stack.push(LoopKind::Block);

        let (params, _) = if self.consume_punct(Punct::BitOr)? {
            (self.parse_formal_params(Punct::BitOr)?, true)
        } else {
            self.consume_punct(Punct::LOr)?;
            (vec![], false)
        };

        let body = if do_flag {
            self.parse_begin()?
        } else {
            let body = self.parse_comp_stmt()?;
            self.expect_punct(Punct::RBrace)?;
            body
        };

        self.loop_stack.pop().unwrap();
        let lvar = self.scope.pop().unwrap().lvar;
        let loc = loc.merge(self.prev_loc());
        let node = Node::new_lambda(params, body, lvar, loc);
        Ok(Some(Box::new(node)))
    }

    fn is_var_command(&mut self) -> bool {
        let tok = match self.peek_no_term() {
            Ok(tok) => tok,
            _ => return false,
        };
        if self.lexer.trailing_space() {
            match tok.kind {
                TokenKind::LineTerm => false,
                TokenKind::Punct(p) => match p {
                    Punct::LParen
                    | Punct::LBracket
                    | Punct::LBrace
                    | Punct::Scope
                    | Punct::Arrow
                    | Punct::Not => true,
                    _ => false,
                },
                TokenKind::Reserved(r) => !matches!(
                    r,
                    Reserved::Do
                        | Reserved::If
                        | Reserved::Unless
                        | Reserved::Rescue
                        | Reserved::While
                        | Reserved::Until
                        | Reserved::And
                        | Reserved::Or
                        | Reserved::Then
                        | Reserved::End
                ),
                _ => true,
            }
        } else {
            matches!(
                tok.kind,
                TokenKind::GlobalVar(_)
                    | TokenKind::InstanceVar(_)
                    | TokenKind::ClassVar(_)
                    | TokenKind::SpecialVar(_)
                    | TokenKind::StringLit(_)
            )
        }
    }
}
