use super::*;

impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    ///
    /// Parse arguments with parentheses.
    ///
    /// - f(arg0, arg1, ...)
    pub(super) fn parse_arguments_paren(
        &mut self,
        allow_block: bool,
    ) -> Result<Option<ArgList>, LexerErr> {
        if self.lexer.trailing_lparen() {
            assert!(self.consume_punct_no_term(Punct::LParen).unwrap());
            let arglist = self.parse_arglist_block(Punct::RParen, allow_block)?;
            Ok(Some(arglist))
        } else {
            Ok(None)
        }
    }

    ///
    /// Parse arguments without parentheses.
    ///
    /// - f arg0, arg1, ...
    pub(super) fn parse_arguments_unparen(
        &mut self,
        allow_block: bool,
    ) -> Result<Option<ArgList>, LexerErr> {
        if self.is_command() {
            let arglist = self.parse_arglist_block(None, allow_block)?;
            Ok(Some(arglist))
        } else {
            Ok(None)
        }
    }

    fn parse_arglist_block(
        &mut self,
        delimiter: impl Into<Option<Punct>>,
        allow_block: bool,
    ) -> Result<ArgList, LexerErr> {
        let mut arglist = self.parse_arglist(delimiter)?;
        if allow_block && let Some(actual_block) = self.parse_block()? {
            if arglist.block.is_some() {
                return Err(error_unexpected(
                    actual_block.loc(),
                    "Both block arg and actual block given.",
                ));
            }
            arglist.block = Some(actual_block);
        };
        Ok(arglist)
    }

    /// Parse argument list.
    /// arg, *splat_arg, kw: kw_arg, **double_splat_arg, &block <punct>
    /// punct: punctuator for terminating arg list. Set None for unparenthesized argument list.
    fn parse_arglist(&mut self, punct: impl Into<Option<Punct>>) -> Result<ArgList, LexerErr> {
        let punct = punct.into();
        let mut arglist = ArgList::default();
        if self.peek()?.kind == TokenKind::Punct(Punct::Comma) {
            return Ok(arglist);
        }
        let mut as_hash_splat = vec![];
        let mut kw_args = vec![];
        loop {
            if let Some(punct) = punct {
                if self.consume_punct(punct)? {
                    return Ok(arglist);
                }
            }
            if self.consume_punct(Punct::Range3)? {
                self.check_forwarding()?;
                arglist.forwarding = true;
            } else if self.consume_punct(Punct::Mul)? {
                // splat argument
                let loc = self.prev_loc();
                let array = self.parse_arg(false)?;
                arglist.splat = true;
                arglist.args.push(Node::new_splat(array, loc));
            } else if self.consume_punct(Punct::DMul)? {
                // double splat argument
                arglist.hash_splat.push(self.parse_arg(false)?);
            } else if self.consume_punct(Punct::BitAnd)? {
                // block argument
                arglist.block = Some(Box::new(self.parse_arg(false)?));
            } else {
                let node = self.parse_arg(false)?;
                if self.consume_punct(Punct::FatArrow)? {
                    let value = self.parse_arg(false)?;
                    if as_hash_splat.is_empty()
                        && let NodeKind::Symbol(sym) = &node.kind
                    {
                        // keyword args
                        kw_args.push((sym.to_string(), value));
                    } else {
                        if as_hash_splat.is_empty() {
                            as_hash_splat.extend(
                                std::mem::take(&mut kw_args)
                                    .into_iter()
                                    .map(|(id, value)| (Node::new_symbol(id, node.loc()), value)),
                            );
                        }
                        as_hash_splat.push((node, value));
                    }
                } else {
                    if let Some(id) = match &node.kind {
                        NodeKind::Ident(id, ..) | NodeKind::LocalVar(_, id) => {
                            if self.consume_punct_no_term(Punct::Colon)? {
                                // keyword args
                                Some(id.to_string())
                            } else {
                                None
                            }
                        }
                        NodeKind::Const {
                            toplevel: false,
                            parent: None,
                            prefix,
                            name: id,
                        } => {
                            if prefix.is_empty() && self.consume_punct_no_term(Punct::Colon)? {
                                Some(id.to_string())
                            } else {
                                None
                            }
                        }
                        NodeKind::String(id) => {
                            if self.consume_punct_no_term(Punct::Colon)? {
                                // keyword args
                                Some(id.to_string())
                            } else {
                                None
                            }
                        }
                        _ => None,
                    } {
                        if as_hash_splat.is_empty() {
                            kw_args.push((id, self.parse_arg(false)?));
                        } else {
                            as_hash_splat
                                .push((Node::new_symbol(id, node.loc), self.parse_arg(false)?));
                        }
                    } else {
                        arglist.args.push(node);
                    }
                }
            }
            if !self.consume_punct(Punct::Comma)? {
                break;
            } else {
                let loc = self.prev_loc();
                if arglist.block.is_some() {
                    return Err(error_unexpected(loc, "unexpected ','."));
                };
            }
        }

        if !as_hash_splat.is_empty() {
            arglist
                .hash_splat
                .push(Node::new_hash(as_hash_splat, self.prev_loc()));
        }
        if !kw_args.is_empty() {
            arglist.kw_args = kw_args;
        }

        if let Some(punct) = punct {
            self.consume_punct(punct)?;
        };
        Ok(arglist)
    }

    fn is_command(&mut self) -> bool {
        let tok = match self.peek_no_term() {
            Ok(tok) => tok,
            _ => return false,
        };
        if self.lexer.trailing_space() {
            match tok.kind {
                TokenKind::LineTerm | TokenKind::Eof => false,
                TokenKind::Punct(p) => match p {
                    Punct::LParen | Punct::LBracket | Punct::Scope | Punct::Arrow | Punct::Not => {
                        true
                    }
                    Punct::Colon
                    | Punct::Plus
                    | Punct::Minus
                    | Punct::Mul
                    | Punct::Div
                    | Punct::BitAnd
                    | Punct::Rem
                    | Punct::Shl => !self.lexer.has_trailing_space(&tok),
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
                        | Reserved::Else
                        | Reserved::Elsif
                        | Reserved::End
                ),
                _ => true,
            }
        } else {
            matches!(
                tok.kind,
                TokenKind::GlobalVar(_)
                    | TokenKind::InstanceVar(_)
                    | TokenKind::StringLit(_)
                    | TokenKind::FloatLit(_)
                    | TokenKind::BignumLit(_)
                    | TokenKind::IntegerLit(_)
            )
        }
    }
}
