use super::*;

// Parse
impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    /// Parse char literals.
    pub(super) fn parse_char_literal(&mut self) -> Result<Node, LexerErr> {
        let loc = self.loc();
        let mut s = RubyString::new();
        self.lexer.read_char_literal(&mut s)?;
        Ok(Node::new_string(s, loc.merge(self.prev_loc)))
    }

    /// Parse string literals.
    /// Adjacent string literals are to be combined.
    pub(super) fn parse_string_literal(&mut self, mut s: RubyString) -> Result<Node, LexerErr> {
        let loc = self.prev_loc();
        loop {
            match self.peek_no_term()?.kind {
                TokenKind::StringLit(next_s) => {
                    self.get()?;
                    s += &next_s;
                }
                TokenKind::OpenString(next_s, delimiter, level) => {
                    self.get()?;
                    s += next_s.as_str();
                    return self.parse_interporated_string_literal(s, delimiter, level);
                }
                _ => break,
            }
        }
        Ok(Node::new_string(s, loc))
    }

    pub(super) fn parse_interporated_string_literal(
        &mut self,
        s: RubyString,
        delimiter: Option<char>,
        level: usize,
    ) -> Result<Node, LexerErr> {
        let start_loc = self.prev_loc();
        let mut nodes = vec![Node::new_string(s, start_loc)];
        loop {
            self.parse_template(&mut nodes)?;
            let tok = self
                .lexer
                .read_string_literal_double(None, delimiter, level)?;
            let mut loc = tok.loc();
            match tok.kind {
                TokenKind::StringLit(mut name) => {
                    loop {
                        match self.peek_no_term()?.kind {
                            TokenKind::StringLit(next_s) => {
                                let t = self.get()?;
                                name += &next_s;
                                loc = loc.merge(t.loc);
                            }
                            TokenKind::OpenString(next_s, _, _) => {
                                let t = self.get()?;
                                name += next_s.as_str();
                                loc = loc.merge(t.loc);
                                break;
                            }
                            _ => {
                                nodes.push(Node::new_string(name, loc));
                                return Ok(Node::new_interporated_string(
                                    nodes,
                                    start_loc.merge(loc),
                                ));
                            }
                        }
                    }
                    nodes.push(Node::new_string(name, loc));
                }
                TokenKind::OpenString(s, _, _) => {
                    nodes.push(Node::new_string(s.into(), loc));
                }
                _ => unreachable!("{:?}", tok),
            }
        }
    }

    pub(super) fn parse_percent_notation(&mut self) -> Result<Node, LexerErr> {
        let tok = self.lexer.get_percent_notation()?;
        let loc = tok.loc;
        if let TokenKind::PercentNotation(kind, content) = tok.kind {
            match kind {
                // TODO: backslash-space must be valid in %w and %i.
                // e.g. "foo\ bar" => "foo bar"
                'w' => {
                    let ary = content
                        .split(|c| c == ' ' || c == '\n')
                        .filter(|x| x != &"")
                        .map(|x| Node::new_string(x.to_string().into(), loc))
                        .collect();
                    Ok(Node::new_array(ary, tok.loc))
                }
                'i' => {
                    let ary = content
                        .split(|c| c == ' ' || c == '\n')
                        .filter(|x| x != &"")
                        .map(|x| Node::new_symbol(x.to_owned(), loc))
                        .collect();
                    Ok(Node::new_array(ary, tok.loc))
                }
                'r' => {
                    let ary = vec![Node::new_string(content.into(), loc)];
                    let (op, free_format) = self.lexer.check_postfix();
                    Ok(Node::new_regexp(ary, op, free_format, tok.loc))
                }
                _ => Err(error_unexpected(loc, "Unsupported % notation.")),
            }
        } else if let TokenKind::StringLit(s) = tok.kind {
            Ok(Node::new_string(s, loc))
        } else if let TokenKind::OpenString(s, term, level) = tok.kind {
            let node = self.parse_interporated_string_literal(s.into(), term, level)?;
            Ok(node)
        } else {
            unreachable!("parse_percent_notation(): {:?}", tok.kind);
        }
    }

    pub(super) fn parse_heredocument(&mut self) -> Result<Node, LexerErr> {
        if self.lexer.trailing_space() {
            let loc = self.prev_loc();
            return Err(error_unexpected(loc, "Unexpectd <<."));
        }
        let (mode, start, end) = self.lexer.read_heredocument()?;
        let node = match mode {
            ParseMode::Single => Node::new_string(
                self.lexer.code[start..end].to_string().into(),
                Loc(start, end),
            ),
            ParseMode::Double => {
                let mut parser = self.new_with_range(start, end);
                let res = parser.here_double();
                res?
            }
            ParseMode::Command => {
                let mut parser = self.new_with_range(start, end);
                let res = parser.here_command();
                res?
            }
        };
        Ok(node)
    }

    fn here_double(&mut self) -> Result<Node, LexerErr> {
        let tok = self.lexer.read_string_literal_double(None, None, 0)?;
        let loc = tok.loc();
        let node = match tok.kind {
            TokenKind::StringLit(s) => Node::new_string(s, loc),
            TokenKind::OpenString(s, term, level) => {
                self.parse_interporated_string_literal(s.into(), term, level)?
            }
            _ => unreachable!(),
        };
        Ok(node)
    }

    fn here_command(&mut self) -> Result<Node, LexerErr> {
        let tok = self.lexer.read_command_literal(None, None, 0)?;
        let loc = tok.loc();
        let node = match tok.kind {
            TokenKind::CommandLit(s) => {
                let content = Node::new_string(s.into(), loc);
                Node::new_command(content)
            }
            TokenKind::OpenString(s, term, level) => {
                let content = self.parse_interporated_string_literal(s.into(), term, level)?;
                Node::new_command(content)
            }
            _ => unreachable!(),
        };
        Ok(node)
    }

    pub(super) fn parse_hash_literal(&mut self) -> Result<Node, LexerErr> {
        let mut kvp = vec![];
        let loc = self.prev_loc();
        loop {
            if self.consume_punct(Punct::RBrace)? {
                return Ok(Node::new_hash(kvp, loc.merge(self.prev_loc())));
            };
            let ident_loc = self.loc();
            let mut symbol_flag = false;
            let key = match self.peek()?.can_be_symbol() {
                Some(id) => {
                    let save = self.save_state();
                    self.get().unwrap();
                    if self.consume_punct(Punct::Colon)? {
                        symbol_flag = true;
                        Node::new_symbol(id.to_owned(), ident_loc)
                    } else {
                        self.restore_state(save);
                        self.parse_arg()?
                    }
                }
                None => self.parse_arg()?,
            };
            if !symbol_flag {
                self.expect_punct(Punct::FatArrow)?
            };
            let value = self.parse_arg()?;
            kvp.push((key, value));
            if !self.consume_punct(Punct::Comma)? {
                break;
            };
        }
        self.expect_punct(Punct::RBrace)?;
        Ok(Node::new_hash(kvp, loc.merge(self.prev_loc())))
    }

    pub(super) fn parse_symbol(&mut self) -> Result<Node, LexerErr> {
        let loc = self.prev_loc();
        if self.lexer.trailing_space() {
            return Err(error_unexpected(loc, "Unexpected ':'."));
        }
        // Symbol literal
        if let Some((id, ident_loc)) = self.lexer.read_symbol_literal()? {
            return Ok(Node::new_symbol(id, loc.merge(ident_loc)));
        };
        let token = self.get()?;
        let symbol_loc = self.prev_loc();
        let id = match token.kind {
            TokenKind::OpenString(s, term, level) => {
                let node = self.parse_interporated_string_literal(s.into(), term, level)?;
                let method = "to_sym".to_string();
                let loc = symbol_loc.merge(node.loc());
                return Ok(Node::new_mcall_noarg(node, method, false, loc));
            }
            TokenKind::StringLit(ident) => ident.to_owned().as_string()?,
            _ => return Err(error_unexpected(symbol_loc, "Expect identifier or string.")),
        };
        Ok(Node::new_symbol(id, loc.merge(self.prev_loc())))
    }

    pub(super) fn parse_regexp(&mut self) -> Result<Node, LexerErr> {
        let start_loc = self.prev_loc();
        let tok = self.lexer.get_regexp()?;
        let mut nodes = match tok.kind {
            TokenKind::Regex {
                body: s,
                postfix: op,
                free_format,
            } => {
                return Ok(Node::new_regexp(
                    vec![Node::new_string(s.into(), tok.loc)],
                    op,
                    free_format,
                    tok.loc,
                ));
            }
            TokenKind::OpenRegex(s) => vec![Node::new_string(s.into(), tok.loc)],
            _ => unreachable!(),
        };
        loop {
            self.parse_template(&mut nodes)?;
            let tok = self.lexer.get_regexp()?;
            let loc = tok.loc();
            match tok.kind {
                TokenKind::Regex {
                    body: s,
                    postfix: op,
                    free_format,
                } => {
                    nodes.push(Node::new_string(s.into(), loc));
                    return Ok(Node::new_regexp(
                        nodes,
                        op,
                        free_format,
                        start_loc.merge(loc),
                    ));
                }
                TokenKind::OpenRegex(s) => {
                    nodes.push(Node::new_string(s.into(), loc));
                }
                _ => unreachable!(),
            }
        }
    }

    pub(super) fn parse_lambda_literal(&mut self) -> Result<Node, LexerErr> {
        // Lambda literal
        let loc = self.prev_loc();

        self.scope.push(LvarScope::new_block(None));
        self.loop_stack.push(LoopKind::Block);
        let peek = self.peek()?.kind;
        let params = if peek == TokenKind::Punct(Punct::LBrace)
            || peek == TokenKind::Reserved(Reserved::Do)
        {
            vec![]
        } else if self.consume_punct(Punct::LParen)? {
            self.parse_formal_params(Punct::RParen)?
        } else {
            self.parse_formal_params(None)?
        };
        let body = if self.consume_punct(Punct::LBrace)? {
            let body = self.parse_comp_stmt()?;
            self.expect_punct(Punct::RBrace)?;
            body
        } else if self.consume_reserved(Reserved::Do)? {
            let body = self.parse_comp_stmt()?;
            self.expect_reserved(Reserved::End)?;
            body
        } else {
            let loc = self.loc();
            let tok = self.get()?;
            return Err(error_unexpected(
                loc,
                format!("Expected 'do' or '{{'. Actual:{:?}", tok.kind),
            ));
        };
        self.loop_stack.pop().unwrap();
        let lvar = self.scope.pop().unwrap().lvar;

        Ok(Node::new_lambda(params, body, lvar, loc))
    }

    /// Parse template (#{..}, #$s, #@a).
    fn parse_template(&mut self, nodes: &mut Vec<Node>) -> Result<(), LexerErr> {
        if self.consume_punct(Punct::LBrace)? {
            nodes.push(self.parse_comp_stmt()?);
            if !self.consume_punct(Punct::RBrace)? {
                let loc = self.prev_loc();
                return Err(error_unexpected(loc, "Expect '}'"));
            }
        } else {
            let tok = self.get()?;
            let loc = tok.loc();
            let node = match tok.kind {
                TokenKind::GlobalVar(name) => Node::new_global_var(name, loc),
                TokenKind::InstanceVar(name) => Node::new_instance_var(name, loc),
                _ => unreachable!("{:?}", tok),
            };
            nodes.push(node);
        };
        Ok(())
    }

    fn new_with_range(&self, pos: usize, end: usize) -> Self {
        let lexer = self.lexer.new_with_range(pos, end);
        Parser {
            lexer,
            path: self.path.clone(),
            prev_loc: Loc(0, 0),
            scope: self.scope.clone(),
            loop_stack: vec![],
            extern_context: self.extern_context.clone(),
            suppress_acc_assign: false,
            suppress_mul_assign: false,
            suppress_do_block: false,
            defined_mode: false,
        }
    }
}
