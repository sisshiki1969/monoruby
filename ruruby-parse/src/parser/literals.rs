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
                TokenKind::OpenString(next_s, term, level) => {
                    self.get()?;
                    s += next_s.as_str();
                    return self.parse_interporation(s, None, term, level);
                }
                _ => break,
            }
        }
        Ok(Node::new_string(s, loc))
    }

    pub(super) fn parse_interporation(
        &mut self,
        s: RubyString,
        open: Option<char>,
        term: Option<char>,
        mut level: usize,
    ) -> Result<Node, LexerErr> {
        let start_loc = self.prev_loc();
        let mut nodes = vec![Node::new_string(s, start_loc)];
        loop {
            self.parse_template(&mut nodes)?;
            let tok = self.lexer.read_string_literal_double(open, term, level)?;
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
                            TokenKind::OpenString(next_s, _, new_level) => {
                                level = new_level;
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

    pub(super) fn parse_interporation_array(
        &mut self,
        s: RubyString,
        v: &mut Vec<Node>,
        delimiter: Option<char>,
        level: usize,
    ) -> Result<(), LexerErr> {
        let start_loc = self.prev_loc();
        let mut nodes = vec![Node::new_string(s, start_loc)];
        loop {
            self.parse_template(&mut nodes)?;
            let tokens = self
                .lexer
                .read_string_literal_double_array(None, delimiter, level)?;
            //let mut loc = tok.loc();
            let mut cont_flag = false;
            for tok in tokens {
                let loc = tok.loc();
                match tok.kind {
                    TokenKind::StringLit(name) => {
                        nodes.push(Node::new_string(name, loc));
                        v.push(Node::new_interporated_string(
                            std::mem::take(&mut nodes),
                            start_loc.merge(self.prev_loc()),
                        ));
                        nodes.clear();
                    }
                    TokenKind::OpenString(s, _, _) => {
                        cont_flag = true;
                        nodes.push(Node::new_string(s.into(), loc));
                    }
                    _ => unreachable!("{:?}", tok),
                }
            }
            if !cont_flag {
                break;
            }
        }
        v.push(Node::new_interporated_string(
            nodes,
            start_loc.merge(self.prev_loc()),
        ));
        Ok(())
    }

    /// Parse % notation.
    ///
    /// - 'w' => Array of String
    /// - 'i' => Array of Symbol
    /// - 'r' => RegExp
    pub(super) fn parse_percent_notation(&mut self) -> Result<Node, LexerErr> {
        fn escape_space<F>(content: String, loc: Loc, f: F) -> Vec<Node>
        where
            F: Fn(String, Loc) -> Node,
        {
            let mut v = vec![];
            let mut elem = String::new();
            let mut escape = false;
            for c in content.chars() {
                if escape {
                    escape = false;
                    if c != ' ' {
                        elem.push('\\');
                    }
                    elem.push(c);
                } else if c == '\\' {
                    escape = true;
                } else if c == ' ' || c == '\n' {
                    if !elem.is_empty() {
                        v.push(f(elem, loc));
                        elem = String::new();
                    }
                } else {
                    elem.push(c);
                }
            }
            if !elem.is_empty() {
                v.push(f(elem, loc));
            }
            v
        }

        let (kind, open, term) = self.lexer.get_percent_notation()?;

        let tok = self.lexer.percent_notation_first_token(kind, open, term)?;

        let loc = tok.loc;

        return match kind {
            'q' => {
                if let TokenKind::StringLit(s) = tok.kind {
                    Ok(Node::new_string(s, loc))
                } else {
                    unreachable!()
                }
            }
            'Q' => match tok.kind {
                TokenKind::StringLit(s) => Ok(Node::new_string(s, loc)),
                TokenKind::OpenString(s, term, level) => {
                    self.parse_interporation(s.into(), open, term, level)
                }
                _ => unreachable!(),
            },
            'W' => match tok.kind {
                TokenKind::Array(tokens) => {
                    let mut v = vec![];
                    for tok in tokens {
                        match tok.kind {
                            TokenKind::StringLit(s) => v.push(Node::new_string(s, loc)),
                            TokenKind::OpenString(s, term, level) => {
                                self.parse_interporation_array(s.into(), &mut v, term, level)?;
                            }
                            _ => unreachable!(),
                        }
                    }
                    let v = v
                        .into_iter()
                        .filter(|n| {
                            if let NodeKind::String(s) = &n.kind {
                                !s.is_empty()
                            } else {
                                true
                            }
                        })
                        .collect::<Vec<_>>();
                    Ok(Node::new_array(v, tok.loc))
                }
                _ => unreachable!(),
            },
            'w' => {
                if let TokenKind::PercentNotation(content) = tok.kind {
                    let v = escape_space(content, loc, |s, loc| Node::new_string(s.into(), loc));
                    Ok(Node::new_array(v, tok.loc))
                } else {
                    unreachable!()
                }
            }
            'i' => {
                if let TokenKind::PercentNotation(content) = tok.kind {
                    let v = escape_space(content, loc, |s, loc| Node::new_symbol(s, loc));
                    Ok(Node::new_array(v, tok.loc))
                } else {
                    unreachable!()
                }
            }
            'r' => {
                if let TokenKind::PercentNotation(content) = tok.kind {
                    let ary = vec![Node::new_string(content.into(), loc)];
                    let op = self.lexer.check_postfix();
                    Ok(Node::new_regexp(ary, op, tok.loc))
                } else {
                    unreachable!()
                }
            }
            _ => Err(error_unexpected(loc, "Unsupported % notation.")),
        };
    }

    pub(super) fn parse_heredocument(&mut self) -> Result<Node, LexerErr> {
        if self.lexer.trailing_space() {
            let loc = self.prev_loc();
            return Err(error_unexpected(loc, "Unexpectd <<."));
        }
        let (mode, indent, start, end) = self.lexer.read_heredocument()?;
        let node = match mode {
            ParseMode::Single => Node::new_string(
                if indent == 0 {
                    self.lexer.code[start..end].to_string().into()
                } else {
                    let v: Vec<&str> = self.lexer.code[start..end]
                        .split('\n')
                        .map(|s| if s.is_empty() { &s[0..] } else { &s[indent..] })
                        .collect();
                    v.join("\n").into()
                },
                Loc(start, end),
            ),
            ParseMode::Double => {
                let mut parser = self.new_with_range(start, end);
                let res = parser.here_double(indent != 0);
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

    fn here_double(&mut self, squiggy: bool) -> Result<Node, LexerErr> {
        fn count_space(s: &str, start: usize) -> (usize, usize, bool) {
            let indent = s[start..]
                .chars()
                .take_while(|c| [' ', '\t'].contains(c))
                .count();
            match s[start + indent..].chars().next() {
                Some('\n') => (start, indent, false),
                _ => (start, indent, true),
            }
        }

        fn calc_indent(
            s: &str,
            is_head: bool,
            min_indent: &mut usize,
        ) -> Vec<(usize, usize, bool)> {
            let len = s.len();
            let mut v = s
                .char_indices()
                .filter_map(|(i, c)| {
                    if c == '\n' && i + 1 != len && &s[i + 1..].chars().next() != &Some('\n') {
                        Some(count_space(s, i + 1))
                    } else {
                        None
                    }
                })
                .rev()
                .collect::<Vec<_>>();
            if is_head {
                v.push(count_space(s, 0));
            }
            let indent = v
                .iter()
                .flat_map(|(_, indent, live)| if *live { Some(*indent) } else { None })
                .min()
                .unwrap_or(usize::MAX);
            if indent < *min_indent {
                *min_indent = indent;
            }
            v
        }

        let tok = self.lexer.read_string_literal_double(None, None, 0)?;
        let loc = tok.loc();
        let mut node = match tok.kind {
            TokenKind::StringLit(s) => Node::new_string(s, loc),
            TokenKind::OpenString(s, term, level) => {
                self.parse_interporation(s.into(), None, term, level)?
            }
            _ => unreachable!(),
        };
        if squiggy {
            let mut min_indent = usize::MAX;
            if let NodeKind::InterporatedString(nodes) = &mut node.kind {
                let mut v = vec![];
                for (idx, node) in nodes.iter().enumerate() {
                    if let NodeKind::String(s) = &node.kind {
                        v.push(calc_indent(s, idx == 0, &mut min_indent));
                    } else {
                        v.push(vec![]);
                    }
                }
                for (idx, node) in nodes.iter_mut().enumerate() {
                    if let NodeKind::String(s) = &mut node.kind {
                        for (i, indent, _) in &v[idx] {
                            if min_indent < *indent {
                                s.drain(*i..*i + min_indent);
                            } else {
                                s.drain(*i..*i + *indent);
                            }
                        }
                    }
                }
            } else if let NodeKind::String(s) = &mut node.kind {
                for (i, indent, _) in calc_indent(s, true, &mut min_indent) {
                    if min_indent < indent {
                        s.drain(i..i + min_indent);
                    } else {
                        s.drain(i..i + indent);
                    }
                }
            }
        }
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
                let content = self.parse_interporation(s.into(), None, term, level)?;
                Node::new_command(content)
            }
            _ => unreachable!(),
        };
        Ok(node)
    }

    pub(super) fn parse_hash_literal(&mut self, no_brace: bool) -> Result<Node, LexerErr> {
        let mut kvp = vec![];
        let loc = self.prev_loc();
        loop {
            if !no_brace && self.consume_punct(Punct::RBrace)? {
                return Ok(Node::new_hash(kvp, loc.merge(self.prev_loc())));
            }
            let mut key = self.parse_arg(false)?;
            if let Some(sym) = key.is_symbol_key()
                && self.consume_punct(Punct::Colon)?
            {
                key = Node::new_symbol(sym, key.loc());
            } else {
                self.expect_punct(Punct::FatArrow)?;
            }
            let value = self.parse_arg(false)?;
            kvp.push((key, value));
            if !self.consume_punct(Punct::Comma)? {
                break;
            };
        }
        if !no_brace {
            self.expect_punct(Punct::RBrace)?;
        }
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
                let node = self.parse_interporation(s.into(), None, term, level)?;
                let method = "to_sym".to_string();
                let loc = symbol_loc.merge(node.loc());
                return Ok(Node::new_mcall_noarg(node, method, false, loc));
            }
            TokenKind::StringLit(ident) => ident.to_owned().into_string()?,
            _ => return Err(error_unexpected(symbol_loc, "Expect identifier or string.")),
        };
        Ok(Node::new_symbol(id, loc.merge(self.prev_loc())))
    }

    pub(super) fn parse_regexp(&mut self) -> Result<Node, LexerErr> {
        let start_loc = self.prev_loc();
        let tok = self.lexer.get_regexp()?;
        let mut nodes = match tok.kind {
            TokenKind::Regex { body, postfix } => {
                return Ok(Node::new_regexp(
                    vec![Node::new_string(body.into(), tok.loc)],
                    postfix,
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
                TokenKind::Regex { body, postfix } => {
                    nodes.push(Node::new_string(body.into(), loc));
                    return Ok(Node::new_regexp(nodes, postfix, start_loc.merge(loc)));
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
                TokenKind::ClassVar(name) => Node::new_class_var(name, loc),
                TokenKind::SpecialVar(name) => Node::new_special_var(name, loc),
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
