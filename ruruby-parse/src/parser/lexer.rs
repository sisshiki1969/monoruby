use super::*;
use num::ToPrimitive;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LexerErr(pub ParseErrKind, pub Loc);

#[derive(Clone, PartialEq)]
pub(crate) enum ParseMode {
    Double,
    Single,
    Command,
}

use enum_iterator::all;

fn check_reserved(maybe_reserved: &str) -> Option<Reserved> {
    all::<Reserved>().find(|&reserved| maybe_reserved == reserved.as_str())
    //for reserved in all::<Reserved>() {
    //    if maybe_reserved == reserved.as_str() {
    //        return Some(reserved);
    //    }
    //}
    //None
}

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    token_start_pos: usize,
    pos: usize,
    heredoc_pos: usize,
    buf: Option<Token>,
    buf_skip_lt: Option<Token>,
    pub code: &'a str,
}

#[cfg(test)]
#[derive(Debug, Clone)]
pub(crate) struct LexerResult {
    pub(crate) tokens: Vec<Token>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
enum VarKind {
    Identifier,
    InstanceVar,
    ClassVar,
    GlobalVar,
}
#[derive(Debug, Clone, PartialEq)]
enum InterpolateState {
    Finished(RubyString),
    FinishedRegex { body: String, postfix: String },
    NewInterpolation(RubyString, usize), // (string, paren_level)
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(code: &'a str) -> Self {
        Lexer {
            token_start_pos: 0,
            pos: 0,
            heredoc_pos: 0,
            buf: None,
            buf_skip_lt: None,
            code,
        }
    }

    pub(crate) fn new_with_range(&self, pos: usize, end: usize) -> Self {
        Lexer {
            token_start_pos: pos,
            pos,
            heredoc_pos: 0,
            buf: None,
            buf_skip_lt: None,
            code: &self.code[..end],
        }
    }

    fn error_unexpected(&self, pos: usize) -> LexerErr {
        let loc = Loc(pos, pos);
        LexerErr(
            ParseErrKind::SyntaxError(format!(
                "Unexpected char. {:?}",
                self.code[pos..].chars().next().unwrap()
            )),
            loc,
        )
    }

    fn error_eof(pos: usize) -> LexerErr {
        let loc = Loc(pos, pos);
        LexerErr(ParseErrKind::UnexpectedEOF, loc)
    }

    fn error_parse(msg: &str, pos: usize) -> LexerErr {
        let loc = Loc(pos, pos);
        LexerErr(
            ParseErrKind::SyntaxError(format!("Parse error. '{}'", msg)),
            loc,
        )
    }
}

impl<'a> Lexer<'a> {
    fn current_slice(&self) -> &str {
        &self.code[self.token_start_pos..self.pos]
    }

    pub(crate) fn get_line(&self, pos: usize) -> usize {
        self.code[0..=pos].chars().filter(|ch| *ch == '\n').count() + 1
    }

    pub(crate) fn get_token(&mut self) -> Result<Token, LexerErr> {
        self.buf = None;
        self.buf_skip_lt = None;
        let tok = self.read_token()?;
        Ok(tok)
    }

    pub(crate) fn peek_token(&mut self) -> Result<Token, LexerErr> {
        if let Some(tok) = &self.buf {
            return Ok(tok.clone());
        };
        let save = self.save_state();
        let tok = self.read_token()?;
        self.restore_state(save);
        self.buf = Some(tok.clone());
        Ok(tok)
    }

    pub(crate) fn peek_token_skip_lt(&mut self) -> Result<Token, LexerErr> {
        if let Some(tok) = &self.buf_skip_lt {
            return Ok(tok.clone());
        };
        let save = self.save_state();
        let mut tok;
        loop {
            tok = self.read_token()?;
            if tok.is_eof() || !tok.is_line_term() {
                break;
            }
        }
        self.restore_state(save);
        self.buf_skip_lt = Some(tok.clone());
        Ok(tok)
    }

    /// Examine if the next char is a whitespace or not.
    pub(crate) fn trailing_space(&self) -> bool {
        match self.peek() {
            Some(ch) => ch.is_ascii_whitespace(),
            _ => false,
        }
    }

    /// Examine if the next char is '('.
    pub(crate) fn trailing_lparen(&self) -> bool {
        match self.peek() {
            Some(ch) => ch == '(',
            _ => false,
        }
    }

    /// Examine if the next char of the token is space.
    pub(crate) fn has_trailing_space(&self, tok: &Token) -> bool {
        match self
            .code
            .get(tok.loc.1 + 1..)
            .and_then(|s| s.chars().next())
        {
            Some(ch) => ch.is_ascii_whitespace(),
            _ => false,
        }
    }

    /// Get token as a regular expression.
    pub(crate) fn get_regexp(&mut self) -> Result<Token, LexerErr> {
        match self.read_regexp_sub()? {
            InterpolateState::FinishedRegex { body, postfix } => {
                Ok(self.new_regexlit(body, postfix))
            }
            InterpolateState::NewInterpolation(s, _) => Ok(self.new_open_reg(s.into_string()?)),
            _ => unreachable!(),
        }
    }

    pub(crate) fn save_state(&self) -> (usize, usize) {
        (self.token_start_pos, self.pos)
    }

    pub(crate) fn restore_state(&mut self, state: (usize, usize)) {
        self.token_start_pos = state.0;
        self.pos = state.1;
    }

    pub(crate) fn flush(&mut self) {
        self.buf = None;
        self.buf_skip_lt = None;
    }
}

impl<'a> Lexer<'a> {
    /// Read token.
    fn read_token(&mut self) -> Result<Token, LexerErr> {
        loop {
            self.token_start_pos = self.pos;
            if let Some(tok) = self.skip_whitespace() {
                if self.code[self.pos..].starts_with("=begin") {
                    self.goto_eol();
                    if !self.consume('\n') {
                        return Err(Lexer::error_eof(self.pos));
                    }
                    while !self.code[self.pos..].starts_with("=end") {
                        self.goto_eol();
                        if !self.consume('\n') {
                            return Err(Lexer::error_eof(self.pos));
                        }
                    }
                    self.goto_eol();
                    if !self.consume('\n') {
                        return Err(Lexer::error_eof(self.pos));
                    }
                }
                return Ok(tok);
            };
            self.token_start_pos = self.pos;
            let pos = self.pos;
            let ch = match self.get() {
                Ok(ch) => ch,
                Err(_) => return Ok(self.new_eof()),
            };

            if ch.is_ascii_alphabetic() || ch == '_' {
                return self.read_identifier(ch, VarKind::Identifier);
            } else if ch.is_numeric() {
                return self.read_number_literal(ch);
            } else if ch.is_ascii_punctuation() {
                match ch {
                    '#' => self.goto_eol(),
                    '"' => return self.read_string_literal_double(None, Some('\"'), 0),
                    '`' => return self.read_command_literal(None, Some('`'), 0),
                    '\'' => {
                        let s = self.read_string_literal_single(None, '\'', false)?;
                        return Ok(self.new_stringlit(s));
                    }
                    ';' => return Ok(self.new_punct(Punct::Semi)),
                    ':' => {
                        if self.consume(':') {
                            return Ok(self.new_punct(Punct::Scope));
                        } else {
                            return Ok(self.new_punct(Punct::Colon));
                        }
                    }
                    ',' => return Ok(self.new_punct(Punct::Comma)),
                    '+' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::Add)));
                        } else {
                            return Ok(self.new_punct(Punct::Plus));
                        }
                    }
                    '-' => {
                        if self.consume('>') {
                            return Ok(self.new_punct(Punct::Arrow));
                        } else if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::Sub)));
                        } else {
                            return Ok(self.new_punct(Punct::Minus));
                        }
                    }
                    '*' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::Mul)));
                        } else if self.consume('*') {
                            return Ok(self.new_punct(Punct::DMul));
                        } else {
                            return Ok(self.new_punct(Punct::Mul));
                        }
                    }
                    '%' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::Rem)));
                        } else {
                            return Ok(self.new_punct(Punct::Rem));
                        }
                    }
                    '/' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::Div)));
                        } else {
                            return Ok(self.new_punct(Punct::Div));
                        }
                    }
                    '(' => return Ok(self.new_punct(Punct::LParen)),
                    ')' => return Ok(self.new_punct(Punct::RParen)),
                    '^' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::BitXor)));
                        } else {
                            return Ok(self.new_punct(Punct::BitXor));
                        }
                    }
                    '~' => return Ok(self.new_punct(Punct::BitNot)),
                    '[' => return Ok(self.new_punct(Punct::LBracket)),
                    ']' => return Ok(self.new_punct(Punct::RBracket)),
                    '{' => return Ok(self.new_punct(Punct::LBrace)),
                    '}' => return Ok(self.new_punct(Punct::RBrace)),
                    '.' => {
                        if self.consume('.') {
                            if self.consume('.') {
                                return Ok(self.new_punct(Punct::Range3));
                            } else {
                                return Ok(self.new_punct(Punct::Range2));
                            }
                        } else {
                            return Ok(self.new_punct(Punct::Dot));
                        }
                    }
                    '?' => return Ok(self.new_punct(Punct::Question)),
                    '\\' => return Ok(self.new_punct(Punct::Backslash)),
                    '=' => {
                        if self.consume('=') {
                            if self.consume('=') {
                                return Ok(self.new_punct(Punct::TEq));
                            } else {
                                return Ok(self.new_punct(Punct::Eq));
                            }
                        } else if self.consume('>') {
                            return Ok(self.new_punct(Punct::FatArrow));
                        } else if self.consume('~') {
                            return Ok(self.new_punct(Punct::Match));
                        } else {
                            return Ok(self.new_punct(Punct::Assign));
                        }
                    }
                    '>' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::Ge));
                        } else if self.consume('>') {
                            if self.consume('=') {
                                return Ok(self.new_punct(Punct::AssignOp(BinOp::Shr)));
                            } else {
                                return Ok(self.new_punct(Punct::Shr));
                            }
                        } else {
                            return Ok(self.new_punct(Punct::Gt));
                        }
                    }
                    '<' => {
                        if self.consume('=') {
                            if self.consume('>') {
                                return Ok(self.new_punct(Punct::Cmp));
                            } else {
                                return Ok(self.new_punct(Punct::Le));
                            }
                        } else if self.consume('<') {
                            if self.consume('=') {
                                return Ok(self.new_punct(Punct::AssignOp(BinOp::Shl)));
                            } else {
                                return Ok(self.new_punct(Punct::Shl));
                            }
                        } else {
                            return Ok(self.new_punct(Punct::Lt));
                        }
                    }
                    '!' => {
                        if self.consume('=') {
                            return Ok(self.new_punct(Punct::Ne));
                        } else if self.consume('~') {
                            return Ok(self.new_punct(Punct::Unmatch));
                        } else {
                            return Ok(self.new_punct(Punct::Not));
                        }
                    }
                    '&' => {
                        if self.consume('&') {
                            if self.consume('=') {
                                return Ok(self.new_punct(Punct::AssignOp(BinOp::LAnd)));
                            } else {
                                return Ok(self.new_punct(Punct::LAnd));
                            }
                        } else if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::BitAnd)));
                        } else if self.consume('.') {
                            return Ok(self.new_punct(Punct::SafeNav));
                        } else {
                            return Ok(self.new_punct(Punct::BitAnd));
                        }
                    }
                    '|' => {
                        if self.consume('|') {
                            if self.consume('=') {
                                return Ok(self.new_punct(Punct::AssignOp(BinOp::LOr)));
                            } else {
                                return Ok(self.new_punct(Punct::LOr));
                            }
                        } else if self.consume('=') {
                            return Ok(self.new_punct(Punct::AssignOp(BinOp::BitOr)));
                        } else {
                            return Ok(self.new_punct(Punct::BitOr));
                        }
                    }
                    '@' => {
                        if self.consume('@') {
                            return self.read_identifier(None, VarKind::ClassVar);
                        } else {
                            return self.read_identifier(None, VarKind::InstanceVar);
                        }
                    }
                    '$' => return self.read_global_var(),
                    _ => return Err(self.error_unexpected(pos)),
                }
            } else {
                return self.read_identifier(ch, VarKind::Identifier);
            };
        }
    }

    fn read_global_var(&mut self) -> Result<Token, LexerErr> {
        if let Some(ch) = self.consume_numeric() {
            if ch == '0' {
                return Ok(self.new_global_var("$0"));
            }
            let mut id = ch as u32 - '0' as u32;
            while let Some(ch) = self.consume_numeric() {
                id = id * 10 + ch as u32 - '0' as u32;
            }
            Ok(self.new_special_var(id + 100))
        } else {
            let tok = match self.peek() {
                Some(ch) if ch.is_ascii_punctuation() => {
                    let ch = self.get()?;
                    match ch {
                        '&' => self.new_special_var(SPECIAL_LASTMATCH),
                        '\'' => self.new_special_var(SPECIAL_POSTMATCH),
                        ':' => self.new_special_var(SPECIAL_LOADPATH),
                        '"' => self.new_special_var(SPECIAL_LOADEDFEATURES),
                        _ => self.new_global_var(format!("${}", ch)),
                    }
                }
                _ => self.read_identifier(None, VarKind::GlobalVar)?,
            };
            Ok(tok)
        }
    }

    ///
    /// Read identifier. ('@@xx', '$x', '@x')
    ///
    fn read_identifier(
        &mut self,
        ch: impl Into<Option<char>>,
        var_kind: VarKind,
    ) -> Result<Token, LexerErr> {
        let ch = ch.into();
        // read identifier or reserved keyword
        let is_const = match ch {
            Some(ch) => ch.is_ascii_uppercase(),
            None => {
                // Builtin global variables.
                let pos = self.pos;
                match self.get() {
                    Ok(ch) => {
                        if ch.is_alphanumeric() || ch == '_' || ch == '&' || ch == '\'' {
                        } else {
                            return Err(self.error_unexpected(pos));
                        }
                    }
                    Err(_) => {
                        return Err(Self::error_eof(pos));
                    }
                };
                false
            }
        };
        self.consume_ident();
        let tok = self.current_slice();
        if var_kind == VarKind::Identifier && ch == Some('_') {
            let mut iter = tok.chars();
            iter.next();
            if let Some(ch1) = iter.next() {
                if ch1.is_ascii_digit() && iter.next().is_none() {
                    return Ok(self.new_numbered_param(ch1));
                }
            }
        }
        match var_kind {
            VarKind::InstanceVar => return Ok(self.new_instance_var(tok)),
            VarKind::ClassVar => return Ok(self.new_class_var(tok)),
            VarKind::GlobalVar => {
                if tok == "$LOAD_PATH" {
                    return Ok(self.new_special_var(SPECIAL_LOADPATH));
                } else if tok == "$LOADED_FEATURES" {
                    return Ok(self.new_special_var(SPECIAL_LOADEDFEATURES));
                } else {
                    return Ok(self.new_global_var(tok));
                }
            }
            _ => {}
        }

        match self.peek() {
            Some(ch) if (ch == '!' && self.peek2() != Some('=')) || ch == '?' => {
                self.get().unwrap();
            }
            _ => {}
        };
        let tok = self.current_slice();
        if let Some(reserved) = check_reserved(tok) {
            return Ok(self.new_reserved(reserved));
        };

        if is_const {
            Ok(self.new_const(tok))
        } else {
            Ok(self.new_ident(tok))
        }
    }

    /// Read method name.
    ///
    /// e.g. Foo? bar bar! baz= == != <= <=>
    /// In primary method call, assign-like method name(cf. foo= or Bar=) is not allowed.
    ///
    // メソッド定義名 : メソッド名 ｜ ( 定数識別子 | 局所変数識別子 ) "="
    // メソッド名 : 局所変数識別子
    //      | 定数識別子
    //      | ( 定数識別子 | 局所変数識別子 ) ( "!" | "?" )
    //      | 演算子メソッド名
    //      | キーワード
    // 演算子メソッド名 : “^” | “&” | “|” | “<=>” | “==” | “===” | “=~” | “>” | “>=” | “<” | “<=”
    //      | “<<” | “>>” | “+” | “-” | “*” | “/” | “%” | “**” | “~” | “+@” | “-@” | “[]” | “[]=” | “ʻ”
    pub(crate) fn read_method_name(
        &mut self,
        allow_assign_like: bool,
    ) -> Result<(String, Loc), LexerErr> {
        self.flush();
        while self.consume_whitespace() || self.consume_newline() {}
        self.token_start_pos = self.pos;
        let ch = self.get()?;
        if ch.is_ascii_punctuation() && ch != '_' {
            // re-definable operators
            // https://docs.ruby-lang.org/ja/latest/doc/spec=2foperator.html
            // |  ^  &  <=>  ==  ===  =~  >   >=  <   <=   <<  >>
            // +  -  *  /    %   **   ~   +@  -@  []  []=  ` ! != !~
            match ch {
                '/' | '%' | '&' | '|' | '^' | '~' | '`' => {}
                '*' => {
                    self.consume('*');
                }
                '+' | '-' => {
                    self.consume('@');
                }
                '<' => if self.consume('<') || (self.consume('=') && self.consume('>')) {},
                '>' => if self.consume('>') || self.consume('=') {},
                '=' => {
                    if self.consume('=') {
                        self.consume('=');
                    } else if self.consume('~') {
                    } else {
                        return Err(self.error_unexpected(self.pos));
                    };
                }
                '!' => {
                    if self.consume('=') || self.consume('~') {};
                }
                '[' => {
                    if self.consume(']') {
                        self.consume('=');
                    } else {
                        return Err(self.error_unexpected(self.token_start_pos));
                    }
                }
                _ => return Err(self.error_unexpected(self.token_start_pos)),
            };
        } else if ch.is_ascii_alphabetic() || ch == '_' || !ch.is_ascii() {
            self.consume_ident();
            match self.peek() {
                Some(ch)
                    if (ch == '!' && self.peek2() != Some('='))
                        || ch == '?'
                        || (allow_assign_like && ch == '=' && self.peek2() != Some('>')) =>
                {
                    self.get().unwrap();
                }
                _ => {}
            };
        } else {
            return Err(self.error_unexpected(self.token_start_pos));
        };
        Ok((
            self.current_slice().to_string(),
            Loc(self.token_start_pos, self.pos),
        ))
    }

    pub(crate) fn read_symbol_literal(&mut self) -> Result<Option<(String, Loc)>, LexerErr> {
        self.flush();
        self.token_start_pos = self.pos;
        let ch = self.peek().ok_or_else(|| self.error_unexpected(self.pos))?;
        match ch {
            '@' => {
                self.consume('@');
                self.consume('@');
                let ch = self.get()?;
                if !ch.is_ascii_alphabetic() && ch != '_' {
                    return Err(self.error_unexpected(self.pos - ch.len_utf8()));
                }
                self.consume_ident();
                Ok(Some((
                    self.current_slice().to_string(),
                    Loc(self.token_start_pos, self.pos),
                )))
            }
            '\"' | '\'' => Ok(None),
            _ => self.read_method_name(true).map(Some),
        }
    }

    /// Check method name extension.
    /// Parse "xxxx=" as a valid mathod name.
    /// "xxxx!=" or "xxxx?=" is invalid.
    pub(crate) fn read_method_ext(&mut self, s: String) -> Result<String, LexerErr> {
        self.flush();
        let id =
            if !(s.ends_with(&['!', '?'][..])) && self.peek2() != Some('>') && self.consume('=') {
                format!("{}=", s)
            } else {
                s
            };
        Ok(id)
    }

    /// Read number literal.
    fn read_number_literal(&mut self, ch: char) -> Result<Token, LexerErr> {
        if ch == '0' {
            if self.consume('x') {
                return self.read_hex_number();
            } else if self.consume('o') {
                return self.read_octal_number();
            } else if self.consume('b') {
                return self.read_bin_number();
            }
        };
        let mut s = ch.to_string();
        let mut float_flag = false;
        loop {
            if let Some(ch) = self.consume_numeric() {
                s.push(ch);
            } else if self.consume('_') {
            } else if !float_flag && self.peek() == Some('.') {
                match self.peek2() {
                    Some(ch) if ch.is_ascii() && ch.is_numeric() => {
                        self.get()?;
                        self.get()?;
                        float_flag = true;
                        s.push('.');
                        s.push(ch);
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }
        if self.consume('e') || self.consume('E') {
            s.push('e');
            if !self.consume('+') && self.consume('-') {
                s.push('-');
            }
            if let Some(ch) = self.consume_numeric() {
                s.push(ch);
            } else {
                return Err(self.error_unexpected(self.pos));
            }
            loop {
                if let Some(ch) = self.consume_numeric() {
                    s.push(ch);
                } else if self.consume('_') {
                } else {
                    break;
                }
            }
            float_flag = true;
        }
        let number = if float_flag {
            match s.parse::<f64>() {
                Ok(f) => NReal::Float(f),
                Err(err) => return Err(Self::error_parse(&format!("{:?}", err), self.pos)),
            }
        } else {
            match BigInt::parse_bytes(s.as_bytes(), 10) {
                Some(b) => match b.to_i64() {
                    Some(i) => NReal::Integer(i),
                    None => NReal::Bignum(b),
                },
                None => return Err(Self::error_parse("Invalid number literal.", self.pos)),
            }
        };
        if self.consume('i') {
            Ok(self.new_imaginarylit(number))
        } else {
            match number {
                NReal::Bignum(n) => Ok(self.new_bignumlit(n)),
                NReal::Integer(i) => Ok(self.new_numlit(i)),
                NReal::Float(f) => Ok(self.new_floatlit(f)),
            }
        }
    }

    /// Read hexadecimal number.
    fn read_hex_number(&mut self) -> Result<Token, LexerErr> {
        let start_pos = self.pos;
        self.expect_hex()
            .map_err(|_| Self::error_parse("Numeric literal without digits.", self.pos))?;

        while self.consume_hex().is_some() || self.consume('_') {}

        let s = &self.code[start_pos..self.pos];
        match BigInt::parse_bytes(s.as_bytes(), 16) {
            Some(b) => match b.to_i64() {
                Some(i) => Ok(self.new_numlit(i)),
                None => Ok(self.new_bignumlit(b)),
            },
            None => Err(Self::error_parse("Invalid hex number literal.", self.pos)),
        }
    }

    /// Read octal number.
    fn read_octal_number(&mut self) -> Result<Token, LexerErr> {
        let mut val = match self.peek() {
            Some(ch @ '0'..='7') => ch as u64 - '0' as u64,
            Some(_) => {
                return Err(self.error_unexpected(self.pos));
            }
            None => return Err(Self::error_eof(self.pos)),
        };
        self.get()?;
        loop {
            match self.peek() {
                Some(ch @ '0'..='7') => val = val * 8 + (ch as u64 - '0' as u64),
                Some('_') => {}
                _ => break,
            }
            self.get()?;
        }
        Ok(self.new_numlit(val as i64))
    }

    /// Read binary number.
    fn read_bin_number(&mut self) -> Result<Token, LexerErr> {
        let mut val = match self.peek() {
            Some(ch @ '0'..='1') => ch as u64 - '0' as u64,
            Some(_) => {
                return Err(self.error_unexpected(self.pos));
            }
            None => return Err(Self::error_eof(self.pos)),
        };
        self.get()?;
        loop {
            match self.peek() {
                Some(ch @ '0'..='1') => val = val * 2 + (ch as u64 - '0' as u64),
                Some('_') => {}
                _ => break,
            }
            self.get()?;
        }
        Ok(self.new_numlit(val as i64))
    }

    /// Read string literal ("..", %Q{..}, %{..})
    pub(crate) fn read_string_literal_double(
        &mut self,
        open: Option<char>,
        term: Option<char>,
        level: usize,
    ) -> Result<Token, LexerErr> {
        match self.read_interpolate(open, term, level)? {
            InterpolateState::Finished(s) => Ok(self.new_stringlit(s)),
            InterpolateState::NewInterpolation(s, level) => {
                Ok(self.new_open_string(s.into_string()?, term, level))
            }
            _ => unreachable!(),
        }
    }

    /// Read command literal (`..`)
    pub(crate) fn read_command_literal(
        &mut self,
        open: Option<char>,
        term: Option<char>,
        level: usize,
    ) -> Result<Token, LexerErr> {
        match self.read_interpolate(open, term, level)? {
            InterpolateState::Finished(s) => self.new_commandlit(s),
            InterpolateState::NewInterpolation(s, level) => {
                Ok(self.new_open_command(s.into_string()?, term, level))
            }
            _ => unreachable!(),
        }
    }

    /// Read interpolation string with `open` as opening char and `term` as a terminator.
    fn read_interpolate(
        &mut self,
        open: Option<char>,
        term: Option<char>,
        mut level: usize,
    ) -> Result<InterpolateState, LexerErr> {
        let mut s = RubyString::new();
        loop {
            let ch = match self.get() {
                Ok(c) => c,
                Err(err) => {
                    if term.is_none() {
                        return Ok(InterpolateState::Finished(s));
                    } else {
                        return Err(err);
                    }
                }
            };
            match ch {
                c if open == Some(c) => {
                    s.push_char(c);
                    level += 1;
                }
                c if Some(c) == term => {
                    if level == 0 {
                        return Ok(InterpolateState::Finished(s));
                    } else {
                        s.push_char(c);
                        level -= 1;
                    }
                }
                '\\' => {
                    // continuation line
                    if self.consume_newline() {
                        continue;
                    };
                    self.read_escaped_char(&mut s)?;
                }
                '#' => match self.peek() {
                    // string interpolation
                    Some(ch) if ch == '{' || ch == '$' || ch == '@' => {
                        return Ok(InterpolateState::NewInterpolation(s, level))
                    }
                    _ => s.push_char('#'),
                },
                '\n' => {
                    s.push_char('\n');
                    if self.heredoc_pos > self.pos {
                        self.pos = self.heredoc_pos;
                    }
                }
                c => s.push_char(c),
            }
        }
    }

    /// Read string literal '..' or %q{..}
    fn read_string_literal_single(
        &mut self,
        open: Option<char>,
        term: char,
        escape_backslash: bool,
    ) -> Result<String, LexerErr> {
        let mut s = "".to_string();
        let mut level = 0;
        loop {
            match self.get()? {
                c if open == Some(c) => {
                    s.push(c);
                    level += 1;
                }
                c if c == term => {
                    if level == 0 {
                        return Ok(s);
                    } else {
                        s.push(c);
                        level -= 1;
                    }
                }
                '\\' => {
                    // continuation line
                    if self.consume_newline() {
                        continue;
                    };
                    let c = self.get()?;
                    if c == '\'' {
                        s.push('\'');
                    } else if c == '\\' {
                        s.push('\\');
                        if escape_backslash {
                            s.push('\\');
                        }
                    } else {
                        s.push('\\');
                        s.push(c);
                    }
                }
                c => s.push(c),
            }
        }
    }

    fn read_unicode_escape_sequence(
        &mut self,
        s: &mut RubyString,
        strict: bool,
    ) -> Result<(), LexerErr> {
        let mut u = 0;
        if strict {
            for _ in 0..4 {
                u = 16 * u
                    + self.expect_hex().map_err(|_| {
                        Self::error_parse("Invalid Unicode escape sequence.", self.pos)
                    })?;
            }
        } else {
            let mut i = 0;
            while let Some(h) = self.consume_hex() {
                if i == 6 {
                    return Err(Self::error_parse(
                        "Invalid Unicode escape sequence; maximum length is 6 digits.",
                        self.pos,
                    ));
                }
                u = u * 16 + h;
                i += 1;
            }
        };
        if let Some(u) = char::from_u32(u) {
            s.push_char(u);
            Ok(())
        } else {
            Err(Self::error_parse(
                "Invalid Unicode escape sequence",
                self.pos,
            ))
        }
    }

    /// Read char literal.
    pub(crate) fn read_char_literal(&mut self, buf: &mut RubyString) -> Result<(), LexerErr> {
        let c = self.get()?;
        self.flush();
        if c == '\\' {
            self.read_escaped_char(buf)?;
        } else {
            buf.push_char(c);
        };
        Ok(())
    }

    /// Convert postfix of regular expression.
    pub(crate) fn check_postfix(&mut self) -> String {
        let mut s = "".to_string();
        loop {
            if self.consume('i') {
                // ignore case
                s.push('i');
            } else if self.consume('m') {
                // match "." for newline
                s.push('m');
            } else if self.consume('x') {
                // free format mode
                s.push('x');
                //free_format = true;
            } else if self.consume('o') {
                // expand "#{}" only once
                //s.push('o');
            } else if self.consume('u') {
                // Encoding+ utf-8
            } else if self.consume('n') {
                // Encoding+ ASCII-8bit
            } else {
                break;
            };
        }
        s
    }

    /// Scan as regular expression.
    fn read_regexp_sub(&mut self) -> Result<InterpolateState, LexerErr> {
        let mut body = "".to_string();
        let mut char_class = 0;
        loop {
            match self.get()? {
                '/' => {
                    let postfix = self.check_postfix();
                    return Ok(InterpolateState::FinishedRegex { body, postfix });
                }
                '[' => {
                    char_class += 1;
                    body.push('[');
                }
                ']' => {
                    char_class -= 1;
                    body.push(']');
                }
                '\\' => {
                    let ch = self.get()?;
                    match ch {
                        'a' => body += "\\a",
                        // '\b' is valid only in the inner of character class. Otherwise, shoud be treated as "\x08".
                        'b' => body += if char_class == 0 { "\\b" } else { "\\x08" },
                        'e' => body += "\\x1b",
                        'f' => body += "\\f",
                        'n' => body += "\\n",
                        'r' => body += "\\r",
                        's' => body += "\\s",
                        't' => body += "\\t",
                        'v' => body += "\\v",
                        'x' => {
                            // CRuby allows not only "\x1f" but "\xf".
                            let mut x = 0;
                            if let Some(c) = self.consume_hex() {
                                x += c;
                                if let Some(c) = self.consume_hex() {
                                    x = x * 16 + c;
                                }
                                body += &format!("\\x{:02x}", x);
                            } else {
                                body += "\\x";
                            }
                        }
                        _ => {
                            body.push('\\');
                            if ('1'..='9').contains(&ch) && !self.peek_digit() {
                                body.push(ch);
                            } else if ('0'..='7').contains(&ch) {
                                // TODO: It is necessary to count capture groups
                                // to determine whether backref or octal digit.
                                // Current impl. may cause problems.
                                let hex = format!("x{:02x}", self.consume_tri_octal(ch).unwrap());
                                body += &hex;
                            } else {
                                body.push(ch);
                            }
                        }
                    };
                }
                '#' => match self.peek() {
                    Some(ch) if ch == '{' || ch == '$' || ch == '@' => {
                        return Ok(InterpolateState::NewInterpolation(body.into(), 0))
                    }
                    _ => body.push('#'),
                },
                c => body.push(c),
            }
        }
    }

    pub(crate) fn get_percent_notation(&mut self) -> Result<Token, LexerErr> {
        let pos = self.pos;
        let c = self.get()?;
        let (kind, delimiter) = match c {
            'q' | 'Q' | 'x' | 'r' | 'w' | 'W' | 's' | 'i' | 'I' => {
                let pos = self.pos;
                let delimiter = self.get()?;
                if delimiter.is_ascii_alphanumeric() {
                    return Err(self.error_unexpected(pos));
                }
                (Some(c), delimiter)
            }
            delimiter if !c.is_ascii_alphanumeric() => (None, delimiter),
            _ => return Err(self.error_unexpected(pos)),
        };
        let (open, term) = match delimiter {
            '(' => (Some('('), ')'),
            '{' => (Some('{'), '}'),
            '[' => (Some('['), ']'),
            '<' => (Some('<'), '>'),
            ' ' | '\n' => match kind {
                Some('i') | Some('I') | Some('w') | Some('W') => {
                    return Err(self.error_unexpected(self.pos - 1))
                }
                _ => (None, delimiter),
            },
            ch => (None, ch),
        };

        match kind {
            Some('q') => {
                let s = self.read_string_literal_single(open, term, false)?;
                Ok(self.new_stringlit(s))
            }
            Some('Q') | None => Ok(self.read_string_literal_double(open, Some(term), 0)?),
            Some('r') => {
                let s = self.read_string_literal_single(open, term, true)?;
                Ok(self.new_percent('r', s))
            }
            Some(kind) => {
                let s = self.read_string_literal_single(open, term, false)?;
                Ok(self.new_percent(kind, s))
            }
        }
    }

    fn read_escaped_char(&mut self, buf: &mut RubyString) -> Result<(), LexerErr> {
        let ch = match self.get()? {
            c @ '\'' | c @ '"' | c @ '?' | c @ '\\' => c,
            'a' => '\x07',
            'b' => '\x08',
            'e' => '\x1b',
            'f' => '\x0c',
            'n' => '\x0a',
            'r' => '\x0d',
            's' => '\x20',
            't' => '\x09',
            'v' => '\x0b',
            'x' => {
                let c1 = self.expect_hex()?;
                let c2 = self.consume_hex();
                let c = if let Some(c2) = c2 { c1 * 16 + c2 } else { c1 };
                if c > 0x7f {
                    buf.push_byte(c as u8);
                } else {
                    buf.push_char(char::from_u32(c).unwrap());
                }
                return Ok(());
            }
            'u' => {
                if self.consume('{') {
                    loop {
                        while self.consume(' ') {}
                        if self.consume('}') {
                            break;
                        }
                        self.read_unicode_escape_sequence(buf, false)?;
                    }
                } else {
                    self.read_unicode_escape_sequence(buf, true)?
                }
                return Ok(());
            }
            c if ('0'..='7').contains(&c) => {
                if let Some(num) = self.consume_tri_octal(c) {
                    num as char
                } else {
                    c
                }
            }
            c => c,
        };
        buf.push_char(ch);
        Ok(())
    }

    pub(crate) fn read_heredocument(&mut self) -> Result<(ParseMode, usize, usize), LexerErr> {
        #[derive(Clone, PartialEq)]
        enum TermMode {
            Normal,
            AllowIndent,
            // TODO currently, not supported.
            Squiggly,
        }

        let term_mode = if self.consume('-') {
            TermMode::AllowIndent
        } else if self.consume('~') {
            TermMode::Squiggly
        } else {
            TermMode::Normal
        };
        let (parse_mode, no_term) = if self.consume('\'') {
            (ParseMode::Single, false)
        } else if self.consume('\"') {
            (ParseMode::Double, false)
        } else if self.consume('`') {
            (ParseMode::Command, false)
        } else {
            (ParseMode::Double, true)
        };
        let delimiter = self.consume_ident();
        if delimiter.is_empty() {
            return Err(self.error_unexpected(self.pos));
        }
        let term_ch = match parse_mode {
            ParseMode::Single => '\'',
            ParseMode::Double => '\"',
            ParseMode::Command => '`',
        };
        if !no_term && !self.consume(term_ch) {
            return Err(Self::error_parse(
                "Unterminated here document identifier.",
                self.pos,
            ));
        }
        let save = self.save_state();
        self.goto_eol();
        self.get()?;
        if self.heredoc_pos > self.pos {
            self.pos = self.heredoc_pos;
        }
        let heredoc_start = self.pos;
        let mut heredoc_end = self.pos;
        loop {
            let start = self.pos;
            self.goto_eol();
            let end = self.pos;
            let next = self.get();
            let line = &self.code[start..end];
            if term_mode == TermMode::AllowIndent || term_mode == TermMode::Squiggly {
                if line.trim_start() == &self.code[delimiter.clone()] {
                    break;
                }
            } else if line == &self.code[delimiter.clone()] {
                break;
            }

            if next.is_err() {
                return Err(Self::error_parse(
                    &format!(
                        r#"Can not find string "{}" anywhere before EOF."#,
                        &self.code[delimiter]
                    ),
                    self.pos,
                ));
            };
            heredoc_end = end + 1;
            //res += line;
            //res.push('\n');
        }
        self.heredoc_pos = self.pos;
        self.restore_state(save);
        Ok((parse_mode, heredoc_start, heredoc_end))
    }
}

// Low level API
impl<'a> Lexer<'a> {
    /// Peek the next char.
    /// Returns Some(char) or None if the cursor reached EOF.
    fn peek(&self) -> Option<char> {
        self.code.get(self.pos..)?.chars().next()
    }

    /// Peek the next next char.
    /// Returns Some(char) or None if the cursor reached EOF.
    fn peek2(&self) -> Option<char> {
        let mut iter = self.code.get(self.pos..)?.chars();
        iter.next()?;
        iter.next()
    }

    /// Get one char and move to the next.
    /// Returns Ok(char) or ParseErr if the cursor reached EOF.
    fn get(&mut self) -> Result<char, LexerErr> {
        match self.peek() {
            Some(ch) => {
                self.pos += ch.len_utf8();
                Ok(ch)
            }
            _ => Err(Self::error_eof(self.pos)),
        }
    }

    /// Consume the next char, if the char is equal to the given one.
    /// Return true if the char was consumed.
    pub(super) fn consume(&mut self, expect: char) -> bool {
        match self.peek() {
            Some(ch) if ch == expect => {
                self.pos += ch.len_utf8();
                true
            }
            _ => false,
        }
    }

    /// Consume continuous ascii_alphanumeric or underscore characters.
    /// Return consumed string.
    fn consume_ident(&mut self) -> Range<usize> {
        let start = self.pos;
        loop {
            match self.peek() {
                Some(ch) if ch.is_ascii_alphanumeric() || ch == '_' || !ch.is_ascii() => {
                    self.get().unwrap()
                }
                _ => break,
            };
        }
        start..self.pos
    }

    fn consume_newline(&mut self) -> bool {
        if self.consume('\n') {
            if self.heredoc_pos > self.pos {
                self.pos = self.heredoc_pos;
            };
            true
        } else {
            false
        }
    }

    /// Consume continue line. ("\\n")
    /// Return true if consumed.
    fn consume_cont_line(&mut self) -> bool {
        if self.peek2() == Some('\n') && self.peek() == Some('\\') {
            self.pos += 2;
            true
        } else {
            false
        }
    }

    /// Consume the next char, if the char is numeric char.
    /// Return Some(ch) if the token (ch) was consumed.
    fn consume_numeric(&mut self) -> Option<char> {
        match self.peek() {
            Some(ch) if ch.is_ascii() && ch.is_numeric() => {
                self.pos += ch.len_utf8();
                Some(ch)
            }
            _ => None,
        }
    }

    /// Consume the next char, if the char is '0'-'7'.
    /// Return Some(<octal_digit>) if the char was consumed.
    fn consume_octal(&mut self) -> Option<u8> {
        match self.peek() {
            Some(ch) if ('0'..='7').contains(&ch) => {
                self.pos += ch.len_utf8();
                Some(ch as u8 - b'0')
            }
            _ => None,
        }
    }

    /// Consume the next char, if the char is '0'-'9' or 'a'-'f'.
    /// Return Some(<hex_digit>) if the char was consumed.
    fn consume_hex(&mut self) -> Option<u32> {
        self.expect_hex().ok()
    }

    fn expect_hex(&mut self) -> Result<u32, LexerErr> {
        let n = match self.peek() {
            Some(ch) => match ch {
                ch @ '0'..='9' => ch as u32 - '0' as u32,
                ch @ 'a'..='f' => ch as u32 - 'a' as u32 + 10,
                ch @ 'A'..='F' => ch as u32 - 'A' as u32 + 10,
                _ => return Err(self.error_unexpected(self.pos)),
            },
            _ => return Err(Self::error_eof(self.pos)),
        };
        self.pos += 1;
        Ok(n)
    }

    /// Consume the next char, if the char is ascii-whitespace char.
    /// Return whether some whitespace characters were consumed or not.
    fn consume_whitespace(&mut self) -> bool {
        match self.peek() {
            Some(ch) if ch.is_ascii_whitespace() => {
                self.pos += ch.len_utf8();
                true
            }
            _ => false,
        }
    }

    fn consume_tri_octal(&mut self, first_ch: char) -> Option<u8> {
        let mut o = first_ch as u8 - b'0';
        for _ in 0..2 {
            match self.consume_octal() {
                Some(n) => o = o.wrapping_mul(8) + n,
                None => break,
            };
        }
        Some(o)
    }

    /// Peek the next char.
    /// Returns Some(char) or None if the cursor reached EOF.
    fn peek_digit(&self) -> bool {
        match self.peek() {
            Some(ch) => ch.is_ascii_digit(),
            None => false,
        }
    }

    /// Skip whitespace, newline and continuation line.
    ///
    /// Returns Some(LineTerm) if some newline characters were skipped.
    fn skip_whitespace(&mut self) -> Option<Token> {
        let mut res = None;
        loop {
            if self.consume_newline() {
                res = Some(self.new_line_term());
            } else if !self.consume_cont_line() && !self.consume_whitespace() {
                self.token_start_pos = self.pos;
                return res;
            }
        }
    }

    fn goto_eol(&mut self) {
        loop {
            match self.peek() {
                Some('\n') | None => return,
                Some(ch) => self.pos += ch.len_utf8(),
            };
        }
    }

    fn cur_loc(&self) -> Loc {
        let end = std::cmp::max(self.token_start_pos, self.pos - 1);
        Loc(self.token_start_pos, end)
    }
}

impl<'a> Lexer<'a> {
    fn new_ident(&self, ident: impl Into<String>) -> Token {
        Token::new_ident(ident.into(), self.cur_loc())
    }

    fn new_numbered_param(&self, i: char) -> Token {
        Token::new_numbered_param(i as u32 - '0' as u32, self.cur_loc())
    }

    fn new_instance_var(&self, ident: impl Into<String>) -> Token {
        Annot::new(TokenKind::InstanceVar(ident.into()), self.cur_loc())
    }

    fn new_class_var(&self, ident: impl Into<String>) -> Token {
        Annot::new(TokenKind::ClassVar(ident.into()), self.cur_loc())
    }

    fn new_global_var(&self, ident: impl Into<String>) -> Token {
        Annot::new(TokenKind::GlobalVar(ident.into()), self.cur_loc())
    }

    fn new_special_var(&self, id: u32) -> Token {
        Annot::new(TokenKind::SpecialVar(id), self.cur_loc())
    }

    fn new_const(&self, ident: impl Into<String>) -> Token {
        Annot::new(TokenKind::Const(ident.into()), self.cur_loc())
    }

    fn new_reserved(&self, ident: Reserved) -> Token {
        Annot::new(TokenKind::Reserved(ident), self.cur_loc())
    }

    fn new_numlit(&self, num: i64) -> Token {
        Token::new_intlit(num, self.cur_loc())
    }

    fn new_bignumlit(&self, num: BigInt) -> Token {
        Token::new_bignumlit(num, self.cur_loc())
    }

    fn new_floatlit(&self, num: f64) -> Token {
        Token::new_floatlit(num, self.cur_loc())
    }

    fn new_imaginarylit(&self, num: NReal) -> Token {
        Token::new_imaginarylit(num, self.cur_loc())
    }

    fn new_stringlit(&self, string: impl Into<RubyString>) -> Token {
        Annot::new(TokenKind::StringLit(string.into()), self.cur_loc())
    }

    fn new_regexlit(&self, string: impl Into<String>, op: String) -> Token {
        Annot::new(
            TokenKind::Regex {
                body: string.into(),
                postfix: op,
            },
            self.cur_loc(),
        )
    }

    fn new_commandlit(&self, string: RubyString) -> Result<Token, LexerErr> {
        Ok(Annot::new(
            TokenKind::CommandLit(string.into_string()?),
            self.cur_loc(),
        ))
    }

    fn new_punct(&self, punc: Punct) -> Token {
        Annot::new(TokenKind::Punct(punc), self.cur_loc())
    }

    fn new_open_string(&self, s: String, delimiter: Option<char>, level: usize) -> Token {
        Token::new_open_string(s, delimiter, level, self.cur_loc())
    }

    fn new_open_reg(&self, s: String) -> Token {
        Token::new_open_reg(s, self.cur_loc())
    }

    fn new_open_command(&self, s: String, delimiter: Option<char>, level: usize) -> Token {
        Token::new_open_command(s, delimiter, level, self.cur_loc())
    }

    fn new_percent(&self, kind: char, content: String) -> Token {
        Token::new_percent(kind, content, self.cur_loc())
    }

    fn new_line_term(&self) -> Token {
        Annot::new(TokenKind::LineTerm, self.cur_loc())
    }

    fn new_eof(&self) -> Token {
        Annot::new(TokenKind::Eof, Loc(self.pos, self.pos))
    }
}

#[cfg(test)]
impl<'a> Lexer<'a> {
    pub(crate) fn tokenize(&mut self) -> Result<LexerResult, LexerErr> {
        let mut tokens = vec![];
        loop {
            match self.get_token() {
                Ok(res) => {
                    if res.is_eof() {
                        tokens.push(res);
                        break;
                    } else {
                        tokens.push(res);
                    }
                }
                Err(err) => return Err(err),
            }
        }
        return Ok(LexerResult::new(tokens));
    }
}

#[cfg(test)]
impl LexerResult {
    fn new(tokens: Vec<Token>) -> Self {
        LexerResult { tokens }
    }
}

#[cfg(test)]
#[allow(unused_imports, dead_code)]
mod test {
    use super::*;
    fn assert_tokens(program: &str, ans: Vec<Token>) {
        let mut lexer = Lexer::new(program);
        match lexer.tokenize() {
            Err(err) => panic!("{:?}", err),
            Ok(LexerResult { tokens, .. }) => {
                let len = tokens.len();
                if len != ans.len() {
                    print_tokens(&tokens, &ans);
                }
                for i in 0..len {
                    if tokens[i] != ans[i] {
                        print_tokens(&tokens, &ans);
                    }
                }
            }
        };
    }

    fn print_tokens(tokens: &[Token], ans: &[Token]) {
        println!("Expected:");
        for t in ans {
            println!("{}", t);
        }
        println!("Got:");
        for t in tokens {
            println!("{}", t);
        }
        panic!();
    }

    macro_rules! Token {
        (Ident($item:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_ident($item, Loc($loc_0, $loc_1))
        };
        (InstanceVar($item:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_instance_var($item, Loc($loc_0, $loc_1))
        };
        (GlobalVar($item:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_global_var($item, Loc($loc_0, $loc_1))
        };
        (Space, $loc_0:expr, $loc_1:expr) => {
            Token::new_space(Loc($loc_0, $loc_1))
        };
        (Punct($item:path), $loc_0:expr, $loc_1:expr) => {
            Token::new_punct($item, Loc($loc_0, $loc_1))
        };
        (Reserved($item:path), $loc_0:expr, $loc_1:expr) => {
            Token::new_reserved($item, Loc($loc_0, $loc_1))
        };
        (NumLit($num:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_intlit($num, Loc($loc_0, $loc_1))
        };
        (StringLit($item:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_stringlit($item.to_string().into(), Loc($loc_0, $loc_1))
        };
        (OpenString($item:expr, $delimiter:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_open_dq($item, $delimiter, Loc($loc_0, $loc_1))
        };
        (InterString($item:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_inter_dq($item, Loc($loc_0, $loc_1))
        };
        (CloseString($item:expr), $loc_0:expr, $loc_1:expr) => {
            Token::new_close_dq($item, Loc($loc_0, $loc_1))
        };
        (LineTerm, $loc_0:expr, $loc_1:expr) => {
            Token::new_line_term(Loc($loc_0, $loc_1))
        };
        (EOF, $pos:expr) => {
            Token::new_eof($pos)
        };
    }

    #[test]
    fn string_literal1() {
        let program = r#""""#;
        let ans = vec![Token![StringLit(""), 0, 1], Token![EOF, 2]];
        assert_tokens(program, ans);
    }

    #[test]
    fn string_literal2() {
        let program = r#""flower""#;
        let ans = vec![Token![StringLit("flower"), 0, 7], Token![EOF, 8]];
        assert_tokens(program, ans);
    }

    #[test]
    fn identifier1() {
        let program = "amber";
        let ans = vec![Token![Ident("amber"), 0, 4], Token![EOF, 5]];
        assert_tokens(program, ans);
    }

    #[test]
    fn instance_var() {
        let program = "@amber";
        let ans = vec![Token![InstanceVar("@amber"), 0, 5], Token![EOF, 6]];
        assert_tokens(program, ans);
    }

    #[test]
    fn global_var() {
        let program = "$amber";
        let ans = vec![Token![GlobalVar("$amber"), 0, 5], Token![EOF, 6]];
        assert_tokens(program, ans);
    }

    #[test]
    fn cmp1() {
        let program = "5 > 0";
        let ans = vec![
            Token![NumLit(5), 0, 0],
            Token![Punct(Punct::Gt), 2, 2],
            Token![NumLit(0), 4, 4],
            Token![EOF, 5],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn cmp2() {
        let program = "5 >= 0";
        let ans = vec![
            Token![NumLit(5), 0, 0],
            Token![Punct(Punct::Ge), 2, 3],
            Token![NumLit(0), 5, 5],
            Token![EOF, 6],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn cmp3() {
        let program = "5 == 0";
        let ans = vec![
            Token![NumLit(5), 0, 0],
            Token![Punct(Punct::Eq), 2, 3],
            Token![NumLit(0), 5, 5],
            Token![EOF, 6],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn cmp4() {
        let program = "5 != 0";
        let ans = vec![
            Token![NumLit(5), 0, 0],
            Token![Punct(Punct::Ne), 2, 3],
            Token![NumLit(0), 5, 5],
            Token![EOF, 6],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn cmp5() {
        let program = "5 < 0";
        let ans = vec![
            Token![NumLit(5), 0, 0],
            Token![Punct(Punct::Lt), 2, 2],
            Token![NumLit(0), 4, 4],
            Token![EOF, 5],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn cmp6() {
        let program = "5 <= 0";
        let ans = vec![
            Token![NumLit(5), 0, 0],
            Token![Punct(Punct::Le), 2, 3],
            Token![NumLit(0), 5, 5],
            Token![EOF, 6],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn lexer_test1() {
        let program = "a = 1\n if a==5 then 5 else 8";
        let ans = vec![
            Token![Ident("a"), 0, 0],
            Token![Punct(Punct::Assign), 2, 2],
            Token![NumLit(1), 4, 4],
            Token![LineTerm, 5, 5],
            Token![Reserved(Reserved::If), 7, 8],
            Token![Ident("a"), 10, 10],
            Token![Punct(Punct::Eq), 11, 12],
            Token![NumLit(5), 13, 13],
            Token![Reserved(Reserved::Then), 15, 18],
            Token![NumLit(5), 20, 20],
            Token![Reserved(Reserved::Else), 22, 25],
            Token![NumLit(8), 27, 27],
            Token![EOF, 28],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn lexer_test2() {
        let program = r"
        a = 0;
        if a == 1_000 then
            5 # this is a comment
        else
            10 # also a comment";
        let ans = vec![
            Token![LineTerm, 0, 0],
            Token![Ident("a"), 9, 9],
            Token![Punct(Punct::Assign), 11, 11],
            Token![NumLit(0), 13, 13],
            Token![Punct(Punct::Semi), 14, 14],
            Token![LineTerm, 15, 15],
            Token![Reserved(Reserved::If), 24, 25],
            Token![Ident("a"), 27, 27],
            Token![Punct(Punct::Eq), 29, 30],
            Token![NumLit(1000), 32, 36],
            Token![Reserved(Reserved::Then), 38, 41],
            Token![LineTerm, 42, 42],
            Token![NumLit(5), 55, 55],
            Token![LineTerm, 76, 76],
            Token![Reserved(Reserved::Else), 85, 88],
            Token![LineTerm, 89, 89],
            Token![NumLit(10), 102, 103],
            Token![EOF, 121],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn cont_line() {
        let program = r###"a \
\
\
  =\
77"###;
        let ans = vec![
            Token![Ident("a"), 0, 0],
            Token![Punct(Punct::Assign), 10, 10],
            Token![NumLit(77), 13, 14],
            Token![EOF, 15],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn octal() {
        let program = "/173";
        let ans = vec![
            Token![Punct(Punct::Div), 0, 0],
            Token![NumLit(173), 1, 3],
            Token![EOF, 4],
        ];
        assert_tokens(program, ans);
    }

    #[test]
    fn octal2() {
        let program = "0o173";
        let ans = vec![Token![NumLit(0o173), 0, 4], Token![EOF, 5]];
        assert_tokens(program, ans);
    }

    #[test]
    fn hex() {
        let program = "0xf";
        let ans = vec![Token![NumLit(0xf), 0, 2], Token![EOF, 3]];
        assert_tokens(program, ans);
    }

    #[test]
    fn hex2() {
        let program = "0xfe";
        let ans = vec![Token![NumLit(0xfe), 0, 3], Token![EOF, 4]];
        assert_tokens(program, ans);
    }

    #[test]
    fn method_name() {
        fn assert(program: &str, expect: &str) {
            let mut lexer = Lexer::new(program);
            assert_eq!(lexer.read_method_name(true).unwrap().0, expect);
        }
        assert("Func", "Func");
        assert("Func!", "Func!");
        assert("Func!=", "Func");
        assert("Func?", "Func?");
        assert("func", "func");
        assert("func=[1,2,3]", "func=");
        assert("compare_by_identity\n", "compare_by_identity");
        assert("func!", "func!");
        assert("func!=", "func");
        assert("func?", "func?");
        assert("==4", "==");
        assert("<=>>", "<=>");
        assert("===-", "===");
        assert(">==", ">=");
        assert("[]*", "[]");
        assert("[]=", "[]=");
        assert("<<<", "<<");
        assert("==~", "==");
        assert("=~-", "=~");
    }
}
