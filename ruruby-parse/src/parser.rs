use super::*;
use num::BigInt;
use std::path::PathBuf;

mod arguments;
mod define;
mod expression;
mod flow_control;
mod lexer;
mod literals;
mod primary;
pub(crate) use lexer::*;

/// $&
pub const SPECIAL_LASTMATCH: u32 = 0;
/// $'
pub const SPECIAL_POSTMATCH: u32 = 1;
/// $LOAD_PATH
pub const SPECIAL_LOADPATH: u32 = 10;
/// $LOADED_FEATURES
pub const SPECIAL_LOADEDFEATURES: u32 = 11;

pub trait LocalsContext: Sized {
    fn find_lvar(&self, id: &str) -> Option<usize>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct DummyContext;

impl LocalsContext for DummyContext {
    fn find_lvar(&self, _id: &str) -> Option<usize> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'a, OuterContext: LocalsContext> {
    lexer: Lexer<'a>,
    path: PathBuf,
    prev_loc: Loc,
    /// scope stack.
    scope: Vec<LvarScope>,
    /// loop stack.
    loop_stack: Vec<LoopKind>,
    extern_context: Option<&'a OuterContext>,
    /// this flag suppress accesory assignment. e.g. x=3
    suppress_acc_assign: bool,
    /// this flag suppress accesory multiple assignment. e.g. x = 2,3
    suppress_mul_assign: bool,
    /// this flag suppress parse do-end style block.
    suppress_do_block: bool,
    /// defined? mode: allow invalid break/next.
    defined_mode: bool,
}

impl<'a> Parser<'a, DummyContext> {
    pub fn parse_program(code: String, path: impl Into<PathBuf>) -> Result<ParseResult, ParseErr> {
        let path = path.into();
        let parse_ctx = LvarScope::new_eval(None);
        parse(code, path, None::<&DummyContext>, parse_ctx)
    }
}

impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    pub fn parse_program_eval(
        code: String,
        path: PathBuf,
        extern_context: Option<&'a OuterContext>,
    ) -> Result<ParseResult, ParseErr> {
        let parse_ctx = LvarScope::new_eval(None);
        parse(code, path, extern_context, parse_ctx)
    }

    pub fn parse_program_binding(
        code: String,
        path: PathBuf,
        context: Option<LvarCollector>,
        extern_context: Option<&OuterContext>,
    ) -> Result<ParseResult, ParseErr> {
        let parse_ctx = LvarScope::new_block(context);
        parse(code, path, extern_context, parse_ctx)
    }

    fn parse(
        code: &'a str,
        path: PathBuf,
        extern_context: Option<&'a OuterContext>,
        scope: LvarScope,
    ) -> Result<(Node, LvarCollector, Token), LexerErr> {
        let lexer = Lexer::new(code);
        let mut parser = Parser {
            lexer,
            path,
            prev_loc: Loc(0, 0),
            scope: vec![scope],
            loop_stack: vec![LoopKind::Top],
            extern_context,
            suppress_acc_assign: false,
            suppress_mul_assign: false,
            suppress_do_block: false,
            defined_mode: false,
        };
        let node = parser.parse_comp_stmt()?;
        let lvar = parser.scope.pop().unwrap().lvar;
        let tok = parser.peek()?;
        Ok((node, lvar, tok))
    }

    fn save_state(&self) -> (usize, usize) {
        self.lexer.save_state()
    }

    fn restore_state(&mut self, state: (usize, usize)) {
        self.lexer.restore_state(state);
    }

    fn scope_mut(&mut self) -> &mut LvarScope {
        self.scope.last_mut().unwrap()
    }

    fn is_method_context(&self) -> bool {
        for scope in self.scope.iter().rev() {
            match scope.kind {
                ScopeKind::Method => return true,
                ScopeKind::Class | ScopeKind::Eval => return false,
                ScopeKind::Block | ScopeKind::For => {}
            }
        }
        false
    }

    /// Check whether parameter delegation exists or not in the method def of current context.
    /// If not, return ParseErr.
    fn check_forwarding(&self) -> Result<(), LexerErr> {
        for ctx in self.scope.iter().rev() {
            if ctx.kind == ScopeKind::Method {
                if ctx.lvar.forwarding_param.is_some() {
                    return Ok(());
                } else {
                    break;
                }
            }
        }
        Err(error_unexpected(self.prev_loc(), "Unexpected ..."))
    }

    /// If the `id` does not exist in the scope chain,
    /// add `id` as a local variable in the current context.
    fn add_local_var_if_new(&mut self, name: &str) -> usize {
        if let Some(outer) = self.is_local_var(name) {
            return outer;
        } else {
            for c in self.scope.iter_mut().rev() {
                match c.kind {
                    ScopeKind::For => {}
                    _ => {
                        c.lvar.insert(name);
                        return 0;
                    }
                };
            }
        }
        unreachable!()
    }

    /// Add the `id` as a new parameter in the current context.
    /// If a parameter with the same name already exists, return error.
    fn new_param(&mut self, name: String, loc: Loc) -> Result<LvarId, LexerErr> {
        match self.scope_mut().lvar.insert_new(name) {
            Some(lvar) => Ok(lvar),
            None => Err(error_unexpected(loc, "Duplicated argument name.")),
        }
    }

    fn add_kw_param(&mut self, lvar: LvarId) {
        self.scope_mut().lvar.kw.push(lvar);
    }

    /// Add the `id` as a new parameter in the current context.
    /// If a parameter with the same name already exists, return error.
    fn new_kwrest_param(&mut self, name: String, loc: Loc) -> Result<(), LexerErr> {
        if self.scope_mut().lvar.insert_kwrest_param(name).is_none() {
            return Err(error_unexpected(loc, "Duplicated argument name."));
        }
        Ok(())
    }

    /// Add the `id` as a new block parameter in the current context.
    /// If a parameter with the same name already exists, return error.
    fn new_block_param(&mut self, name: String, loc: Loc) -> Result<(), LexerErr> {
        if self.scope_mut().lvar.insert_block_param(name).is_none() {
            return Err(error_unexpected(loc, "Duplicated argument name."));
        }
        Ok(())
    }

    /// Add the `id` as a new block parameter in the current context.
    /// If a parameter with the same name already exists, return error.
    fn new_forwarding_param(&mut self, loc: Loc) -> Result<(), LexerErr> {
        if self.scope_mut().lvar.insert_delegate_param().is_none() {
            return Err(error_unexpected(loc, "Duplicated argument name."));
        }
        Ok(())
    }

    /// Examine whether `id` exists in the scope chain.
    /// If exiets, return true.
    fn is_local_var(&mut self, id: &str) -> Option<usize> {
        let mut outer = 0;
        for c in self.scope.iter().rev() {
            if c.lvar.table.get_lvarid(id).is_some() {
                return Some(outer);
            }
            match c.kind {
                ScopeKind::Block => outer += 1,
                ScopeKind::For => {}
                _ => break,
            }
        }
        if let Some(a) = &self.extern_context {
            return a.find_lvar(id);
        }
        None
    }

    /// Check whether numbered parameters exist in outer blocks.
    fn check_outer_numbered_param(&mut self, loc: Loc) -> Result<(), LexerErr> {
        //let mut outer = 0;
        if let Some(loc) = self.scope.last_mut().unwrap().lvar.prohibit_numbered_param {
            return Err(error_nested_numbered_param(loc));
        }
        for c in self.scope.iter_mut().rev().skip(1) {
            if let Some(loc) = c.lvar.numbered_param {
                return Err(error_nested_numbered_param(loc));
            }
            let prohibit = &mut c.lvar.prohibit_numbered_param;
            if prohibit.is_none() {
                *prohibit = Some(loc);
            }
            match c.kind {
                ScopeKind::Block => {} //outer += 1,
                ScopeKind::For => {}
                _ => break,
            }
        }
        self.scope.last_mut().unwrap().lvar.numbered_param = Some(loc);
        /*if let Some(a) = &self.extern_context {
            return a.find_lvar(id);
        }*/
        Ok(())
    }

    /// Peek next token (skipping line terminators).
    fn peek(&mut self) -> Result<Token, LexerErr> {
        self.lexer.peek_token_skip_lt()
    }

    /// Peek next token (no skipping line terminators).
    fn peek_no_term(&mut self) -> Result<Token, LexerErr> {
        self.lexer.peek_token()
    }

    /// Peek next token (no skipping line terminators), and check whether the token is `punct` or not.
    fn peek_punct_no_term(&mut self, punct: Punct) -> bool {
        match self.lexer.peek_token() {
            Ok(tok) => tok.kind == TokenKind::Punct(punct),
            Err(_) => false,
        }
    }

    /// Examine the next token, and return true if it is a line terminator.
    fn is_line_term(&mut self) -> Result<bool, LexerErr> {
        Ok(self.peek_no_term()?.is_line_term())
    }

    fn loc(&mut self) -> Loc {
        self.peek_no_term().unwrap().loc()
    }

    fn prev_loc(&self) -> Loc {
        self.prev_loc
    }

    /// Get next token (skipping line terminators).
    /// Return RubyError if it was EOF.
    fn get(&mut self) -> Result<Token, LexerErr> {
        loop {
            let tok = self.lexer.get_token(false)?;
            if tok.is_eof() {
                return Err(error_eof(tok.loc()));
            }
            if !tok.is_line_term() {
                self.prev_loc = tok.loc;
                return Ok(tok);
            }
        }
    }

    /// Get next token (skipping line terminators).
    /// Do not allow assignment operators.
    /// Return RubyError if it was EOF.
    fn get_primary(&mut self) -> Result<Token, LexerErr> {
        loop {
            let tok = self.lexer.get_token(true)?;
            if tok.is_eof() {
                return Err(error_eof(tok.loc()));
            }
            if !tok.is_line_term() {
                self.prev_loc = tok.loc;
                return Ok(tok);
            }
        }
    }

    /// Get next token (no skipping line terminators).
    fn get_no_skip_line_term(&mut self) -> Result<Token, LexerErr> {
        let tok = self.lexer.get_token(false)?;
        self.prev_loc = tok.loc;
        Ok(tok)
    }

    fn consume_char(&mut self, ch: char) -> bool {
        self.lexer.consume(ch)
    }

    /// If the next token is Ident, consume and return Some(it).
    /// If not, return None.
    fn consume_ident(&mut self) -> Result<Option<String>, LexerErr> {
        match self.peek()?.kind {
            TokenKind::Ident(s) => {
                self.get()?;
                Ok(Some(s))
            }
            TokenKind::NumberedParam(i, _) => Err(error_numbered_param(self.prev_loc(), i)),
            _ => Ok(None),
        }
    }

    /// If the next token is an expected kind of Punctuator, get it and return true.
    /// Otherwise, return false.
    fn consume_punct(&mut self, expect: Punct) -> Result<bool, LexerErr> {
        match self.peek()?.kind {
            TokenKind::Punct(punct) if punct == expect => {
                self.get()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn consume_punct_no_term(&mut self, expect: Punct) -> Result<bool, LexerErr> {
        if TokenKind::Punct(expect) == self.peek_no_term()?.kind {
            self.get()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume_assign_op_no_term(&mut self) -> Result<Option<BinOp>, LexerErr> {
        if let TokenKind::Punct(Punct::AssignOp(op)) = self.peek_no_term()?.kind {
            Ok(Some(op))
        } else {
            Ok(None)
        }
    }

    /// If next token is an expected kind of Reserved keyeord, get it and return true.
    /// Otherwise, return false.
    fn consume_reserved(&mut self, expect: Reserved) -> Result<bool, LexerErr> {
        match self.peek()?.kind {
            TokenKind::Reserved(reserved) if reserved == expect => {
                self.get()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn consume_reserved_no_skip_line_term(&mut self, expect: Reserved) -> Result<bool, LexerErr> {
        if TokenKind::Reserved(expect) == self.peek_no_term()?.kind {
            self.get()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Get the next token if it is a line terminator or ';' or EOF, and return true,
    /// Otherwise, return false.
    fn consume_term(&mut self) -> Result<bool, LexerErr> {
        if !self.peek_no_term()?.is_term() {
            return Ok(false);
        };
        while self.peek_no_term()?.is_term() {
            if self.get_no_skip_line_term()?.is_eof() {
                return Ok(true);
            }
        }
        Ok(true)
    }

    /// Get the next token and examine whether it is an expected Reserved.
    /// If not, return RubyError.
    fn expect_reserved(&mut self, expect: Reserved) -> Result<(), LexerErr> {
        match &self.get()?.kind {
            TokenKind::Reserved(reserved) if *reserved == expect => Ok(()),
            t => Err(error_unexpected(
                self.prev_loc(),
                format!("Expect {:?} Got {:?}", expect, t),
            )),
        }
    }

    /// Get the next token and examine whether it is an expected Punct.
    /// If not, return RubyError.
    fn expect_punct(&mut self, expect: Punct) -> Result<(), LexerErr> {
        match &self.get()?.kind {
            TokenKind::Punct(punct) if *punct == expect => Ok(()),
            t => Err(error_unexpected(
                self.prev_loc(),
                format!("Expect {:?} Got {:?}", expect, t),
            )),
        }
    }

    /// Get the next token and examine whether it is Ident.
    /// Return IdentId of the Ident.
    /// If not, return RubyError.
    fn expect_ident(&mut self) -> Result<String, LexerErr> {
        match self.get()?.kind {
            TokenKind::Ident(name) => Ok(name),
            TokenKind::NumberedParam(i, _) => Err(error_numbered_param(self.prev_loc(), i)),
            _ => Err(error_unexpected(self.prev_loc(), "Expect identifier.")),
        }
    }

    /// Get the next token and examine whether it is Const.
    /// Return IdentId of the Const.
    /// If not, return RubyError.
    fn expect_const(&mut self) -> Result<String, LexerErr> {
        match self.get()?.kind {
            TokenKind::Const(s) => Ok(s),
            _ => Err(error_unexpected(self.prev_loc(), "Expect constant.")),
        }
    }

    fn read_method_name(&mut self, allow_assign_like: bool) -> Result<(String, Loc), LexerErr> {
        self.lexer.read_method_name(allow_assign_like)
    }

    fn read_method_ext(&mut self, s: String) -> Result<String, LexerErr> {
        self.lexer.read_method_ext(s)
    }
}

impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    /// Parse operator which can be defined as a method.
    /// Return IdentId of the operator.
    fn parse_op_definable(&mut self, punct: &Punct) -> Result<&'static str, LexerErr> {
        // TODO: must support
        // **  ~  ` !  !~
        match punct {
            Punct::Plus => {
                if self.consume_char('@') {
                    Ok("+@")
                } else {
                    Ok("+")
                }
            }
            Punct::Minus => {
                if self.consume_char('@') {
                    Ok("-@")
                } else {
                    Ok("-")
                }
            }
            Punct::Mul => Ok("*"),
            Punct::Div => Ok("/"),
            Punct::Rem => Ok("%"),
            Punct::Shl => Ok("<<"),
            Punct::Shr => Ok(">>"),
            Punct::BitAnd => Ok("&"),
            Punct::BitOr => Ok("|"),

            Punct::Cmp => Ok("<=>"),
            Punct::Eq => Ok("=="),
            Punct::Ne => Ok("!="),
            Punct::Lt => Ok("<"),
            Punct::Le => Ok("<="),
            Punct::Gt => Ok(">"),
            Punct::Ge => Ok(">="),
            Punct::TEq => Ok("==="),
            Punct::Match => Ok("=~"),
            Punct::BitXor => Ok("^"),
            Punct::LBracket => {
                if self.consume_punct_no_term(Punct::RBracket)? {
                    if self.consume_punct_no_term(Punct::Assign)? {
                        Ok("[]=")
                    } else {
                        Ok("[]")
                    }
                } else {
                    let loc = self.loc();
                    Err(error_unexpected(loc, "Invalid operator."))
                }
            }
            _ => Err(error_unexpected(self.prev_loc(), "Invalid operator.")),
        }
    }

    fn parse_then(&mut self) -> Result<(), LexerErr> {
        if self.consume_term()? {
            self.consume_reserved(Reserved::Then)?;
            return Ok(());
        }
        self.expect_reserved(Reserved::Then)
    }

    fn parse_do(&mut self) -> Result<(), LexerErr> {
        if self.consume_term()? {
            return Ok(());
        }
        self.expect_reserved(Reserved::Do)
    }

    /// Parse formal parameters.
    /// required, optional = defaule, *rest, post_required, kw: default, **rest_kw, &block
    fn parse_formal_params(
        &mut self,
        terminator: impl Into<Option<Punct>>,
    ) -> Result<Vec<FormalParam>, LexerErr> {
        #[derive(Debug, Clone, PartialEq, PartialOrd)]
        enum Kind {
            Required,
            Optional,
            Rest,
            PostReq,
            KeyWord,
            KWRest,
        }

        let terminator = terminator.into();
        let mut args = vec![];
        let mut state = Kind::Required;

        loop {
            if let Some(term) = terminator {
                if self.consume_punct(term)? {
                    return Ok(args);
                }
            }
            let mut loc = self.loc();
            if self.consume_punct(Punct::LParen)? {
                args.push(self.parse_destruct_param()?);
            } else if self.consume_punct(Punct::Range3)? {
                // Argument delegation
                if state > Kind::Required {
                    return Err(error_unexpected(
                        loc,
                        "parameter forwarding is not allowed in ths position.",
                    ));
                }
                args.push(FormalParam::forwarding(loc));
                self.new_forwarding_param(loc)?;
                break;
            } else if self.consume_punct(Punct::BitAnd)? {
                // Block param
                let name = self.expect_ident()?;
                loc = loc.merge(self.prev_loc());
                args.push(FormalParam::block(name.clone(), loc));
                self.new_block_param(name, loc)?;
                break;
            } else if self.consume_punct(Punct::Mul)? {
                // Splat(Rest) param
                loc = loc.merge(self.prev_loc());
                if state >= Kind::Rest {
                    return Err(error_unexpected(
                        loc,
                        "Rest parameter is not allowed in ths position.",
                    ));
                } else {
                    state = Kind::Rest;
                };
                match self.consume_ident()? {
                    Some(name) => {
                        args.push(FormalParam::rest(name.clone(), loc));
                        self.new_param(name, self.prev_loc())?;
                    }
                    None => args.push(FormalParam::rest_discard(loc)),
                }
            } else if self.consume_punct(Punct::DMul)? {
                // Keyword rest param
                let name = self.expect_ident()?;
                loc = loc.merge(self.prev_loc());
                if state >= Kind::KWRest {
                    return Err(error_unexpected(
                        loc,
                        "Keyword rest parameter is not allowed in ths position.",
                    ));
                } else {
                    state = Kind::KWRest;
                }

                args.push(FormalParam::kwrest(name.clone(), loc));
                self.new_kwrest_param(name, self.prev_loc())?;
            } else {
                let name = self.expect_ident()?;
                if self.consume_punct(Punct::Assign)? {
                    // Optional param
                    let default = if let Some(Punct::BitOr) = terminator {
                        let node = self.parse_primary()?;
                        node
                    } else {
                        self.parse_arg(false)?
                    };
                    loc = loc.merge(self.prev_loc());
                    match state {
                        Kind::Required => state = Kind::Optional,
                        Kind::Optional => {}
                        _ => {
                            return Err(error_unexpected(
                                loc,
                                "Optional parameter is not allowed in ths position.",
                            ));
                        }
                    };
                    args.push(FormalParam::optional(name.clone(), default, loc));
                    self.new_param(name, loc)?;
                } else if self.consume_punct_no_term(Punct::Colon)? {
                    // Keyword param
                    let next = self.peek_no_term()?.kind;
                    let default =
                        if next == TokenKind::Punct(Punct::Comma) || next == TokenKind::LineTerm {
                            None
                        } else if let Some(term) = terminator {
                            if next == TokenKind::Punct(term) {
                                None
                            } else {
                                Some(self.parse_arg(false)?)
                            }
                        } else {
                            Some(self.parse_arg(false)?)
                        };
                    loc = loc.merge(self.prev_loc());
                    if state == Kind::KWRest {
                        return Err(error_unexpected(
                            loc,
                            "Keyword parameter is not allowed in ths position.",
                        ));
                    } else {
                        state = Kind::KeyWord;
                    };
                    args.push(FormalParam::keyword(name.clone(), default, loc));
                    let lvar = self.new_param(name, loc)?;
                    self.add_kw_param(lvar);
                } else {
                    // Required param
                    loc = self.prev_loc();
                    match state {
                        Kind::Required => {
                            args.push(FormalParam::req_param(name.clone(), loc));
                            self.new_param(name, loc)?;
                        }
                        Kind::PostReq | Kind::Optional | Kind::Rest => {
                            args.push(FormalParam::post(name.clone(), loc));
                            self.new_param(name, loc)?;
                            state = Kind::PostReq;
                        }
                        _ => {
                            return Err(error_unexpected(
                                loc,
                                "Required parameter is not allowed in ths position.",
                            ));
                        }
                    }
                };
            }
            match terminator {
                Some(_) => {
                    if !self.consume_punct(Punct::Comma)? {
                        break;
                    }
                }
                None => {
                    if !self.consume_punct_no_term(Punct::Comma)? {
                        break;
                    }
                }
            }
        }
        if let Some(term) = terminator {
            self.expect_punct(term)?;
        }
        Ok(args)
    }

    fn parse_destruct_param(&mut self) -> Result<FormalParam, LexerErr> {
        let loc = self.loc();
        let name = self.expect_ident()?;
        self.new_param(name.clone(), loc)?;
        let mut idents = vec![(name, loc)];
        while self.consume_punct(Punct::Comma)? {
            if self.consume_punct(Punct::RParen)? {
                return Ok(FormalParam::destruct_param(idents));
            }
            let loc = self.loc();
            let name = self.expect_ident()?;
            self.new_param(name.clone(), loc)?;
            idents.push((name, loc));
        }
        self.expect_punct(Punct::RParen)?;
        Ok(FormalParam::destruct_param(idents))
    }
}

fn error_unexpected(loc: Loc, msg: impl Into<String>) -> LexerErr {
    LexerErr(ParseErrKind::SyntaxError(msg.into()), loc)
}

fn error_numbered_param(loc: Loc, i: u8) -> LexerErr {
    LexerErr(
        ParseErrKind::SyntaxError(format!("_{i} is reserved for numbered parameter")),
        loc,
    )
}

fn error_nested_numbered_param(loc: Loc) -> LexerErr {
    LexerErr(
        ParseErrKind::SyntaxError("numbered parameter is already used".to_string()),
        loc,
    )
}

fn error_eof(loc: Loc) -> LexerErr {
    LexerErr(ParseErrKind::UnexpectedEOF, loc)
}

fn parse(
    code: String,
    path: PathBuf,
    extern_context: Option<&impl LocalsContext>,
    parse_context: LvarScope,
) -> Result<ParseResult, ParseErr> {
    match Parser::parse(&code, path.clone(), extern_context, parse_context) {
        Ok((node, lvar_collector, tok)) => {
            let source_info = SourceInfoRef::new(SourceInfo::new(path, code));
            if tok.is_eof() {
                let result = ParseResult {
                    node,
                    lvar_collector,
                    source_info,
                };
                Ok(result)
            } else {
                let err = error_unexpected(tok.loc(), "Expected end-of-input.");
                Err(ParseErr::from_lexer_err(err, source_info))
            }
        }
        Err(err) => {
            let source_info = SourceInfoRef::new(SourceInfo::new(path, code));
            Err(ParseErr::from_lexer_err(err, source_info))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseResult {
    pub node: Node,
    pub lvar_collector: LvarCollector,
    pub source_info: SourceInfoRef,
    //pub id_store: IdentifierTable,
}

#[derive(Debug, Clone, PartialEq)]
enum LoopKind {
    Top,
    Block,
    While,
    For,
}

#[derive(Debug, Clone, PartialEq)]
enum ScopeKind {
    Eval,
    Class,
    Method,
    Block,
    For,
}

#[derive(Debug, Clone, PartialEq)]
struct LvarScope {
    kind: ScopeKind,
    lvar: LvarCollector,
}

impl LvarScope {
    fn new_method() -> Self {
        LvarScope {
            lvar: LvarCollector::new(),
            kind: ScopeKind::Method,
        }
    }

    fn new_eval(lvar_collector: Option<LvarCollector>) -> Self {
        LvarScope {
            lvar: lvar_collector.unwrap_or_default(),
            kind: ScopeKind::Eval,
        }
    }

    fn new_class(lvar_collector: Option<LvarCollector>) -> Self {
        LvarScope {
            lvar: lvar_collector.unwrap_or_default(),
            kind: ScopeKind::Class,
        }
    }

    fn new_block(lvar_collector: Option<LvarCollector>) -> Self {
        LvarScope {
            lvar: lvar_collector.unwrap_or_default(),
            kind: ScopeKind::Block,
        }
    }

    fn new_for() -> Self {
        LvarScope {
            lvar: LvarCollector::new(),
            kind: ScopeKind::For,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RescueEntry {
    /// The exception classes for this rescue clause.
    pub exception_list: Vec<Node>,
    /// Assignment destination for error value in rescue clause.
    pub assign: Option<Box<Node>>,
    /// The body of this rescue clause.
    pub body: Box<Node>,
}

impl RescueEntry {
    fn new(exception_list: Vec<Node>, assign: Option<Node>, body: Node) -> Self {
        Self {
            exception_list,
            assign: assign.map(Box::new),
            body: Box::new(body),
        }
    }

    fn new_postfix(body: Node) -> Self {
        Self {
            exception_list: vec![],
            assign: None,
            body: Box::new(body),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NReal {
    Integer(i64),
    Bignum(BigInt),
    Float(f64),
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_test(code: &str) {
        let node = Parser::<DummyContext>::parse(
            code,
            std::path::PathBuf::new(),
            None,
            LvarScope::new_eval(None),
        )
        .unwrap()
        .0;
        dbg!(node);
    }

    fn parse_node(code: &str, expected: Node) {
        let node = Parser::<DummyContext>::parse(
            code,
            std::path::PathBuf::new(),
            None,
            LvarScope::new_eval(None),
        )
        .unwrap()
        .0;
        assert_eq!(node, expected);
    }

    fn parse_test_err(code: &str) {
        Parser::<DummyContext>::parse(
            code,
            std::path::PathBuf::new(),
            None,
            LvarScope::new_eval(None),
        )
        .unwrap_err();
    }

    #[test]
    fn test() {
        parse_test("1.times {|x,y|}");
        parse_test("1.times {|x,|}");
        parse_test("1.times {|x,(y,z)|}");
        parse_test("1.times {|x,(y,z,),|}");
        parse_test_err("1.times {|x,(y,z|}");
        parse_test_err("1.times {|x,(y,z)}");

        parse_test(
            r#"1.times {|x
            ,(y,z,),|}"#,
        );
        parse_test(
            r#"1.times {|x
            ,(y
                ,z,),|}"#,
        );
        parse_test(
            r#"
            def f y,z
            end"#,
        );
        parse_test(
            r#"
            def f y,
            z
            end"#,
        );
        parse_test_err(
            r#"
            def f y
            ,z
            end"#,
        );
        parse_test(
            r#"
        for i in 0..5
            redo
        end
        "#,
        );
    }

    #[test]
    fn method_call() {
        parse_test("a()");
        parse_test("a(100)");
        parse_test("a(:a=>1, :b=>2)");
        parse_test("a(:a=>1, :b=>2, )");
    }

    #[test]
    fn hash() {
        parse_test("[:a => 5]");
        parse_test("[][:a => 5]");
        parse_test("p(:a => 5)");
        parse_test("p :a => 5");
    }

    #[test]
    fn special_vars() {
        parse_node(
            "$&",
            Annot {
                kind: NodeKind::SpecialVar(SPECIAL_LASTMATCH),
                loc: Loc(0, 1),
            },
        );
        parse_node(
            "$'",
            Annot {
                kind: NodeKind::SpecialVar(SPECIAL_POSTMATCH),
                loc: Loc(0, 1),
            },
        );
        parse_node(
            "$LOAD_PATH",
            Annot {
                kind: NodeKind::SpecialVar(SPECIAL_LOADPATH),
                loc: Loc(0, 9),
            },
        );
        parse_node(
            "$:",
            Annot {
                kind: NodeKind::SpecialVar(SPECIAL_LOADPATH),
                loc: Loc(0, 1),
            },
        );
        parse_node(
            "$LOADED_FEATURES",
            Annot {
                kind: NodeKind::SpecialVar(SPECIAL_LOADEDFEATURES),
                loc: Loc(0, 15),
            },
        );
        parse_node(
            "$\"",
            Annot {
                kind: NodeKind::SpecialVar(SPECIAL_LOADEDFEATURES),
                loc: Loc(0, 1),
            },
        );
    }

    #[test]
    fn test_err() {
        parse_test_err(
            r#"
        def f
          class C; end
        end
        "#,
        );
        parse_test_err(
            r#"
        def f
          1.times do
            class C; end
          end
        end
        "#,
        );
        //parse_test_err("break");
        //parse_test_err("next");
        //parse_test_err("redo");
        parse_test_err(
            r#"
        for i in 0..5
            redo 5
        end
        "#,
        );
        parse_test("defined? break");
        parse_test("defined? next");
        parse_test("defined? redo");
        /*parse_test_err(
            r#"
        def f
          break
        end
        "#,
        );
        parse_test_err(
            r#"
        def f
          next
        end
        "#,
        );*/
    }
}
