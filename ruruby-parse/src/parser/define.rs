//use super::get_string_from_reserved;
use super::*;

impl<'a, OuterContext: LocalsContext> Parser<'a, OuterContext> {
    /// Parse method definition.
    pub(super) fn parse_def(&mut self) -> Result<Node, LexerErr> {
        // メソッド定義

        // 特異メソッド定義
        // ( 変数参照 | "(" 式 ")" ) ( "." | "::" ) メソッド定義名
        // 変数参照 : 定数識別子 | 大域変数識別子 | クラス変数識別子 | インスタンス変数識別子 | 局所変数識別子 | 擬似変数
        // メソッド定義名 : メソッド名 ｜ ( 定数識別子 | 局所変数識別子 ) "="
        let def_loc = self.prev_loc();
        let tok = self.get()?;
        let loc = tok.loc;
        let (singleton, name) = match tok.kind {
            TokenKind::GlobalVar(name) => {
                self.consume_punct_no_term(Punct::Dot)?;
                (
                    Some(Node::new_global_var(name, loc)),
                    self.read_method_name(true)?.0,
                )
            }
            TokenKind::InstanceVar(name) => {
                self.consume_punct_no_term(Punct::Dot)?;
                (
                    Some(Node::new_instance_var(name, loc)),
                    self.read_method_name(true)?.0,
                )
            }
            TokenKind::Reserved(r) => (None, self.read_method_ext(r.as_str().to_string())?),
            TokenKind::Ident(s) => {
                if s.as_str() == "self" {
                    self.consume_punct_no_term(Punct::Dot)?;
                    (Some(Node::new_self(loc)), self.read_method_name(true)?.0)
                } else if self.consume_punct_no_term(Punct::Dot)?
                    || self.consume_punct_no_term(Punct::Scope)?
                {
                    (
                        Some(Node::new_lvar(s, 0, loc)),
                        self.read_method_name(true)?.0,
                    )
                } else {
                    (None, self.read_method_ext(s)?)
                }
            }
            TokenKind::NumberedParam(i, _) => return Err(error_numbered_param(loc, i)),
            TokenKind::Const(s) => {
                if self.consume_punct_no_term(Punct::Dot)?
                    || self.consume_punct_no_term(Punct::Scope)?
                {
                    (
                        Some(Node::new_const(s, false, None, vec![], loc)),
                        self.read_method_name(true)?.0,
                    )
                } else {
                    (None, self.read_method_ext(s)?)
                }
            }
            TokenKind::Punct(p) => (None, self.parse_op_definable(&p)?.to_string()),
            _ => return Err(error_unexpected(loc, "Invalid method name.")),
        };

        self.scope.push(LvarScope::new_method());
        self.loop_stack.push(LoopKind::Top);
        let args = self.parse_def_params()?;
        let body = self.parse_begin()?;
        self.loop_stack.pop().unwrap();
        let lvar = self.scope.pop().unwrap().lvar;

        let decl = match singleton {
            Some(singleton) => {
                Node::new_singleton_method_decl(singleton, name, args, body, lvar, def_loc)
            }
            None => Node::new_method_decl(name, args, body, lvar, def_loc),
        };
        Ok(decl)
    }

    /// Parse class definition.
    pub(super) fn parse_class(&mut self, is_module: bool) -> Result<Node, LexerErr> {
        // クラス定義 : "class" クラスパス [行終端子禁止] ("<" 式)? 分離子 本体文 "end"
        // クラスパス : "::" 定数識別子
        //      ｜ 定数識別子
        //      ｜ 一次式 [行終端子禁止] "::" 定数識別子
        //let loc = self.prev_loc();
        let prim = self.parse_class_def_name()?;
        let loc = prim.loc;
        let (base, name) = match prim.kind {
            NodeKind::Const {
                toplevel,
                name,
                parent,
                mut prefix,
            } => match prefix.pop() {
                Some(parent_name) => (
                    Some(Node::new_const(
                        parent_name,
                        toplevel,
                        parent,
                        prefix,
                        prim.loc,
                    )),
                    name,
                ),
                None => match parent {
                    Some(parent) => (Some(*parent), name),
                    None => (None, name),
                },
            },
            //NodeKind::Scope(base, id) => (*base, id),
            _ => return Err(error_unexpected(prim.loc, "Invalid Class/Module name.")),
        };
        //eprintln!("base:{:?} name:{:?}", base, name);

        let superclass = if self.consume_punct_no_term(Punct::Lt)? {
            if is_module {
                return Err(error_unexpected(self.prev_loc(), "Unexpected '<'."));
            };
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.consume_term()?;

        self.scope.push(LvarScope::new_class(None));
        self.loop_stack.push(LoopKind::Top);
        let body = self.parse_begin()?;
        self.loop_stack.pop().unwrap();
        let lvar = self.scope.pop().unwrap().lvar;

        Ok(Node::new_class_decl(
            base, name, superclass, body, lvar, is_module, loc,
        ))
    }

    /// Parse singleton class definition.
    pub(super) fn parse_singleton_class(&mut self, loc: Loc) -> Result<Node, LexerErr> {
        // class "<<" EXPR <term>
        //      COMPSTMT
        // end
        let singleton = self.parse_expr()?;
        let loc = loc.merge(self.prev_loc());
        self.consume_term()?;

        self.scope.push(LvarScope::new_class(None));
        self.loop_stack.push(LoopKind::Top);
        let body = self.parse_begin()?;
        self.loop_stack.pop().unwrap();
        let lvar = self.scope.pop().unwrap().lvar;

        Ok(Node::new_singleton_class_decl(singleton, body, lvar, loc))
    }

    pub(crate) fn alias_name(&mut self) -> Result<Node, LexerErr> {
        if self.consume_punct_no_term(Punct::Colon)? {
            self.parse_symbol()
        } else if let TokenKind::GlobalVar(_) = self.peek_no_term()?.kind {
            let tok = self.get()?;
            match &tok.kind {
                TokenKind::GlobalVar(name) => Ok(Node::new_symbol(name.to_owned(), tok.loc)),
                _ => unreachable!(),
            }
        } else {
            Ok(Node::new_symbol(
                self.read_method_name(true)?.0,
                self.prev_loc(),
            ))
        }
    }

    // ( )
    // ( ident [, ident]* )
    fn parse_def_params(&mut self) -> Result<Vec<FormalParam>, LexerErr> {
        if self.consume_term()? {
            return Ok(vec![]);
        };
        let term = if self.consume_punct(Punct::LParen)? {
            Some(Punct::RParen)
        } else {
            None
        };
        let args = self.parse_formal_params(term)?;
        self.consume_term()?;
        Ok(args)
    }

    fn parse_class_def_name(&mut self) -> Result<Node, LexerErr> {
        // クラスパス : "::" 定数識別子
        //      ｜ 定数識別子
        //      ｜ 一次式 [行終端子禁止] "::" 定数識別子
        let mut node = self.parse_primary()?;
        loop {
            node = if self.consume_punct(Punct::Dot)? {
                self.parse_primary_method(node, false)?
            } else if self.consume_punct_no_term(Punct::Scope)? {
                let loc = node.loc;
                let name = self.expect_const()?;

                if let NodeKind::Const {
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

                //Node::new_const(name, node,  loc)
            } else {
                return Ok(node);
            };
        }
    }
}
