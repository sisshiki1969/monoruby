use super::*;
use num::BigInt;

pub type Node = Annot<NodeKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    SelfValue,
    Nil,
    Integer(i64),
    Bignum(BigInt),
    Float(f64),
    Imaginary(NReal),
    Bool(bool),
    String(String),
    Bytes(Vec<u8>),
    InterporatedString(Vec<Node>),
    Command(Box<Node>),
    Symbol(String),
    Range {
        start: Box<Node>,
        end: Box<Node>,
        exclude_end: bool,
        is_const: bool,
    }, // start, end, exclude_end
    Array(Vec<Node>, bool),          // Vec<ELEM>, is_constant_expr
    Hash(Vec<(Node, Node)>),         // Vec<KEY, VALUE>
    RegExp(Vec<Node>, String, bool), // Vec<STRING>, option, is_constant_expr

    LocalVar(usize, String),
    Ident(String),
    InstanceVar(String),
    GlobalVar(String),
    SpecialVar(u32),
    ClassVar(String),
    Const {
        toplevel: bool,
        parent: Option<Box<Node>>,
        prefix: Vec<String>,
        name: String,
    },
    //Scope(Box<Node>, String),
    BinOp(BinOp, Box<Node>, Box<Node>),
    UnOp(UnOp, Box<Node>),
    Index {
        base: Box<Node>,
        index: Vec<Node>,
    },
    Splat(Box<Node>),
    AssignOp(BinOp, Box<Node>, Box<Node>),
    MulAssign(Vec<Node>, Vec<Node>), // mlhs, mrhs

    CompStmt(Vec<Node>),
    If {
        cond: Box<Node>,
        then_: Box<Node>,
        else_: Box<Node>,
    },
    For {
        param: Vec<(usize, String)>,
        iter: Box<Node>,
        body: Box<BlockInfo>,
    },
    While {
        cond: Box<Node>,
        body: Box<Node>,
        cond_op: bool, // true: While, false: Until
        postfix: bool, // true: do .. while *cond*  false: while *cond* do .. end
    },
    Case {
        cond: Option<Box<Node>>,
        when_: Vec<CaseBranch>,
        else_: Box<Node>,
    },
    Begin {
        body: Box<Node>,
        rescue: Vec<RescueEntry>, // (ex_class_list, ex_param)
        else_: Option<Box<Node>>,
        ensure: Option<Box<Node>>,
    },
    Lambda(Box<BlockInfo>),
    Break(Box<Node>),
    Next(Box<Node>),
    Redo,
    Return(Box<Node>),
    Yield(Box<ArgList>),
    MethodDef(String, Box<BlockInfo>), // id, params, body
    SingletonMethodDef(Box<Node>, String, Box<BlockInfo>), // singleton_class, id, params, body
    ClassDef {
        base: Option<Box<Node>>,
        name: String,
        superclass: Option<Box<Node>>,
        info: Box<BlockInfo>,
        is_module: bool,
    },
    SingletonClassDef {
        singleton: Box<Node>,
        info: Box<BlockInfo>,
    },
    MethodCall {
        receiver: Box<Node>,
        method: String,
        arglist: Box<ArgList>,
        safe_nav: bool,
    },
    FuncCall {
        method: String,
        arglist: Box<ArgList>,
        safe_nav: bool,
    },

    Defined(Box<Node>),
    Super(Option<Box<ArgList>>),
    AliasMethod(Box<Node>, Box<Node>), // (new_method, old_method)
    UndefMethod(Box<Node>),            // (new_method, old_method)
    DiscardLhs,
}

impl std::default::Default for NodeKind {
    fn default() -> Self {
        Self::Nil
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockInfo {
    pub params: Vec<FormalParam>,
    pub body: Box<Node>,
    pub lvar: LvarCollector,
    pub loc: Loc,
}

impl BlockInfo {
    pub(crate) fn new(params: Vec<FormalParam>, body: Node, lvar: LvarCollector, loc: Loc) -> Self {
        BlockInfo {
            params,
            body: Box::new(body),
            lvar,
            loc,
        }
    }
}

pub type FormalParam = Annot<ParamKind>;

impl FormalParam {
    pub(crate) fn req_param(name: String, loc: Loc) -> Self {
        FormalParam::new(ParamKind::Param(name), loc)
    }

    pub(crate) fn optional(name: String, default: Node, loc: Loc) -> Self {
        FormalParam::new(ParamKind::Optional(name, Box::new(default)), loc)
    }

    pub(crate) fn rest(name: String, loc: Loc) -> Self {
        FormalParam::new(ParamKind::Rest(Some(name)), loc)
    }

    pub(crate) fn rest_discard(loc: Loc) -> Self {
        FormalParam::new(ParamKind::Rest(None), loc)
    }

    pub(crate) fn post(name: String, loc: Loc) -> Self {
        FormalParam::new(ParamKind::Post(name), loc)
    }

    pub(crate) fn keyword(name: String, default: Option<Node>, loc: Loc) -> Self {
        FormalParam::new(ParamKind::Keyword(name, default.map(Box::new)), loc)
    }

    pub(crate) fn kwrest(name: String, loc: Loc) -> Self {
        FormalParam::new(ParamKind::KWRest(name), loc)
    }

    pub(crate) fn block(name: String, loc: Loc) -> Self {
        FormalParam::new(ParamKind::Block(name), loc)
    }

    pub(crate) fn delegeate(loc: Loc) -> Self {
        FormalParam::new(ParamKind::Delegate, loc)
    }

    pub(crate) fn destruct_param(params: Vec<(String, Loc)>) -> Self {
        let loc = params
            .iter()
            .fold(params[0].1, |acc, elem| acc.merge(elem.1));
        FormalParam::new(ParamKind::Destruct(params), loc)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParamKind {
    Param(String),
    Post(String),
    Optional(String, Box<Node>), // name, default expr
    Rest(Option<String>),
    Keyword(String, Option<Box<Node>>), // name, default expr
    KWRest(String),
    Block(String),
    Delegate,
    Destruct(Vec<(String, Loc)>),
}

impl std::default::Default for ParamKind {
    fn default() -> Self {
        Self::Delegate
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ArgList {
    /// positional args
    pub args: Vec<Node>,
    /// keyword args
    pub kw_args: Vec<(String, Node)>,
    /// double splat args (**{})
    pub hash_splat: Vec<Node>,
    /// block
    pub block: Option<Box<Node>>,
    /// args forwarding
    pub forwarding: bool,
    /// has splat argument
    pub splat: bool,
}

impl ArgList {
    /*pub fn default() -> Self {
        ArgList {
            args: vec![],
            kw_args: vec![],
            hash_splat: vec![],
            block: None,
            forwarding: false,
            splat: false,
        }
    }*/

    pub fn from_args(args: Vec<Node>) -> Self {
        ArgList {
            args,
            kw_args: vec![],
            hash_splat: vec![],
            block: None,
            forwarding: false,
            splat: false,
        }
    }

    pub(crate) fn with_block(block: Box<Node>) -> Self {
        ArgList {
            args: vec![],
            kw_args: vec![],
            hash_splat: vec![],
            block: Some(block),
            forwarding: false,
            splat: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseBranch {
    pub when: Vec<Node>,
    pub body: Box<Node>,
}

impl CaseBranch {
    pub(crate) fn new(when: Vec<Node>, body: Node) -> Self {
        CaseBranch {
            when,
            body: Box::new(body),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum CmpKind {
    Eq = 0,
    Ne = 1,
    Lt = 2,
    Le = 3,
    Gt = 4,
    Ge = 5,
    TEq = 6,
    //Cmp = 7,
}

impl std::fmt::Debug for CmpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::TEq => write!(f, "==="),
            //Self::Cmp => write!(f, "<=>"),
        }
    }
}

impl CmpKind {
    pub fn from(i: u8) -> Self {
        match i {
            0 => Self::Eq,
            1 => Self::Ne,
            2 => Self::Lt,
            3 => Self::Le,
            4 => Self::Gt,
            5 => Self::Ge,
            6 => Self::TEq,
            //7 => Self::Cmp,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,
    Shr,
    Shl,
    BitAnd,
    BitOr,
    BitXor,
    Compare,
    Cmp(CmpKind),
    LAnd,
    LOr,
    Match,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    BitNot,
    Not,
    Pos,
    Neg,
}

impl Node {
    pub fn is_empty(&self) -> bool {
        match &self.kind {
            NodeKind::CompStmt(nodes) => nodes.len() == 0,
            _ => false,
        }
    }

    pub(crate) fn is_integer(&self) -> bool {
        matches!(&self.kind, NodeKind::Integer(_) | NodeKind::Bignum(_))
    }

    pub(crate) fn is_const_expr(&self) -> bool {
        matches!(
            &self.kind,
            NodeKind::Bool(_)
                | NodeKind::Integer(_)
                | NodeKind::Bignum(_)
                | NodeKind::Float(_)
                | NodeKind::Nil
                | NodeKind::Symbol(_)
                | NodeKind::String(_)
                | NodeKind::Bytes(_)
        )
    }

    pub(crate) fn is_symbol_key(&self) -> Option<String> {
        let s = match &self.kind {
            NodeKind::Ident(s) => s,
            NodeKind::LocalVar(_, s) => s,
            NodeKind::Const {
                toplevel: false,
                parent: None,
                prefix,
                name,
            } if prefix.is_empty() => name,
            NodeKind::String(s) => s,
            _ => return None,
        };
        Some(s.clone())
    }

    pub fn new_nil(loc: Loc) -> Self {
        Node::new(NodeKind::Nil, loc)
    }

    pub(crate) fn new_integer(num: i64, loc: Loc) -> Self {
        Node::new(NodeKind::Integer(num), loc)
    }

    pub(crate) fn new_bignum(num: BigInt, loc: Loc) -> Self {
        Node::new(NodeKind::Bignum(num), loc)
    }

    pub(crate) fn new_bool(b: bool, loc: Loc) -> Self {
        Node::new(NodeKind::Bool(b), loc)
    }

    pub(crate) fn new_float(num: f64, loc: Loc) -> Self {
        Node::new(NodeKind::Float(num), loc)
    }

    pub(crate) fn new_imaginary(num: NReal, loc: Loc) -> Self {
        Node::new(NodeKind::Imaginary(num), loc)
    }

    pub(crate) fn new_string(s: RubyString, loc: Loc) -> Self {
        let kind = match s {
            RubyString::Bytes(bytes) => NodeKind::Bytes(bytes),
            RubyString::Utf8(s) => NodeKind::String(s),
        };
        Node::new(kind, loc)
    }

    pub(crate) fn new_array(nodes: Vec<Node>, loc: Loc) -> Self {
        let loc = match nodes.last() {
            Some(node) => loc.merge(node.loc()),
            None => loc,
        };
        let is_const = nodes.iter().all(|n| n.is_const_expr());
        Node::new(NodeKind::Array(nodes, is_const), loc)
    }

    pub(crate) fn new_range(start: Node, end: Node, exclude_end: bool, loc: Loc) -> Self {
        let is_const = start.is_integer() && end.is_integer();
        Node::new(
            NodeKind::Range {
                start: Box::new(start),
                end: Box::new(end),
                exclude_end,
                is_const,
            },
            loc,
        )
    }

    pub(crate) fn new_hash(key_value: Vec<(Node, Node)>, loc: Loc) -> Self {
        Node::new(NodeKind::Hash(key_value), loc)
    }

    pub(crate) fn new_regexp(regex: Vec<Node>, postfix: String, loc: Loc) -> Self {
        let is_const = regex.iter().all(|n| matches!(n.kind, NodeKind::String(_)));
        Node::new(NodeKind::RegExp(regex, postfix, is_const), loc)
    }

    pub(crate) fn new_self(loc: Loc) -> Self {
        Node::new(NodeKind::SelfValue, loc)
    }

    pub(crate) fn new_interporated_string(nodes: Vec<Node>, loc: Loc) -> Self {
        let nodes = nodes
            .into_iter()
            .filter(|node| match &node.kind {
                NodeKind::String(s) => !s.is_empty(),
                _ => true,
            })
            .collect();
        Node::new(NodeKind::InterporatedString(nodes), loc)
    }

    pub(crate) fn new_command(node: Node) -> Self {
        let loc = node.loc;
        Node::new(NodeKind::Command(Box::new(node)), loc)
    }

    pub(crate) fn new_defined(node: Node) -> Self {
        let loc = node.loc;
        Node::new(NodeKind::Defined(Box::new(node)), loc)
    }

    pub(crate) fn new_alias(new: Node, old: Node, loc: Loc) -> Self {
        Node::new(NodeKind::AliasMethod(Box::new(new), Box::new(old)), loc)
    }

    pub(crate) fn new_undef(undef: Node, loc: Loc) -> Self {
        Node::new(NodeKind::UndefMethod(Box::new(undef)), loc)
    }

    pub(crate) fn new_discard(loc: Loc) -> Self {
        Node::new(NodeKind::DiscardLhs, loc)
    }

    pub(crate) fn new_comp_stmt(mut nodes: Vec<Node>, mut loc: Loc) -> Self {
        if nodes.len() == 1 {
            return nodes.remove(0);
        }
        if let Some(node) = nodes.first() {
            loc = node.loc();
        };
        if let Some(node) = nodes.last() {
            loc = loc.merge(node.loc());
        };
        Node::new(NodeKind::CompStmt(nodes), loc)
    }

    pub fn new_binop(op: BinOp, lhs: Node, rhs: Node) -> Self {
        let loc = (lhs.loc()).merge(rhs.loc());
        let kind = NodeKind::BinOp(op, Box::new(lhs), Box::new(rhs));
        Node::new(kind, loc)
    }

    pub(crate) fn new_unop(op: UnOp, lhs: Node, loc: Loc) -> Self {
        let loc = loc.merge(lhs.loc());
        let kind = NodeKind::UnOp(op, Box::new(lhs));
        Node::new(kind, loc)
    }

    pub(crate) fn new_array_member(array: Node, index: Vec<Node>, loc: Loc) -> Self {
        let kind = NodeKind::Index {
            base: Box::new(array),
            index,
        };
        Node::new(kind, loc)
    }

    pub(crate) fn new_splat(array: Node, loc: Loc) -> Self {
        let loc = loc.merge(array.loc());
        Node::new(NodeKind::Splat(Box::new(array)), loc)
    }

    pub(crate) fn new_lvar(name: String, outer: usize, loc: Loc) -> Self {
        Node::new(NodeKind::LocalVar(outer, name), loc)
    }

    pub(crate) fn new_identifier(name: String, loc: Loc) -> Self {
        Node::new(NodeKind::Ident(name), loc)
    }

    pub(crate) fn new_symbol(symbol: String, loc: Loc) -> Self {
        Node::new(NodeKind::Symbol(symbol), loc)
    }

    pub(crate) fn new_instance_var(name: String, loc: Loc) -> Self {
        Node::new(NodeKind::InstanceVar(name), loc)
    }

    pub(crate) fn new_class_var(name: String, loc: Loc) -> Self {
        Node::new(NodeKind::ClassVar(name), loc)
    }

    pub(crate) fn new_global_var(name: String, loc: Loc) -> Self {
        Node::new(NodeKind::GlobalVar(name), loc)
    }

    pub(crate) fn new_special_var(id: u32, loc: Loc) -> Self {
        Node::new(NodeKind::SpecialVar(id), loc)
    }

    pub(crate) fn new_const(
        name: String,
        toplevel: bool,
        parent: Option<Box<Node>>,
        prefix: Vec<String>,
        loc: Loc,
    ) -> Self {
        Node::new(
            NodeKind::Const {
                toplevel,
                parent,
                prefix,
                name,
            },
            loc,
        )
    }

    /*pub(crate) fn new_scope(parent: Node, name: String, loc: Loc) -> Self {
        Node::new(NodeKind::Scope(Box::new(parent), name), loc)
    }*/

    pub(crate) fn new_mul_assign(mlhs: Vec<Node>, mrhs: Vec<Node>) -> Self {
        let splat_flag = mrhs.iter().any(|n| n.is_splat());
        let mrhs = if splat_flag || mlhs.len() == 1 && mrhs.len() != 1 {
            let loc = mrhs[0].loc();
            vec![Node::new_array(mrhs, loc)]
        } else {
            mrhs
        };
        let loc = mlhs[0].loc().merge(mrhs.last().unwrap().loc());
        Node::new(NodeKind::MulAssign(mlhs, mrhs), loc)
    }

    pub(crate) fn new_assign_op(op: BinOp, lhs: Node, rhs: Node) -> Self {
        let loc = lhs.loc().merge(rhs.loc());
        Node::new(NodeKind::AssignOp(op, Box::new(lhs), Box::new(rhs)), loc)
    }

    pub(crate) fn new_method_decl(
        name: String,
        params: Vec<FormalParam>,
        body: Node,
        lvar: LvarCollector,
        loc: Loc,
    ) -> Self {
        let info = BlockInfo::new(params, body, lvar, loc);
        Node::new(NodeKind::MethodDef(name, Box::new(info)), loc)
    }

    pub(crate) fn new_singleton_method_decl(
        singleton: Node,
        name: String,
        params: Vec<FormalParam>,
        body: Node,
        lvar: LvarCollector,
        loc: Loc,
    ) -> Self {
        let info = BlockInfo::new(params, body, lvar, loc);
        Node::new(
            NodeKind::SingletonMethodDef(Box::new(singleton), name, Box::new(info)),
            loc,
        )
    }

    pub(crate) fn new_class_decl(
        base: Option<Node>,
        name: String,
        superclass: Option<Node>,
        body: Node,
        lvar: LvarCollector,
        is_module: bool,
        loc: Loc,
    ) -> Self {
        let info = BlockInfo::new(vec![], body, lvar, loc);
        Node::new(
            NodeKind::ClassDef {
                base: base.map(Box::new),
                name,
                superclass: superclass.map(Box::new),
                info: Box::new(info),
                is_module,
            },
            loc,
        )
    }

    pub(crate) fn new_singleton_class_decl(
        singleton: Node,
        body: Node,
        lvar: LvarCollector,
        loc: Loc,
    ) -> Self {
        let info = BlockInfo::new(vec![], body, lvar, loc);
        Node::new(
            NodeKind::SingletonClassDef {
                singleton: Box::new(singleton),
                info: Box::new(info),
            },
            loc,
        )
    }

    pub(crate) fn new_mcall(
        receiver: Node,
        method: String,
        arglist: ArgList,
        safe_nav: bool,
        loc: Loc,
    ) -> Self {
        Node::new(
            NodeKind::MethodCall {
                receiver: Box::new(receiver),
                method,
                arglist: Box::new(arglist),
                safe_nav,
            },
            loc,
        )
    }

    pub(crate) fn new_mcall_noarg(
        receiver: Node,
        method: String,
        safe_nav: bool,
        loc: Loc,
    ) -> Self {
        let arglist = ArgList::default();
        Node::new(
            NodeKind::MethodCall {
                receiver: Box::new(receiver),
                method,
                arglist: Box::new(arglist),
                safe_nav,
            },
            loc,
        )
    }

    pub(crate) fn new_fcall(method: String, arglist: ArgList, safe_nav: bool, loc: Loc) -> Self {
        Node::new(
            NodeKind::FuncCall {
                method,
                arglist: Box::new(arglist),
                safe_nav,
            },
            loc,
        )
    }

    pub(crate) fn new_fcall_noarg(method: String, safe_nav: bool, loc: Loc) -> Self {
        let arglist = ArgList::default();
        Node::new(
            NodeKind::FuncCall {
                method,
                arglist: Box::new(arglist),
                safe_nav,
            },
            loc,
        )
    }

    pub(crate) fn new_if(cond: Node, then_: Node, else_: Node, loc: Loc) -> Self {
        let loc = loc.merge(then_.loc()).merge(else_.loc());
        Node::new(
            NodeKind::If {
                cond: Box::new(cond),
                then_: Box::new(then_),
                else_: Box::new(else_),
            },
            loc,
        )
    }

    pub(crate) fn new_while(cond: Node, body: Node, cond_op: bool, loc: Loc) -> Self {
        let loc = loc.merge(body.loc());
        Node::new(
            NodeKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
                cond_op,
                postfix: false,
            },
            loc,
        )
    }

    pub(crate) fn new_while_postfix(cond: Node, body: Node, cond_op: bool, loc: Loc) -> Self {
        let loc = loc.merge(body.loc());
        Node::new(
            NodeKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
                cond_op,
                postfix: true,
            },
            loc,
        )
    }

    pub(crate) fn new_case(
        cond: Option<Node>,
        when_: Vec<CaseBranch>,
        else_: Node,
        loc: Loc,
    ) -> Self {
        let loc = loc.merge(else_.loc());
        Node::new(
            NodeKind::Case {
                cond: cond.map(Box::new),
                when_,
                else_: Box::new(else_),
            },
            loc,
        )
    }

    pub(crate) fn new_begin(
        body: Node,
        rescue: Vec<RescueEntry>, //Vec<(Vec<Node>, Box<Node>)>,
        else_: Option<Node>,
        ensure: Option<Node>,
    ) -> Self {
        let mut loc = body.loc();
        let else_ = match else_ {
            Some(else_) => {
                loc = loc.merge(else_.loc);
                Some(Box::new(else_))
            }
            None => None,
        };
        let ensure = match ensure {
            Some(ensure) => {
                loc = loc.merge(ensure.loc());
                Some(Box::new(ensure))
            }
            None => None,
        };
        Node::new(
            NodeKind::Begin {
                body: Box::new(body),
                rescue,
                else_,
                ensure,
            },
            loc,
        )
    }

    pub(crate) fn new_break(val: Node, loc: Loc) -> Self {
        Node::new(NodeKind::Break(Box::new(val)), loc)
    }

    pub(crate) fn new_next(val: Node, loc: Loc) -> Self {
        Node::new(NodeKind::Next(Box::new(val)), loc)
    }

    pub(crate) fn new_redo(loc: Loc) -> Self {
        Node::new(NodeKind::Redo, loc)
    }

    pub(crate) fn new_return(val: Node, loc: Loc) -> Self {
        Node::new(NodeKind::Return(Box::new(val)), loc)
    }

    pub(crate) fn new_yield(args: ArgList, loc: Loc) -> Self {
        Node::new(NodeKind::Yield(Box::new(args)), loc)
    }

    pub(crate) fn new_super(args: impl Into<Option<ArgList>>, loc: Loc) -> Self {
        let args: Option<ArgList> = args.into();
        let args = args.map(Box::new);
        Node::new(NodeKind::Super(args), loc)
    }

    pub(crate) fn new_lambda(
        params: Vec<FormalParam>,
        body: Node,
        lvar: LvarCollector,
        loc: Loc,
    ) -> Self {
        Node::new(
            NodeKind::Lambda(Box::new(BlockInfo::new(params, body, lvar, loc))),
            loc,
        )
    }

    pub fn is_splat(&self) -> bool {
        matches!(self.kind, NodeKind::Splat(_))
    }

    pub fn is_imm_u32(&self) -> Option<u32> {
        if let NodeKind::Integer(i) = self.kind {
            if 0 <= i && i <= u32::MAX as i64 {
                Some(i as u32)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn is_imm_i32(&self) -> Option<i32> {
        if let NodeKind::Integer(i) = self.kind {
            if i32::MIN as i64 <= i && i <= i32::MAX as i64 {
                Some(i as i32)
            } else {
                None
            }
        } else {
            None
        }
    }
}
