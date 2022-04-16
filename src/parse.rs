use super::*;

use ariadne::*;
use chumsky::{error::Cheap, prelude::*, Stream};
pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    If,
    Then,
    Else,
    End,
    While,
    Do,
    Def,
    Call(String),
    OParen,
    CParen,
    Comma,
    Minus,
    Plus,
    Mul,
    Div,
    Assign,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    Int(i32),
    Float(u64),
    Ident(String),
    Separator,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
            Self::End => write!(f, "end"),
            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::Def => write!(f, "def"),
            Self::Call(s) => write!(f, "call {}", s),
            Self::OParen => write!(f, "("),
            Self::CParen => write!(f, ")"),
            Self::Comma => write!(f, ","),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Assign => write!(f, "="),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(n) => write!(f, "{}", f64::from_ne_bytes(u64::to_ne_bytes(*n))),
            Self::Ident(s) => write!(f, "{}", s),
            Self::Separator => write!(f, ";"),
        }
    }
}

pub struct MonorubyParser {}

impl MonorubyParser {
    pub fn parse(code: &str) -> Result<Vec<Spanned<Stmt>>, ()> {
        let len = code.len();
        let (tokens, errs) = lexer().parse_recovery(code);
        //dbg!(&tokens);
        let (mut ast, parse_errs) = if let Some(tokens) = tokens {
            let (ast, parse_err) =
                parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
            (ast.map_or(vec![], |ast| ast), parse_err)
        } else {
            (vec![], vec![])
        };
        add_return(&mut ast);
        if !errs.is_empty() || !parse_errs.is_empty() {
            Self::show_err(errs, parse_errs, code);
            Err(())
        } else {
            Ok(ast)
        }
    }

    fn show_err(errs: Vec<Cheap<char>>, parse_errs: Vec<Cheap<Token>>, code: &str) {
        errs.into_iter().for_each(|e| {
            let mut rep = Report::build(ReportKind::Error, (), e.span().start);
            rep = rep.with_label(
                Label::new(e.span()).with_message(format!("unexpected:{:?}", e.label())),
            );
            rep.finish().eprint(Source::from(code)).unwrap();
        });
        parse_errs.into_iter().for_each(|e| {
            let mut rep = Report::build(ReportKind::Error, (), e.span().start);
            rep = rep.with_label(Label::new(e.span()).with_message(format!(
                "unexpected:{}",
                match e.label() {
                    Some(s) => s,
                    None => "",
                }
            )));
            rep.finish().eprint(Source::from(code)).unwrap();
        });
    }
}

fn add_return(ast: &mut Vec<Spanned<Stmt>>) {
    for (stmt, _) in ast.iter_mut() {
        match stmt {
            Stmt::Decl((Decl::MethodDef(_, _, ref mut body), _)) => {
                let len = body.len();
                assert!(len != 0);
                let (expr, span) = std::mem::take(&mut body[len - 1]);
                body[len - 1].0 = Expr::ret(Stmt::expr((expr, span)));
            }
            _ => {}
        }
    }
    let len = ast.len();
    if len == 0 {
        let span = Span::default();
        let nil = Stmt::expr((Expr::Nil, span.clone()));
        let ret = Stmt::expr((Expr::Return(Box::new(nil)), span));
        ast.push(ret);
    } else {
        let (stmt, span) = std::mem::take(&mut ast[len - 1]);
        ast[len - 1] = (
            Stmt::Expr((Expr::ret((stmt, span.clone())), span.clone())),
            span,
        );
    }
}

fn reserved() -> impl Parser<char, Token, Error = Cheap<char>> {
    choice((
        text::keyword("if").to(Token::If),
        text::keyword("then").to(Token::Then),
        text::keyword("else").to(Token::Else),
        text::keyword("end").to(Token::End),
        text::keyword("while").to(Token::While),
        text::keyword("do").to(Token::Do),
        text::keyword("def").to(Token::Def),
    ))
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Cheap<char>> {
    let token = choice::<_, Cheap<char>>((
        just('(').to(Token::OParen),
        just(')').to(Token::CParen),
        just(',').to(Token::Comma),
        just('-').to(Token::Minus),
        just('+').to(Token::Plus),
        just('*').to(Token::Mul),
        just('/').to(Token::Div),
        just("==").to(Token::Eq),
        just("!=").to(Token::Ne),
        just(">=").to(Token::Ge),
        just("<=").to(Token::Le),
        just('=').to(Token::Assign),
        just(">").to(Token::Gt),
        just("<").to(Token::Lt),
        reserved(),
        text::int(10)
            .then(just('.').ignore_then(text::digits(10)).or_not())
            .map(|(int, frac)| match frac {
                Some(frac) => {
                    let f = format!("{}.{}", int, frac).parse::<f64>().unwrap();
                    Token::Float(u64::from_ne_bytes(f.to_ne_bytes()))
                }
                None => {
                    let i = int.parse::<i32>().unwrap();
                    Token::Int(i)
                }
            }),
        text::int(10).from_str().unwrapped().map(Token::Int),
        text::ident().then_ignore(just('(')).map(Token::Call),
        text::ident().map(Token::Ident),
        one_of(" \t")
            .repeated()
            .ignored()
            .then(
                choice((just('\n'), just(';')))
                    .ignored()
                    .then_ignore(one_of(" ;\t\n").repeated()),
            )
            .to(Token::Separator),
    ));

    one_of(" \t")
        .repeated()
        .ignore_then(token)
        .map_with_span(|tok, span| (tok, span))
        .then_ignore(one_of(" \t").repeated())
        .repeated()
        .then_ignore(end())
}

pub fn parser() -> impl Parser<Token, Vec<Spanned<Stmt>>, Error = Cheap<Token>> {
    stmt()
        .separated_by(just(Token::Separator))
        .allow_trailing()
        .allow_leading()
        .or_not()
        .then_ignore(end())
        .map(|opt| match opt {
            Some(v) => v,
            None => vec![],
        })
}

///
/// A parser of *Statement*
///
fn stmt() -> impl Parser<Token, Spanned<Stmt>, Error = Cheap<Token>> {
    decl()
        .map(|(decl, span)| (Stmt::Decl((decl, span.clone())), span))
        .or(expr().map(|(expr, span)| (Stmt::Expr((expr, span.clone())), span)))
}

fn ident() -> impl Parser<Token, String, Error = Cheap<Token>> {
    select! {
        Token::Ident(s) => s
    }
}

fn ident_list() -> impl Parser<Token, Vec<String>, Error = Cheap<Token>> {
    select! {
        Token::Ident(s) => s
    }
    .separated_by(just(Token::Comma))
}

///
/// A parser of *Declaration*
///
fn decl() -> impl Parser<Token, Spanned<Decl>, Error = Cheap<Token>> {
    just(Token::Def)
        .ignore_then(select! {Token::Call(s) => s})
        .then(ident_list())
        .then_ignore(just(Token::CParen))
        .then(
            expr()
                .separated_by(just(Token::Separator))
                .allow_trailing()
                .allow_leading(),
        )
        .then_ignore(just(Token::End))
        .map_with_span(|((func_name, arg_name), body), span| {
            (Decl::MethodDef(func_name, arg_name, body), span)
        })
}

///
/// A parser of *Expr*
///
fn expr() -> impl Parser<Token, Spanned<Expr>, Error = Cheap<Token>> {
    recursive(|expr: Recursive<Token, Spanned<Expr>, Cheap<Token>>| {
        let exprs = expr
            .clone()
            .separated_by(just(Token::Separator))
            .allow_leading()
            .allow_trailing();

        let number = select! {
            Token::Int(i) => Expr::Integer(i),
            Token::Float(f) => Expr::Float(f64::from_ne_bytes(u64::to_ne_bytes(f))),
        }
        .map_with_span(|expr, span| (expr, span));

        let local = select! {
            Token::Ident(s) => Expr::LocalLoad(s),
        }
        .map_with_span(|expr, span| (expr, span));

        let call = select! {Token::Call(s) => s}
            .map_with_span(|s, span| (s, span))
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing(),
            )
            .then_ignore(just(Token::CParen))
            .map_with_span(|(f_name, arg), span| {
                (Expr::Call(f_name.0, arg), f_name.1.start..span.end)
            });

        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::OParen), just(Token::CParen));

        // primary := <number>
        //          | '(' <expr> ')'
        let primary = choice((call, number, local, parenthesized));

        // unary := -a
        //        | +a
        let unary = choice((just(Token::Minus), just(Token::Plus)))
            .map_with_span(|tok, span: Span| (tok, span))
            .repeated()
            .then(primary.clone())
            .foldr(|op, rhs| {
                let span = op.1.start..rhs.1.end;
                if op.0 == Token::Minus {
                    (Expr::Neg(Box::new(rhs)), span)
                } else {
                    (rhs.0, span)
                }
            });

        // multiplicative := a * b
        //                 | a / b
        let multiplicative = unary
            .clone()
            .then(
                choice((
                    just(Token::Mul).to(Expr::Mul as fn(_, _) -> _),
                    just(Token::Div).to(Expr::Div as fn(_, _) -> _),
                ))
                .then(unary)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;
                (op(Box::new(lhs), Box::new(rhs)), span)
            });

        // additive := a + b
        //           | a - b
        let additive = multiplicative
            .clone()
            .then(
                choice((
                    just(Token::Plus).to(Expr::Add as fn(_, _) -> _),
                    just(Token::Minus).to(Expr::Sub as fn(_, _) -> _),
                ))
                .then(multiplicative)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;
                (op(Box::new(lhs), Box::new(rhs)), span)
            });

        let comparative = additive
            .clone()
            .then(choice((
                just(Token::Eq).to(CmpKind::Eq),
                just(Token::Ne).to(CmpKind::Ne),
                just(Token::Ge).to(CmpKind::Ge),
                just(Token::Le).to(CmpKind::Le),
                just(Token::Gt).to(CmpKind::Gt),
                just(Token::Lt).to(CmpKind::Lt),
            )))
            .then(additive.clone())
            .map(|((lhs, kind), rhs)| {
                let span = lhs.1.start..rhs.1.end;
                (Expr::Cmp(kind, Box::new(lhs), Box::new(rhs)), span)
            });

        let assignment = ident()
            .then_ignore(just(Token::Assign))
            .map_with_span(|expr, span| (expr, span))
            .then(expr.clone())
            .map(|(lhs, rhs)| {
                let span = lhs.1.start..rhs.1.end;
                (Expr::LocalStore(lhs.0, Box::new(rhs)), span)
            });

        let if_expr = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(choice((
                just(Token::Then),
                just(Token::Separator).then_ignore(just(Token::Then).or_not()),
            )))
            .or_else(|e| Err(e.with_label("expected 'then' or ';' or new line.")))
            .then(exprs.clone())
            .then_ignore(just(Token::Else))
            .or_else(|e| Err(e.with_label("expected 'else'.")))
            .then(exprs.clone())
            .then(just(Token::End).map_with_span(|_, span| span))
            .or_else(|e| Err(e.with_label("expected 'end'.")))
            .map(|(((cond_, then_), else_), end_span)| {
                let span = cond_.1.start..end_span.end;
                (Expr::If(Box::new(cond_), then_, else_), span)
            });
        let while_expr = just(Token::While)
            .map_with_span(|_, span| span)
            .then(expr.clone())
            .then_ignore(just(Token::Do))
            .or_else(|e| Err(e.with_label("expected 'do'.")))
            .then(exprs.clone())
            .then(just(Token::End).map_with_span(|_, span| span))
            .or_else(|e| Err(e.with_label("expected 'end'.")))
            .map(|(((span1, cond), body), span2)| {
                let span = span1.start..span2.end;
                (Expr::While(Box::new(cond), body), span)
            });

        if_expr
            .or(while_expr)
            .or(assignment)
            .or(choice((comparative, additive)))
    })
}
