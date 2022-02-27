use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    If,
    Then,
    Else,
    End,
    Def,
    OParen,
    CParen,
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

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    choice::<_, Simple<char>>((
        just('(').to(Token::OParen),
        just(')').to(Token::CParen),
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
        text::keyword("if").to(Token::If),
        text::keyword("then").to(Token::Then),
        text::keyword("else").to(Token::Else),
        text::keyword("end").to(Token::End),
        text::keyword("def").to(Token::Def),
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
        text::ident().map(Token::Ident),
        choice((just('\n'), just(';')))
            .ignored()
            .repeated()
            .at_least(1)
            .padded()
            .to(Token::Separator),
    ))
    .padded()
    .repeated()
    .then_ignore(end())
}

pub fn parser() -> impl Parser<Token, Vec<Stmt>, Error = Simple<Token>> {
    stmt()
        .separated_by(just(Token::Separator))
        .allow_trailing()
        .allow_leading()
}

///
/// A parser of *Statement*
///
fn stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    decl()
        .map(|decl| Stmt::Decl(decl))
        .or(expr().map(|expr| Stmt::Expr(expr)))
}

fn ident() -> impl Parser<Token, String, Error = Simple<Token>> {
    select! {
        Token::Ident(s) => s
    }
}

///
/// A parser of *Declaration*
///
fn decl() -> impl Parser<Token, Decl, Error = Simple<Token>> {
    just(Token::Def)
        .ignore_then(ident())
        .then(ident().delimited_by(just(Token::OParen), just(Token::CParen)))
        .then(
            expr()
                .separated_by(just(Token::Separator))
                .allow_trailing()
                .allow_leading(),
        )
        .then_ignore(just(Token::End))
        .map(|((func_name, arg_name), body)| Decl::MethodDef(func_name, arg_name, body))
}

///
/// A parser of *Expr*
///
fn expr() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    recursive(|expr| {
        let number = select! {
            Token::Int(i) => Expr::Integer(i),
            Token::Float(f) => Expr::Float(f64::from_ne_bytes(u64::to_ne_bytes(f))),
        };
        let local = select! {
            Token::Ident(s) => Expr::LocalLoad(s),
        };
        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::OParen), just(Token::CParen));
        // primary := <number>
        //          | '(' <expr> ')'
        let primary = choice((number, local, parenthesized));
        // unary := -a
        //        | +a
        let unary = choice((just(Token::Minus), just(Token::Plus)))
            .repeated()
            .then(primary.clone())
            .foldr(|op, lhs| {
                if op == Token::Minus {
                    Expr::Neg(Box::new(lhs))
                } else {
                    lhs
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
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
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
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

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
            .map(|((lhs, kind), rhs)| Expr::Cmp(kind, Box::new(lhs), Box::new(rhs)));

        let assignment = ident()
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(lhs, rhs)| Expr::LocalStore(lhs, Box::new(rhs)));

        let if_expr = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr)
            .then_ignore(just(Token::End))
            .map(|((cond_, then_), else_)| {
                Expr::If(Box::new(cond_), Box::new(then_), Box::new(else_))
            });

        if_expr.or(assignment).or(choice((comparative, additive)))
    })
}
