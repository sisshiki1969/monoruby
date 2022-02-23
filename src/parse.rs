use super::*;

pub fn parser() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
    expr()
        .separated_by(just(';').repeated().at_least(1))
        .allow_trailing()
        .allow_leading()
        .then_ignore(end())
}

///
/// A parser of *Expr*
///
fn expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    let digits = text::digits::<_, Simple<char>>(10);
    let int = text::int::<_, Simple<char>>(10);

    // number := <integer> | <float>
    let number = int
        .then(just('.').ignore_then(digits).or_not())
        .map(|(int, frac)| match frac {
            Some(frac) => {
                let f = format!("{}.{}", int, frac).parse::<f64>().unwrap();
                Expr::Float(f)
            }
            None => {
                let i = int.parse::<i32>().unwrap();
                Expr::Integer(i)
            }
        });

    let local = text::ident::<_, Simple<char>>().map(|s| Expr::LocalLoad(s));

    let ident = text::ident::<_, Simple<char>>();

    let op = |ch: &str| just(ch.to_string()).padded();

    recursive(|expr| {
        let parenthesized = expr.clone().delimited_by(just('('), just(')'));
        // primary := <number>
        //          | '(' <expr> ')'
        let primary = choice((number, local, parenthesized));
        // unary := -a
        //        | +a
        let unary = choice((op("-"), op("+")))
            .repeated()
            .then(primary.clone())
            .foldr(|op, lhs| {
                if op == "-" {
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
                    op("*").to(Expr::Mul as fn(_, _) -> _),
                    op("/").to(Expr::Div as fn(_, _) -> _),
                ))
                .then(unary.clone())
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        // additive := a + b
        //           | a - b
        let additive = multiplicative
            .clone()
            .then(
                choice((
                    op("+").to(Expr::Add as fn(_, _) -> _),
                    op("-").to(Expr::Sub as fn(_, _) -> _),
                ))
                .then(multiplicative.clone())
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let comparative = additive
            .clone()
            .then(choice((
                op("==").to(CmpKind::Eq),
                op("!=").to(CmpKind::Ne),
                op(">=").to(CmpKind::Ge),
                op("<=").to(CmpKind::Le),
                op(">").to(CmpKind::Gt),
                op("<").to(CmpKind::Lt),
            )))
            .then(additive.clone())
            .map(|((lhs, kind), rhs)| Expr::Cmp(kind, Box::new(lhs), Box::new(rhs)));

        let assignment = ident
            .padded()
            .then_ignore(op("="))
            .then(expr.clone())
            .map(|(lhs, rhs)| Expr::LocalStore(lhs, Box::new(rhs)));

        let if_expr = just("if")
            .ignore_then(expr.clone())
            .then_ignore(just("then"))
            .then(expr.clone())
            .then_ignore(just("else"))
            .then(expr)
            .then_ignore(just("end"))
            .map(|((cond_, then_), else_)| {
                Expr::If(Box::new(cond_), Box::new(then_), Box::new(else_))
            });

        if_expr
            .or(assignment)
            .or(choice((comparative, additive, multiplicative, primary)))
            .padded()
    })
}
