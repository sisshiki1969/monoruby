use super::*;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    expr().then_ignore(end())
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

    let op = |ch: char| just(ch).padded();

    recursive(|expr| {
        let parenthesized = expr.delimited_by(just('('), just(')'));
        // primary := <number>
        //          | '(' <expr> ')'
        let primary = choice((number, local, parenthesized));
        // unary := -a
        //        | +a
        let unary = choice((op('-'), op('+')))
            .repeated()
            .then(primary)
            .foldr(|op, lhs| {
                if op == '-' {
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
                    op('*').to(Expr::Mul as fn(_, _) -> _),
                    op('/').to(Expr::Div as fn(_, _) -> _),
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
                    op('+').to(Expr::Add as fn(_, _) -> _),
                    op('-').to(Expr::Sub as fn(_, _) -> _),
                ))
                .then(multiplicative)
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));
        let assignment = ident
            .then_ignore(op('='))
            .then(additive.clone())
            .map(|(lhs, rhs)| Expr::LocalStore(lhs, Box::new(rhs)));

        let assign_expr = choice((assignment, additive));
        assign_expr.padded()
    })
}
