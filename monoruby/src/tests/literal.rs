#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn imaginary() {
        run_test(r#"5i"#);
        run_test(r#"4+5i"#);
    }
}
