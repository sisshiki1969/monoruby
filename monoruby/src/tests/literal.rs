#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn imaginary() {
        run_test(r#"5i"#);
        run_test(r#"4+5i"#);
        run_test(r#"4000000000000000000000000000000+5000000000000000000000000000000i"#);
        run_test(r#"4.27+1.5i"#);
        run_test(r#"4-5i"#);
        run_test(r#"4000000000000000000000000000000-5000000000000000000000000000000i"#);
        run_test(r#"4.27-1.5i"#);
        run_test(r#"(4.27-1.5i) + (4-5i)"#);
        run_test(r#"(4.27-1.5i) - (4-5i)"#);
        run_test(r#"(4.27-1.5i) * (4-5i)"#);
        //run_test(r#"(4.27-1.5i) / (4-5i)"#);
        //run_test(r#"(4.27-1.5i) % (4-5i)"#);
    }
}
