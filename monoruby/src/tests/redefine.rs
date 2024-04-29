#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn redefine_test1() {
        run_test_once(
            r##"
        a = [100 * 100]
        class Integer
          def *(other)
            42
          end
        end
        a << 100 * 100
        a
        "##,
        );
    }
}
