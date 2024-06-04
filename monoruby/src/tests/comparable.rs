#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn comparable() {
        run_test_with_prelude(
            r##"
          res = []
          res << ( C.new(1) == C.new(2) )
          res << ( C.new(1) == C.new(1) )
          res << ( C.new(2) == C.new(1) )
          res << ( C.new(1) != C.new(2) )
          res << ( C.new(1) != C.new(1) )
          res << ( C.new(2) != C.new(1) )
          res << ( C.new(1) >= C.new(2) )
          res << ( C.new(1) >= C.new(1) )
          res << ( C.new(2) >= C.new(1) )
          res << ( C.new(1) > C.new(2) )
          res << ( C.new(1) > C.new(1) )
          res << ( C.new(2) > C.new(1) )
          res << ( C.new(1) <= C.new(2) )
          res << ( C.new(1) <= C.new(1) )
          res << ( C.new(2) <= C.new(1) )
          res << ( C.new(1) < C.new(2) )
          res << ( C.new(1) < C.new(1) )
          res << ( C.new(2) < C.new(1) )
          res
"##,
            r##"
          class C
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              self.x <=> other.x
            end
          end
            "##,
        );
    }
}
