//! `&:sym` called with nothing to be the receiver (#1380). The
//! `Symbol#to_proc` body is declared with one required parameter so its
//! `arity` / `parameters` read as CRuby's, which made a yield of nothing
//! fail the arity check ("wrong number of arguments (given 0, expected
//! 1+)") before the body could say "no receiver given". Keywords alone
//! are a receiver: `:to_s.to_proc.call(k: 1)` is `{k: 1}.to_s`.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn yield_of_nothing_says_no_receiver_given() {
    run_test(
        r##"
        def y0(&b) = yield
        def y1(&b) = yield(1)
        def yk(&b) = yield(k: 1)
        def ys(&b) = yield(*[])
        pr = :to_s.to_proc
        res = [pr.arity, pr.lambda?, pr.parameters]
        [-> { y0(&:to_s) }, -> { pr.call }, -> { pr.() }, -> { pr.yield }, -> { pr[] },
         -> { pr.call(nil) }, -> { y1(&:to_s) }, -> { yk(&:to_s) }, -> { ys(&:to_s) },
         -> { pr.call(k: 1) }, -> { pr.call({k: 1}) }, -> { :+.to_proc.call },
         -> { :+.to_proc.call(1) }, -> { :+.to_proc.yield(1, 2) }, -> { y0(&:itself) },
         -> { [nil].each(&:to_s) }, -> { [[1, 2]].map(&:first) }].each do |l|
          res << (begin; l.call; rescue ArgumentError, NoMethodError => e; "#{e.class}: #{e.message}"; end)
        end
        res
        "##,
    );
}

#[test]
fn define_method_with_symbol_proc() {
    run_test_once(
        r##"
        class C
          define_method(:dm, &:to_s)
        end
        c = C.new
        [(c.dm rescue $!.message), c.dm(5), (c.dm(k: 1) rescue $!.message)]
        "##,
    );
}
