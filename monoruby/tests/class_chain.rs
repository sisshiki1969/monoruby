use monoruby::tests::*;

#[test]
fn class_chain1() {
    test_ruby(
        r##"
# Test 1: include + include (via module chain) でMが2回現れるケース
#
# 状況:
#   1. Child が M を include → Child's chain: [Child, M, Parent, ...]
#   2. Parent が N を include (N は M を include している)
#      → Parent's chain: [Parent, N, M, GrandParent, ...]
#      → Child's chain: [Child, M(1回目), Parent, N, M(2回目), GrandParent, Object]
#
# 問題:
#   2回目のM#fooがsuperを呼ぶとき、
#   CRuby: defined_class = 2回目のM_iclass → super = GrandParent → 42
#   monoruby: self_classからownerを探す → 1回目のM_iclassを発見 → super = M_iclass(1回目) → 無限ループ

    module M
      def foo
        super
      end
    end

    module N
      include M
    end

    class GrandParent
      def foo
        42
      end
    end

    class Parent < GrandParent
    end

    class Child < Parent
      include M
    end

    class Parent
      include N
    end

# CRuby:  42
# monoruby: SystemStackError (infinite recursion)
    Child.new.foo
    "##,
    );
}

#[test]
fn class_chain2() {
    test_ruby(
        r##"
# Test 2: include + prepend でMが2回現れるケース
#
# 状況:
#   1. B が M を include → B's chain: [B, M(1回目), A, Object]
#   2. A が M を prepend  → A's chain: [M(prepend host), A(prepend host), A_origin, Object]
#      → B's chain: [B, M(1回目), A(prepend host), M(2回目), A_origin, Object]
#
# 問題:
#   2回目のM#fooがsuperを呼ぶとき、
#   CRuby: defined_class = 2回目のM_iclass → super = A_origin → A#foo → 42
#   monoruby: self_classからownerを探す → 1回目のM_iclassを発見 → 無限ループ

    module M2
      def bar
        super
      end
    end

    class A2
      def bar
        42
      end
    end

    class B2 < A2
      include M2  # M2 を先に B2 に include
    end

    class A2
      prepend M2  # 後から M2 を A2 に prepend (A2 のチェーンにも M2 が追加される)
    end

# B2's chain: [B2, M2(from B2.include), A2(prepend host), M2(from A2.prepend), A2_origin, Object]

# CRuby:  42
# monoruby: SystemStackError (infinite recursion)
    B2.new.bar
    "##,
    );
}
