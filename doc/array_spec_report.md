# ruby/spec core/array 失敗レポート

**実行日**: 2026-04-05
**対象**: monoruby 0.3.0 (release build)
**結果**: 128 files, 3097 examples, 4923 expectations, **283 failures, 328 errors**, 0 tagged

---

## 共通パターン別の優先度リスト

影響が大きく(多くのspecを修正できる)、実装が容易な順に並べています。

### 1. Enumerator#size 未実装 — 27件 (容易)

`delete_if`, `select!`, `filter!`, `keep_if`, `reject!`, `collect!`, `map!`, `each`, `each_index`, `reverse_each`, `select`, `filter`, `reject`, `collect`, `map`, `sort_by!`, `combination`, `repeated_combination`, `repeated_permutation`, `find_index`, `count`, `bsearch_index` など多数のメソッドが返す `Enumerator` の `size` が正しく返されていない。ブロックなし呼び出し時に `Enumerator.new { ... }.size` が配列サイズを返すようにすれば一括で修正可能。

### 2. Array#[] / #slice の ArithmeticSequence 対応 — 18件 (中程度)

`Enumerator::ArithmeticSequence` (例: `(0..).step(2)`) を引数に取るスライスが `NoMethodError` になる。ArithmeticSequence を検出して step/range を取り出すロジックの追加が必要。`#[]` と `#slice` で共有されるので18件一気に修正可能。

### 3. イテレーション中の配列サイズ増加への耐性 — 17件 (中程度)

`all?`, `any?`, `delete_if`, `keep_if`, `filter!`, `select!`, `reject!`, `uniq`, `uniq!`, `sort_by!`, `to_h`, `find_index`, `index`, `count`, `filter`, `select`, `reject` で、ブロック内で配列サイズが増えた場合に正しく動作しない。イテレータが開始時の長さでループを止めている可能性。ループ条件を毎回 `self.len()` で再評価するよう修正。

### 4. `#to_ary` 強制変換の不備 — 約12件 (中程度)

`Array#initialize`, `Array.new`, `Array#to_h`, `Array#zip`, `Array#product`, `Array.try_convert`, `Array#==`, `Array#flatten` 等で `#to_ary` メソッド呼び出しによる型変換が不足。`respond_to?(:to_ary)` → `to_ary` 呼び出しのパターンを適切に実装する必要あり。

### 5. pattern引数がある時にblockを無視する — 8件 (容易)

`all?`, `any?`, `none?`, `one?`, `count`, `index`, `find_index`, `rindex` でパターン引数とブロックの両方が渡されたとき、ブロックを無視すべきだが現在はブロックを優先している。条件分岐の優先順位を修正するだけ。

### 6. Array#pack — 434件 (困難)

全体の70%を占めるが、`pack` のテンプレート文字(`w`, `U`, `Q`, `q`, `J`, `j`等)ごとの境界値処理・エンコーディング処理が大量に不足。実装コストが非常に高い。

### 7. その他の個別修正 (各1-6件)

| メソッド | 件数 | 内容 |
|---|---|---|
| `join` | 6 | エンコーディング処理、空配列のUS-ASCII、`$,`警告 |
| `permutation` | 5 | Enumerator size計算(階乗)、変更後の配列対応 |
| `flatten` | 6 | `respond_to_missing?`/`method_missing`対応 |
| `fill` | 4 | 引数チェック、nil length対応 |
| `hash` | 2 | 再帰配列、`to_int`対応 |
| `inspect`/`to_s` | 4 | エンコーディング関連 |
| `&`/`-` | 3 | `eql?`/`hash`での等価判定 |
| `<=>` | 1 | 左から右への比較順 |
| `clone` | 1 | frozen状態のコピー |
| `shuffle!` | 2 | `random:`/`srand`対応 |

---

## 推奨実装順

1. **Enumerator#size** (27件修正、低コスト) — Enumeratorに`size`を渡す仕組みの追加
2. **pattern引数時のblock無視** (8件修正、低コスト) — 条件分岐の修正のみ
3. **イテレーション中のサイズ変更耐性** (17件修正、中コスト) — ループ条件の修正
4. **ArithmeticSequence対応** (18件修正、中コスト) — 新しい引数型の処理追加
5. **#to_ary強制変換** (12件修正、中コスト) — 複数メソッドでの共通パターン
6. **Array#pack** (434件、高コスト) — 最後に回すべき大規模実装

---

## メソッド別集計

| Method | Fail | Err | Total | Top Error Types |
|---|---|---|---|---|
| `Array#pack` | 147 | 287 | 434 | RangeError(157), assertion(147), TypeError(128) |
| `Array#[]` | 0 | 10 | 10 | NoMethodError(9), ArgumentError(1) |
| `Array#slice` | 0 | 10 | 10 | NoMethodError(9), ArgumentError(1) |
| `Array#initialize` | 7 | 2 | 9 | assertion(7), TypeError(2) |
| `Array#flatten` | 6 | 0 | 6 | assertion(6) |
| `Array#join` | 5 | 1 | 6 | assertion(5), TypeError(1) |
| `Array#permutation` | 5 | 0 | 5 | assertion(5) |
| `Array#delete_if` | 4 | 0 | 4 | assertion(4) |
| `Array#[]=` | 1 | 3 | 4 | IndexError(2), ArgumentError(1), assertion(1) |
| `Array#fill` | 3 | 1 | 4 | assertion(3), TypeError(1) |
| `Array#repeated_combination` | 4 | 0 | 4 | assertion(4) |
| `Array#zip` | 0 | 4 | 4 | NoMethodError(2), ArgumentError(1), TypeError(1) |
| `Array#filter!` | 3 | 0 | 3 | assertion(3) |
| `Array#keep_if` | 3 | 0 | 3 | assertion(3) |
| `Array.new` | 2 | 1 | 3 | assertion(2), TypeError(1) |
| `Array#reject!` | 3 | 0 | 3 | assertion(3) |
| `Array#select!` | 3 | 0 | 3 | assertion(3) |
| `Array#to_h` | 1 | 2 | 3 | TypeError(2), assertion(1) |
| `Array#all?` | 2 | 0 | 2 | assertion(2) |
| `Array#any?` | 2 | 0 | 2 | assertion(2) |
| `Array#collect!` | 2 | 0 | 2 | assertion(2) |
| `Array#combination` | 2 | 0 | 2 | assertion(2) |
| `Array#count` | 2 | 0 | 2 | assertion(2) |
| `Array#==` | 2 | 0 | 2 | assertion(2) |
| `Array#filter` | 2 | 0 | 2 | assertion(2) |
| `Array#find_index` | 2 | 0 | 2 | assertion(2) |
| `Array#hash` | 2 | 0 | 2 | assertion(2) |
| `Array#index` | 2 | 0 | 2 | assertion(2) |
| `Array#inspect` | 2 | 0 | 2 | assertion(2) |
| `Array#&` | 2 | 0 | 2 | assertion(2) |
| `Array#map!` | 2 | 0 | 2 | assertion(2) |
| `Array#reject` | 2 | 0 | 2 | assertion(2) |
| `Array#repeated_permutation` | 2 | 0 | 2 | assertion(2) |
| `Array#reverse_each` | 2 | 0 | 2 | assertion(2) |
| `Array#rindex` | 2 | 0 | 2 | assertion(2) |
| `Array#select` | 2 | 0 | 2 | assertion(2) |
| `Array#shuffle!` | 1 | 1 | 2 | assertion(1), NoMethodError(1) |
| `Array#sort_by!` | 2 | 0 | 2 | assertion(2) |
| `Array#to_s` | 2 | 0 | 2 | assertion(2) |
| `Array#bsearch_index` | 1 | 0 | 1 | assertion(1) |
| `Array#clone` | 1 | 0 | 1 | assertion(1) |
| `Array#collect` | 1 | 0 | 1 | assertion(1) |
| `Array#<=>` | 1 | 0 | 1 | assertion(1) |
| `Array#cycle` | 0 | 1 | 1 | NoMethodError(1) |
| `Array#delete` | 0 | 1 | 1 | FrozenError(1) |
| `Array#dup` | 1 | 0 | 1 | assertion(1) |
| `Array#each_index` | 1 | 0 | 1 | assertion(1) |
| `Array#each` | 1 | 0 | 1 | assertion(1) |
| `Array#eql?` | 1 | 0 | 1 | assertion(1) |
| `Array#fetch` | 1 | 0 | 1 | assertion(1) |
| `Array#map` | 1 | 0 | 1 | assertion(1) |
| `Array#minmax` | 0 | 1 | 1 | NoMethodError(1) |
| `Array#-` | 1 | 0 | 1 | assertion(1) |
| `Array#none?` | 1 | 0 | 1 | assertion(1) |
| `Array#one?` | 1 | 0 | 1 | assertion(1) |
| `Array#product` | 0 | 1 | 1 | TypeError(1) |
| `Array#slice!` | 0 | 1 | 1 | ArgumentError(1) |
| `Array#sum` | 1 | 0 | 1 | assertion(1) |
| `Array.try_convert` | 0 | 1 | 1 | TypeError(1) |
| `Array#uniq` | 1 | 0 | 1 | assertion(1) |
| `Array#uniq!` | 1 | 0 | 1 | assertion(1) |
