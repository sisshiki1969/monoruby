# monoruby spec failures report (core/basicobject)

`bin/spec` (`mspec core/basicobject -t monoruby`) の実行結果:
**178 examples, 230 expectations, 7 failures, 3 errors**

---

## Issue 1: `method_missing` による比較演算子の dispatch が動作しない

**種別**: ERROR (1件)
**テスト**: `BasicObject#__send__ has a negative arity`
**ファイル**: `spec/shared/basicobject/send.rb:115`

### エラー内容

```
NoMethodError: undefined method `<' for #<SpecPositiveOperatorMatcher:0x...>
```

### 分析

テストコード `method(@method).arity.should < 0` で、`should` が返す `SpecPositiveOperatorMatcher` に対して `<` が呼ばれる。mspec では `method_missing` 経由で比較演算子をキャッチする仕組みだが、monoruby でこの dispatch が正しく動作していない。

### 修正案

- `SpecPositiveOperatorMatcher` (または親クラス) での `method_missing` の呼び出しチェーンを調査
- monoruby の `method_missing` dispatch が、Fixnum の `<` と競合して `SpecPositiveOperatorMatcher` に対して正しくフォールバックしない可能性を調査
- `BasicObject` を継承したクラスでの演算子メソッド解決を修正

---

## Issue 2: `IO.popen` が未実装

**種別**: ERROR (1件)
**テスト**: `BasicObject raises NoMethodError for nonexistent methods after #method_missing is removed`
**ファイル**: `spec/core/basicobject/basicobject_spec.rb:7`

### エラー内容

```
NoMethodError: undefined method `popen' for IO
```

### 分析

テストは `ruby_exe(script)` ヘルパーを使い、別プロセスで Ruby スクリプトを実行する。`ruby_exe` 内部で `IO.popen` を呼ぶが、monoruby は `IO.popen` を実装していない。

### 修正案

- `IO.popen` を builtins に実装する (`builtins/io.rs` に追加)
- 最低限、コマンド文字列を受け取り `std::process::Command` 経由でサブプロセスを起動し、stdout を `IO` オブジェクトとして返す実装
- もしくは、mspec の `ruby_exe` が使う最小限の機能 (read モード) のみを実装

---

## Issue 3: `instance_eval` の文字列評価時に filename/lineno 引数が無視される

**種別**: FAILURE (6件)
**テスト**:
1. `instance_eval evaluates string with given filename and linenumber` (spec:254)
2. `instance_eval evaluates string with given filename and negative linenumber` (spec:263)
3. `instance_eval converts filename argument with #to_str method` (spec:299)
4. `instance_eval converts lineno argument with #to_int method` (spec:318)
5. `instance_eval raises ArgumentError if returned value is not Integer` (spec:325)
6. `instance_eval uses the caller location as default location` (spec:89)

### エラー内容

```
Expected ["(eval)", 1] == ["(eval at .../instance_eval_spec.rb:89)", 1]
Expected ["/home/user/spec/.../instance_eval_spec.rb", "235"] == ["a_file", "10"]
Expected ["<internal>", "in 'Kernel#raise'"] == ["b_file", "-98"]
Expected "<internal>" == "file.rb"
Expected "in 'BasicObject#instance_eval'" == "15"
```

### 分析

`instance_eval(string, filename, lineno)` の第2・第3引数が無視されている。文字列を eval する際に:
- `filename` がバックトレースに反映されない (常に `"(eval)"` や `"<internal>"` になる)
- `lineno` がバックトレースに反映されない
- `filename` に対する `#to_str` 変換が行われていない
- `lineno` に対する `#to_int` 変換が行われていない
- デフォルトの filename が `"(eval at <caller_file>:<caller_line>)"` 形式になっていない

### 修正案

- `instance_eval` の文字列評価パス (builtins/kernel.rs or builtins/object.rs) を修正
- パーサ (`ruruby-parse`) にファイル名・行番号を渡す引数を追加
- eval 時のソース位置情報をバックトレースに正しく反映する
- `filename` 引数に対して `to_str` を呼ぶ型変換を追加
- `lineno` 引数に対して `to_int` を呼ぶ型変換を追加
- デフォルト filename を `"(eval at #{caller_file}:#{caller_line})"` 形式にする

---

## Issue 4: `instance_eval` が4引数以上でエラーメッセージが不正

**種別**: ERROR (1件)
**テスト**: `instance_eval raises an ArgumentError when more than 3 arguments are given`
**ファイル**: `spec/core/basicobject/instance_eval_spec.rb:33`

### エラー内容

```
ArgumentError: wrong number of arguments (given 4, expected 0..3)
```

テストの期待値:
```
ArgumentError: wrong number of arguments (given 4, expected 1..3)
```

### 分析

`instance_eval` の引数バリデーションが `expected 0..3` を返しているが、CRuby では `expected 1..3` を返す。文字列引数は1つ以上必要なので、最小引数数が 0 ではなく 1 であるべき。

ただし、ブロック付き呼び出しの場合は 0 引数が有効なため、ブロックの有無で arity を変える必要がある。

### 修正案

- `instance_eval` の arity 定義を修正: ブロックなしの場合は `1..3`、ブロックありの場合は `0` とする
- もしくは、引数チェックロジックを手動で実装し、ブロックと文字列引数の組み合わせに応じた適切なエラーメッセージを出す

---

## Issue 5: frozen オブジェクトへの `instance_eval` 内での ivar 設定で `FrozenError` が発生しない

**種別**: FAILURE (1件)
**テスト**: `instance_eval raises TypeError for frozen objects when tries to set receiver's instance variables`
**ファイル**: `spec/core/basicobject/instance_eval_spec.rb:106`

### エラー内容

```
Expected FrozenError but no exception was raised (42 was returned)
```

テストコード:
```ruby
obj = Object.new
obj.freeze
-> { obj.instance_eval { @foo = 42 } }.should raise_error(FrozenError)
```

### 分析

frozen オブジェクトに対して `@foo = 42` でインスタンス変数を設定しようとしても `FrozenError` が発生していない。monoruby のインスタンス変数設定処理で freeze チェックが欠落している。

### 修正案

- インスタンス変数設定のバイトコード実行時 (`SetIvar` 系の処理) で、対象オブジェクトが frozen かどうかをチェックし、frozen なら `FrozenError` を raise する
- 該当箇所は `executor/` 内の ivar 設定処理と、JIT コンパイラの ivar 設定コード生成の両方
- Fixnum, Symbol, true, false, nil 等の immediate value も常に frozen なので、これらに対しても同様のチェックが必要
