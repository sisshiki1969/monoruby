# monoruby SEGV/パニック/ハング 調査レポート

## 概要

ruby/spec core カテゴリ全58分野を mspec で実行し、SEGV/パニック(SIGABRT)/ハング(タイムアウト)が発生するケースを特定しました。

**前回調査からの差分:**
- Range#to_a SEGV → **解消**
- Range#count/min/max/minmax HANG×4 → **解消** (PR #215 で実装)
- String#modulo PANIC → 原因が変化 (バイトコード生成assert → JIT recompile)

## ruby/spec core 全体結果

| 指標 | 件数 | 前回 |
|------|------|------|
| カテゴリ総数 | 58 | 58 |
| 正常完了カテゴリ | 52 | 49 |
| **SEGV** | **0件** | 1件 |
| **パニック (SIGABRT)** | **5件** | 7件 |
| **ハング (タイムアウト)** | **2件** | 6件 |

---

## SEGV (0件)

前回報告の Range#to_a SEGV は解消済み。

---

## パニック — SIGABRT (5件)

### 1. JIT再コンパイル/コンパイル時パニック (影響: array, string/modulo, numeric/step)

3つのカテゴリで同じJITパニックが発生。根本原因は共通と推定。

| spec | パニック箇所 |
|------|-------------|
| `core/array` (全体実行時) | `jit_recompile_method_with_recovery` |
| `core/string/modulo_spec.rb` | `jit_recompile_method_with_recovery` |
| `core/numeric/step_spec.rb` | `jit_compile_patch` |

- **原因**: JITがメソッドを再コンパイル/パッチする際、インラインキャッシュまたは型状態の不整合でパニック。多数のspecを連続実行してJITキャッシュが蓄積した状態でのみ発生。個別spec単独実行では再現しないケースもある。
- **前回との差分**: String#modulo は前回 `bytecodegen/expression.rs:451` のassert失敗だったが、今回は JIT recompile パニックに変化。

### 2. Fiber#transfer — send() 内パニック

| 項目 | 詳細 |
|------|------|
| **spec** | `core/fiber/transfer_spec.rb` |
| **パニック箇所** | `builtins::kernel::send` |
| **原因** | `send(:transfer)` 呼び出し時に、エラーメッセージ生成中にBuiltin FuncInfoに対して不正な操作を行いパニック |

### 3. Process.getrlimit — Array#grep 内パニック

| 項目 | 詳細 |
|------|------|
| **spec** | `core/process/getrlimit_spec.rb` |
| **パニック箇所** | `builtins::array::grep` |
| **原因** | `Array#grep` が対応していない引数パターンでパニック (前回は `Array#flatten` の `unimplemented!()` だったが変化) |

### 4. Set#flatten — スタックオーバーフロー

| 項目 | 詳細 |
|------|------|
| **spec** | `core/set/flatten_spec.rb` |
| **原因** | 再帰的 Set 構造 (`s << s; s.flatten`) でサイクル検出なしに無限再帰。CRuby は `ArgumentError` を発生させる |

---

## ハング — タイムアウト (2件)

### 1. IO#close — IO.popen サブプロセスの無応答

| 項目 | 詳細 |
|------|------|
| **spec** | `core/io/close_spec.rb` |
| **原因** | spec後半で `IO.popen(ruby_cmd(...))` を使用。子プロセスが正常に終了せず `IO#close` がブロック |

### 2. Process.exit — Thread + sleep でのデッドロック

| 項目 | 詳細 |
|------|------|
| **spec** | `core/process/exit_spec.rb` |
| **原因** | `Thread.new { exit 42 }` + `sleep` で、`SystemExit` がスレッド間で正しく伝播しないためメインスレッドがブロック |

---

## 影響範囲順の優先度表

| 優先度 | 問題 | 種別 | 影響 | 根本原因 |
|--------|------|------|------|---------|
| **1** | JIT再コンパイル/パッチ時パニック | PANIC×3 | array全体・string/modulo・numeric/step がクラッシュ | JITインラインキャッシュ/型状態不整合 |
| **2** | Fiber#transfer send() パニック | PANIC | send(:transfer) でクラッシュ | Builtin FuncInfo のエラーメッセージ生成 |
| **3** | Process.getrlimit Array#grep パニック | PANIC | getrlimit specがクラッシュ | Array#grep の未対応パターン |
| **4** | Set#flatten スタックオーバーフロー | PANIC | 再帰Set構造でクラッシュ | サイクル検出なし |
| **5** | IO#close IO.popen ハング | HANG | IO.popen テスト不可 | サブプロセス管理の問題 |
| **6** | Process.exit Thread伝播ハング | HANG | Thread + SystemExit テスト不可 | SystemExit のスレッド間伝播未対応 |

---

## 解消済みの問題

| 問題 | 種別 | 解消方法 |
|------|------|---------|
| Range#to_a SEGV | SEGV | 本ブランチの修正で解消 (JIT生成コードの問題が修正された) |
| Range#count/min/max/minmax ハング | HANG×4 | PR #215 で Range#min/max/count/minmax を実装 |
| String#modulo バイトコード生成assert | PANIC | パニック箇所が変化 (JIT recompile に統合) |
| Array#flatten unimplemented | PANIC | パニック箇所が変化 (Array#grep に統合) |

---

## cargo test 結果

- **568 passed**, 10 failed (master と同一)
- 失敗10件は全て CRuby との出力不一致（`Value::assert_eq` アサート失敗）
- SEGV/ハング/パニックはゼロ

| テスト | 原因 |
|--------|------|
| `array::array_inspect` 他7件 | Hash inspect 形式 `{a: 1}` vs CRuby `{:a=>1}` (`hash.rs:794`) |
| `set::set_inspect` | Set inspect 形式 `Set[]` vs CRuby `#<Set: {}>` (`set.rs:318`) |
| `string::unpack1g` | Encoding inspect 形式 (`string.rs:142`) |
