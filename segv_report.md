# monoruby SEGV/パニック/ハング 調査レポート

## 概要

ruby/spec core カテゴリ全58分野を mspec で実行し、SEGV/パニック(SIGABRT)/ハング(タイムアウト)が発生するケースを特定しました。

## ruby/spec core 全体結果

| 指標 | 件数 | 前回 | 初回 |
|------|------|------|------|
| カテゴリ総数 | 58 | 58 | 58 |
| 正常完了カテゴリ | 55 | 52 | 49 |
| **SEGV** | **0件** | 0件 | 1件 |
| **パニック (SIGABRT)** | **1件** | 5件 | 7件 |
| **ハング (タイムアウト)** | **2件** | 2件 | 6件 |

---

## パニック — SIGABRT (1件)

### Fiber#transfer — send() 内パニック

| 項目 | 詳細 |
|------|------|
| **spec** | `core/fiber/transfer_spec.rb` |
| **パニック箇所** | `builtins::kernel::send` |
| **原因** | `send(:transfer)` 呼び出し時に、エラーメッセージ生成中にBuiltin FuncInfoに対して不正な操作を行いパニック |

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

## 優先度表

| 優先度 | 問題 | 種別 | 根本原因 |
|--------|------|------|---------|
| **1** | Fiber#transfer send() パニック | PANIC | Builtin FuncInfo のエラーメッセージ生成 |
| **2** | IO#close IO.popen ハング | HANG | サブプロセス管理の問題 |
| **3** | Process.exit Thread伝播ハング | HANG | SystemExit のスレッド間伝播未対応 |

---

## 解消済みの問題

| 問題 | 種別 | 解消方法 |
|------|------|---------|
| Range#to_a SEGV | SEGV | JIT生成コードの問題が修正された |
| Range#count/min/max/minmax ハング | HANG×4 | PR #215 で Range#min/max/count/minmax を実装 |
| JIT再コンパイル/パッチ時パニック (array, string/modulo, numeric/step) | PANIC×3 | PR #220 で BOP再定義後のJITエントリ消失に対応 + catch_unwind安全策 |
| Set#flatten スタックオーバーフロー | PANIC | PR #220 でサイクル検出を追加 |
| Array#grep ブロック付き呼び出しパニック | PANIC | PR #220 でブロック対応を実装 |
| Process.getrlimit Array#grep パニック | PANIC | PR #220 で解消 (Array#grep修正の副次効果) |

---

## cargo test 結果

- **578 passed**, 10 failed
- 失敗10件は全て CRuby との出力不一致（`Value::assert_eq` アサート失敗）
- SEGV/ハング/パニックはゼロ

| テスト | 原因 |
|--------|------|
| `array::array_inspect` 他7件 | Hash inspect 形式 `{a: 1}` vs CRuby `{:a=>1}` (`hash.rs:794`) |
| `set::set_inspect` | Set inspect 形式 `Set[]` vs CRuby `#<Set: {}>` (`set.rs:318`) |
| `string::unpack1g` | Encoding inspect 形式 (`string.rs:142`) |
