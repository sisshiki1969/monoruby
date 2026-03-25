# monoruby SEGV/パニック/ハング 調査レポート

## 概要

ruby/spec core カテゴリ全58分野を mspec で実行し、SEGV/パニック(SIGABRT)/ハング(タイムアウト)が発生するケースを特定しました。

## ruby/spec core 全体結果

| 指標 | 件数 |
|------|------|
| カテゴリ総数 | 58 |
| 正常完了カテゴリ | 49 |
| **SEGV** | **1件** (Range#to_a) |
| **パニック (SIGABRT)** | **7件** (6 spec + 1 category-level) |
| **ハング (タイムアウト)** | **6件** |

---

## SEGV (1件)

### 1. Range#to_a — JIT生成コードのSEGV

| 項目 | 詳細 |
|------|------|
| **spec** | `core/range/to_a_spec.rb` |
| **再現** | release ビルド + mspec経由のみ。debug ビルドでは再現せず |
| **直接実行** | `monoruby -e 'p (-5..5).to_a'` → 正常動作 |
| **コード箇所** | JIT生成コード（ネイティブx86-64命令列内） |
| **原因推定** | mspec インフラ読み込みでJITが多くのメソッドをコンパイルした後、Range#to_a 実行時にJIT生成コードが不正メモリアクセス。release最適化でのみ顕在化するため、JITコード生成またはインラインキャッシュの問題 |
| **repro** | `repro/segv_range_to_a.sh` |

---

## パニック — SIGABRT (7件)

影響範囲の大きい順に並べています。

### 1. JIT再コンパイル時のインラインキャッシュ未初期化 (影響: array カテゴリ全体)

| 項目 | 詳細 |
|------|------|
| **spec** | `core/array` (全体実行時。個別specでは再現せず) |
| **コード箇所** | `globals/store/iseq.rs:431` — `get_cache_map()` の `.unwrap()` |
| **原因** | `jit_entry.get(&self_class)` が `None` を返す。JITがメソッドを再コンパイルする際、対象クラスのインラインキャッシュマップが未初期化のケースがある。多数のspecを連続実行してJITキャッシュが蓄積した状態でのみ発生 |
| **呼出しパス** | `jit_recompile_method_with_recovery` → `update_inline_cache` → `get_cache_map` → `unwrap()` → PANIC |
| **repro** | `repro/panic_array_jit_recompile.sh` |

### 2. String#% (modulo) — バイトコード生成のassert失敗

| 項目 | 詳細 |
|------|------|
| **spec** | `core/string/modulo_spec.rb` |
| **コード箇所** | `bytecodegen/expression.rs:451` — `assert_eq!(Some(lvar), self.outer_block_param_name(outer))` |
| **原因** | String#% specで使われるブロックパラメータのパターンが、バイトコード生成器の想定と一致しない。外側ブロックの引数名の一致確認が失敗 |
| **repro** | `repro/panic_string_modulo.sh` |

### 3. Numeric#step — JIT型状態マージの unreachable

| 項目 | 詳細 |
|------|------|
| **spec** | `core/numeric/step_spec.rb` |
| **コード箇所** | `codegen/jitgen/state/slot.rs:1333` — `unreachable!()` in `gen_bridge` |
| **原因** | Fixnum/Float 混在の step 呼び出しで、JITの抽象型状態マージが対応していないLinkMode組み合わせに到達。型状態のブリッジ生成で `(l, r)` のcatch-allアームに落ちる |
| **repro** | `repro/panic_numeric_step.sh` |

### 4. Fiber#transfer — FuncInfo の as_iseq unreachable

| 項目 | 詳細 |
|------|------|
| **spec** | `core/fiber/transfer_spec.rb` |
| **コード箇所** | `globals/store/function.rs:963` — `as_iseq()` の `unreachable!()` |
| **呼出しパス** | `send(:transfer)` → `method_not_found` → `RValue::to_s` → `RValue::debug` → builtin FuncInfo に対して `as_iseq()` |
| **原因** | エラーメッセージ生成中に、Builtin関数の `FuncInfo` に対して `as_iseq()` を呼び、`ISeq` でない種別で `unreachable!()` に到達 |
| **repro** | `repro/panic_fiber_transfer.sh` |

### 5. Set#flatten — スタックオーバーフロー

| 項目 | 詳細 |
|------|------|
| **spec** | `core/set/flatten_spec.rb` |
| **コード箇所** | Rust スタック (再帰の深さ超過) |
| **原因** | 再帰的 Set 構造 (`s << s; s.flatten`) でサイクル検出なしに無限再帰。CRuby は `ArgumentError` を発生させる |
| **repro** | `repro/panic_set_flatten.sh` |

### 6. Process.getrlimit — Array#flatten の unimplemented

| 項目 | 詳細 |
|------|------|
| **spec** | `core/process/getrlimit_spec.rb` |
| **コード箇所** | `builtins/array.rs:1788` — `unimplemented!()` |
| **原因** | `Array#flatten` が対応していない引数パターンで `unimplemented!()` に到達 |
| **repro** | `repro/panic_process_getrlimit.sh` |

---

## ハング — タイムアウト (6件)

### 1. Range#count, #min, #max, #minmax (4件) — 未実装メソッドでの無限イテレーション

| 項目 | 詳細 |
|------|------|
| **spec** | `core/range/count_spec.rb`, `min_spec.rb`, `max_spec.rb`, `minmax_spec.rb` |
| **原因** | `Range#count`, `#min`, `#max`, `#minmax` が Range クラスに未実装。`Enumerable` のフォールバック実装が呼ばれ、endless range `(1..)` に対して無限にイテレーションする |
| **CRuby動作** | `Range#count` on endless range → `Float::INFINITY`、`#max` on endless range → `RangeError` |
| **repro** | `repro/hang_range_count_min_max.sh` |

### 2. IO#close (1件) — IO.popen サブプロセスの無応答

| 項目 | 詳細 |
|------|------|
| **spec** | `core/io/close_spec.rb` |
| **原因** | spec後半で `IO.popen(ruby_cmd(...))` を使用。mspec の `ruby_cmd` がサブプロセスを起動するが、子プロセスが正常に終了せず `IO#close` がブロック |
| **repro** | `repro/hang_io_close.sh` |

### 3. Process.exit (1件) — Thread + sleep でのデッドロック

| 項目 | 詳細 |
|------|------|
| **spec** | `core/process/exit_spec.rb` |
| **原因** | spec内で `Thread.new { exit 42 }` + `sleep`（メインスレッド）のパターンを使用。`SystemExit` がスレッド間で正しく伝播しないため、メインスレッドの `sleep` が永久にブロック。`Process.exit!` も未実装 |
| **repro** | `repro/hang_process_exit.sh` |

---

## 影響範囲順の優先度表

| 優先度 | 問題 | 種別 | 影響 | コード箇所 | 根本原因 |
|--------|------|------|------|-----------|---------|
| **1** | JIT再コンパイル時キャッシュ未初期化 | PANIC | array全体がクラッシュ。多数のメソッド実行後に発生するため他カテゴリでも潜在的に影響 | `iseq.rs:431` | `jit_entry` に `self_class` のエントリがない状態で `unwrap()` |
| **2** | Range#to_a SEGV | SEGV | release + mspec でのみ再現。JIT生成コードの品質問題 | JIT生成ネイティブコード | mspec読み込み後のJIT状態でSEGV |
| **3** | JIT型状態マージ failure | PANIC | Numeric#step が使えない | `slot.rs:1333` | Fixnum/Float混在で `gen_bridge` の unreachable |
| **4** | Range#count/min/max/minmax 未実装 | HANG×4 | endless range で無限ループ | `builtins/range.rs` | メソッド未実装→Enumerable fallback→無限イテレーション |
| **5** | String#% バイトコード生成 assert | PANIC | String#% specが使えない | `expression.rs:451` | ブロックパラメータ名の assert 失敗 |
| **6** | Fiber#transfer エラーメッセージ生成 | PANIC | send(:transfer) でクラッシュ | `function.rs:963` | Builtin FuncInfo に対する `as_iseq()` |
| **7** | Set#flatten スタックオーバーフロー | PANIC | 再帰Set構造でクラッシュ | Rustスタック | サイクル検出なし |
| **8** | Process.exit Thread伝播 | HANG | Thread + SystemExit テスト不可 | Thread/sleep実装 | SystemExit のスレッド間伝播未対応 |
| **9** | IO#close IO.popen | HANG | IO.popen テスト不可 | IO.popen/ruby_cmd | サブプロセス管理の問題 |
| **10** | Array#flatten unimplemented | PANIC | 特定引数パターンで落ちる | `array.rs:1788` | `unimplemented!()` catch-all |

---

## 修正済みのクラッシュ (本ブランチ + PR)

### PR #210: String メソッドクラッシュ修正 (5件)
### PR #211: Proc#call yield パニック修正 (2件)
### PR #212: String メソッド invalid UTF-8 パニック (6件)
### 本ブランチ: is_str() 根本修正 (2件)

詳細は git log 参照。

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
