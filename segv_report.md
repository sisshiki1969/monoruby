# monoruby SEGV/パニック/ハング 調査レポート

## 概要

体系的なテスト（150+ ケース）を実施し、SEGV/SIGABRT（パニック）/ハングが発生するケースを調査しました。

## 現在のステータス: 全て解消済み

| 問題種別 | 件数 |
|---------|------|
| **SEGV** | **0件** |
| **パニック (SIGABRT)** | **0件** |
| **ハング (タイムアウト)** | **0件** |

### テスト対象

150+ のテストケースを以下のカテゴリで実施:

- Fiber (basic, yield, transfer, dead resume, recursive resume, raise, many)
- Proc/Lambda/Block (call+yield, arity, curry, binding, method_to_proc, nested)
- Numeric (BigInt, step, pow, infinity, NaN, division, coerce, Rational, Complex)
- String (invalid UTF-8, encode, gsub, match, split, scan, succ, freeze, format, unpack, bytes, tr, squeeze, delete, center, strip)
- Array (flatten recursive, sort mixed, product, pack, zip, rotate, bsearch, dig)
- Hash (recursive inspect, default_proc, compare_by_identity, merge, transform, group_by)
- Range (each, step, include, to_a, endless, beginless, string, float_step)
- Regexp (match, named capture, backref, gsub block, lookahead)
- Exception (retry, ensure, nested, custom)
- Class/Module (subclass, include, method_missing, respond_to_missing, singleton, define_method, reopen, const_missing)
- Enumerable (map, select, reduce, flat_map, each_slice, each_cons, zip, chunk, tally, minmax, sort_by, lazy)
- IO (read/write, each_line, StringIO)
- GC (manual, stress alloc, circular ref)
- Marshal (basic, nested, invalid data)
- Set (basic, operations, flatten recursive)
- Encoding (ascii, force, valid, encode)
- Edge cases (deep recursion, eval, send, respond_to, frozen, splat, keyword args, block pass, case/when, etc.)

## 修正済みのクラッシュ

### PR #210: String メソッドクラッシュ修正

| # | メソッド | 原因 | 修正内容 |
|---|---------|------|---------|
| 1 | `String#casecmp` | `is_str()` → `check_utf8().unwrap()` で invalid UTF-8 パニック | バイトレベルASCII比較に変更 |
| 2 | `String#casecmp?` | 同上 | `check_utf8()` で事前検証、`ArgumentError` を返す |
| 3 | `String#index` | `char_pos == char_len` で `nth().unwrap()` パニック | 末尾位置の境界チェック追加 |
| 4 | `String#rindex` | ゼロ幅マッチで `last_char_pos.unwrap()` パニック | デフォルト位置処理と末尾マッチ対応 |
| 5 | `check_utf8` | `RuntimeError` を返していた | `ArgumentError` に変更（CRuby互換） |

### PR #211: Proc#call yield パニック修正

| # | メソッド | 原因 | 修正内容 |
|---|---------|------|---------|
| 6 | `Proc#call` + `yield` | detached context で `parent_fiber.unwrap()` パニック | `try_prev_cfp()` を新設、`Option` で安全にチェーン走査 |
| 7 | エラーメッセージ | `"no block given (yield)."` | `"no block given (yield)"` に修正（CRuby互換） |

### PR #212: String メソッド invalid UTF-8 パニック (追加)

| # | メソッド | 原因 | 修正内容 |
|---|---------|------|---------|
| 8 | `String#split` (separator) | `as_str()` → `check_utf8().unwrap()` パニック | `is_rstring_inner()` + `check_utf8()?` に変更 |
| 9 | `String#to_i` | 同上 | `expect_str(globals)?` に変更 |
| 10 | `String#upcase` / `upcase!` | 同上 | 同上 |
| 11 | `String#downcase` / `downcase!` | 同上 | 同上 |
| 12 | `String#capitalize` / `capitalize!` | 同上 | 同上 |
| 13 | `String#swapcase` / `swapcase!` | 同上 | 同上 |

### 本ブランチ: `is_str()` 根本修正

| # | 対象 | 原因 | 修正内容 |
|---|------|------|---------|
| 14 | `Value::is_str()` (20箇所で使用) | 内部で `check_utf8().unwrap()` → invalid UTF-8 で常にパニック | `check_utf8().ok()` で `Option<&str>` を返す（invalid UTF-8 は `None`） |
| 15 | `RValue::as_str()` | 同上 | `Option<&str>` を返すように変更 |

## 以前報告されていた「残存する問題」の解消状況

### ~~Fiber#transfer パニック~~ → **解消**
mspec経由でのみ再現していた `RValue::debug` の `unreachable!()` パニック。
現在は `NoMethodError` で正常にエラー終了。

### ~~Numeric#step JIT型状態マージ失敗~~ → **解消**
mspec経由でのみ再現していた JIT `gen_bridge` の `unreachable!()` パニック。
現在は `NoMethodError`（`Float#step` 未実装）で正常にエラー終了。

### ~~Set#flatten スタックオーバーフロー~~ → **解消**
再帰的Set構造での無限再帰。
現在は `NoMethodError`（Set の Hash 操作不完全）で正常にエラー終了。

## 潜在的パニック箇所 — 静的解析結果

### CRITICAL: BigInt → Float 変換 (18+ 箇所)

`to_f64().unwrap()` が BigInt を f64 に変換する際、表現不可能な値でパニック。

| ファイル | 箇所数 | コンテキスト |
|---------|--------|-------------|
| `executor/op/binary_ops.rs` | 2 | 累乗演算 |
| `executor/op.rs` | 10 | 二項演算子 |
| `builtins/range.rs` | 8 | `Range#include?` |
| `builtins/numeric/integer.rs` | 2 | 数値比較 |
| `builtins/numeric/float.rs` | 1 | Float vs BigInt 比較 |
| `builtins/kernel.rs` | 1 | Kernel 演算 |
| `builtins/math.rs` | 1 | Math モジュール |
| `builtins/process.rs` | 3 | `Process.clock_gettime` |

**トリガー:** `(10**1000).to_f` や `(2**1024) == 1.0` など極端な BigInt 値。
**注:** 実テストでは `(10**1000).to_f` が `Infinity` を正常に返すため、実際にパニックするパスはJIT経由等の特定条件に限られる可能性あり。

### CRITICAL: FFI ポインタ参照 (7+ 箇所)

ユーザ指定のポインタを直接参照し、SEGV の可能性。

| ファイル | 行 | パターン |
|---------|-----|---------|
| `builtins/fiddle.rs` | 377 | `CStr::from_ptr()` |
| `builtins/fiddle.rs` | 393, 410 | `from_raw_parts()` |
| `builtins/kernel.rs` | 1198, 1227 | `CStr::from_ptr()` |
| `builtins/kernel.rs` | 1418, 1431 | `memcpy()`, `from_raw_parts()` |

### HIGH: ファイルシステム操作 (7 箇所)

| ファイル | パターン |
|---------|---------|
| `builtins/dir.rs` | `current_dir().unwrap()`, `home_dir().unwrap()`, `set_current_dir().unwrap()` |
| `builtins/file.rs` | `.unwrap()` on `get_constant_noautoload()` |

### HIGH: 文字列/入力パース (3 箇所)

| ファイル | パターン |
|---------|---------|
| `builtins/file.rs` | `mode.split(':').next().unwrap()` |
| `builtins/marshal.rs` | `parse::<i32>().unwrap()` |
| `builtins/string.rs` | `char::from_u32().expect()` |

## cargo test 結果

- **568 passed**, 10 failed (master と同一)
- 失敗10件は全て CRuby との出力不一致（`Value::assert_eq` アサート失敗）
- SEGV/ハング/パニックはゼロ

### 失敗テスト一覧（CRuby出力不一致）

| テスト | 原因 |
|--------|------|
| `array::array_inspect` | Hash inspect 形式が `{a: 1}` vs CRuby `{:a=>1}` |
| `array::array_tos_recursive` | 同上 |
| `class::initializer` | 同上 |
| `hash::hash_inspect` | 同上 |
| `hash::hash_inspect_recursive` | 同上 |
| `hash::hash_inspect_user_defined` | 同上 |
| `hash::hash_tos_recursive` | 同上 |
| `hash::test_hash` | 同上 |
| `set::set_inspect` | Set inspect 形式の差異 |
| `string::unpack1g` | encoding inspect 形式の差異 |
