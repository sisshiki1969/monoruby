# monoruby SEGV/パニック/ハング 調査レポート

## 概要

ruby/spec core カテゴリおよび体系的なエッジケーステストを実行し、SEGV/SIGABRT（パニック）およびハング（無限ループ）が発生するケースを特定・修正しました。さらにコードベース全体の静的解析で潜在的パニック箇所を網羅的に調査しました。

## 修正済みのクラッシュ (PR #210, #211, および本ブランチ)

### PR #210: String メソッドクラッシュ修正

| # | メソッド | 原因 | 修正内容 |
|---|---------|------|---------|
| 1 | `String#casecmp` | `is_str()` → `as_str()` → `check_utf8().unwrap()` で invalid UTF-8 パニック | バイトレベルASCII比較に変更 |
| 2 | `String#casecmp?` | 同上（`to_lowercase()` 前のUTF-8検証なし） | `check_utf8()` で事前検証、`ArgumentError` を返す |
| 3 | `String#index` | `char_pos == char_len` で `nth().unwrap()` パニック | 末尾位置の境界チェック追加 |
| 4 | `String#rindex` | ゼロ幅マッチで `last_char_pos.unwrap()` パニック | デフォルト位置処理と末尾マッチ対応 |
| 5 | `check_utf8` | `RuntimeError` を返していた | `ArgumentError` に変更（CRuby互換） |

### PR #211: Proc#call yield パニック修正

| # | メソッド | 原因 | 修正内容 |
|---|---------|------|---------|
| 6 | `Proc#call` + `yield` | detached context で `traverse_cfp()` → `prev_cfp()` → `parent_fiber.unwrap()` パニック | `try_prev_cfp()` を新設、`Option` で安全にチェーン走査 |
| 7 | エラーメッセージ | `"no block given (yield)."` (末尾ピリオド) | `"no block given (yield)"` に修正（CRuby互換） |

### 本ブランチ追加修正: String メソッド invalid UTF-8 パニック

| # | メソッド | 原因 | 修正内容 |
|---|---------|------|---------|
| 8 | `String#split` (separator) | `arg0.is_str()` → `as_str()` → `check_utf8().unwrap()` パニック | `is_rstring_inner()` + `check_utf8()?` に変更 |
| 9 | `String#to_i` | `self_.as_str()` パニック | `self_.expect_str(globals)?` に変更 |
| 10 | `String#upcase` / `upcase!` | `self_val.as_str()` パニック | `self_val.expect_str(globals)?` に変更 |
| 11 | `String#downcase` / `downcase!` | 同上 | 同上 |
| 12 | `String#capitalize` / `capitalize!` | 同上 | 同上 |
| 13 | `String#swapcase` / `swapcase!` | 同上 | 同上 |

## 残存する問題（mspec実行コンテキストでのみ再現）

### 1. Fiber#transfer — メソッド未定義エラー時のパニック

**再現:** mspec経由でのみ (`core/fiber/transfer_spec.rb`)
**原因:** `RValue::debug` で `unreachable!()` に到達。`send(:transfer)` のエラーメッセージ生成中に未対応の `FuncInfo` 種別を処理。
**再現コード:** `repro/crash1_fiber.rb` (単体実行ではNoMethodErrorで正常終了)

### 2. Numeric#step — JIT型状態マージ失敗

**再現:** mspec経由でのみ (`core/numeric/step_spec.rb`)
**原因:** JITコンパイラの `gen_bridge` で `unreachable!()` に到達。多様な型パターン（Fixnum/Float混在）で型状態マージが矛盾。
**再現コード:** `repro/crash2_numeric_step.rb` (単体実行ではNoMethodErrorで正常終了)

### 3. Set#flatten — 再帰検出なしでスタックオーバーフロー

**再現:** mspec経由でのみ (`core/set/flatten_spec.rb`)
**原因:** 再帰的Set構造で無限再帰。CRubyはサイクル検出して `ArgumentError` を発生。
**再現コード:** `repro/crash4_set_flatten.rb` (単体実行ではNoMethodErrorで正常終了)

## 潜在的パニック箇所 — 静的解析結果

コードベース全体を静的解析し、`unwrap()` / `expect()` / `unreachable!()` で
パニックする可能性のある箇所をリスクレベル別に分類しました。

### CRITICAL: BigInt → Float 変換 (18+ 箇所)

`to_f64().unwrap()` が BigInt を f64 に変換する際、表現不可能な値でパニック。

| ファイル | 箇所数 | コンテキスト |
|---------|--------|-------------|
| `executor/op/binary_ops.rs` | 2 | 累乗演算 |
| `executor/op.rs` | 10 | 二項演算子 (==, <>, +, -, etc.) |
| `builtins/range.rs` | 8 | `Range#include?` |
| `builtins/numeric/integer.rs` | 2 | 数値比較 |
| `builtins/numeric/float.rs` | 1 | Float vs BigInt 比較 |
| `builtins/kernel.rs` | 1 | Kernel 演算 |
| `builtins/math.rs` | 1 | Math モジュール |
| `builtins/process.rs` | 3 | `Process.clock_gettime` |

**トリガー:** `(10**1000).to_f` や `(2**1024) == 1.0` など極端な BigInt 値。

### CRITICAL: FFI ポインタ参照 (7+ 箇所)

ユーザ指定のポインタを直接参照し、SEGV の可能性。

| ファイル | 行 | パターン |
|---------|-----|---------|
| `builtins/fiddle.rs` | 377 | `CStr::from_ptr()` — NULL/不正ポインタで SEGV |
| `builtins/fiddle.rs` | 393, 410 | `from_raw_parts()` — 不正な長さでバッファオーバーリード |
| `builtins/kernel.rs` | 1198, 1227 | `CStr::from_ptr()` on `dlerror()`/`dlsym()` |
| `builtins/kernel.rs` | 1418, 1431 | `memcpy()`, `from_raw_parts()` — ユーザ制御のアドレス/長さ |

**トリガー:** `Fiddle.___read_string(0)`, `Kernel.___memcpy` に不正引数。

### HIGH: ファイルシステム操作 (7 箇所)

| ファイル | 行 | パターン |
|---------|-----|---------|
| `builtins/dir.rs` | 299, 306, 487 | `current_dir().unwrap()` — cwd削除時にパニック |
| `builtins/dir.rs` | 483 | `home_dir().unwrap()` — ホームディレクトリ不明時 |
| `builtins/dir.rs` | 496, 499 | `set_current_dir().unwrap()` — 権限不足時 |
| `builtins/file.rs` | 15 | `.unwrap()` on `get_constant_noautoload()` |

### HIGH: 文字列/入力パース (3 箇所)

| ファイル | 行 | パターン |
|---------|-----|---------|
| `builtins/file.rs` | 482 | `mode.split(':').next().unwrap()` — 空文字列モード |
| `builtins/marshal.rs` | 592 | `parse::<i32>().unwrap()` — 不正な Marshal データ |
| `builtins/string.rs` | 753, 757 | `char::from_u32().expect()` — `String#succ` 境界値 |

### MEDIUM: `is_str()` メソッドの設計問題

`Value::is_str()` は内部で `RValue::as_str()` → `check_utf8().unwrap()` を呼ぶため、
invalid UTF-8 文字列に対して**常にパニック**します。

現在 `is_str()` は codebase 全体で **20箇所** 使われています。今回修正した箇所以外にも、
ユーザ入力が invalid UTF-8 の場合にパニックする可能性があります。

**推奨:** `is_str()` を `check_utf8()` が失敗した場合に `None` を返すよう変更するか、
新しい `try_str() -> Option<Result<&str>>` メソッドを導入して段階的に移行する。

### LOW-MEDIUM: 型チェック後の unwrap (8 箇所)

| ファイル | 行 | パターン |
|---------|-----|---------|
| `builtins/exception.rs` | 157, 171, 186 | `.is_exception().unwrap()` |
| `builtins/regexp.rs` | 154, 175, 212 | `.is_regex().unwrap()` |
| `builtins/main_object.rs` | 10 | `get_singleton().unwrap()` |
| `builtins/object.rs` | 297 | `cfp.prev().unwrap()` |

### リスクサマリ

| リスク | カテゴリ | 箇所数 | 悪用容易度 |
|--------|---------|--------|-----------|
| **CRITICAL** | BigInt→Float unwrap | 18+ | 高（細工した数値） |
| **CRITICAL** | FFI ポインタ参照 | 7+ | 高（Fiddle/dlsym/memcpy） |
| **HIGH** | ファイルシステム unwrap | 7 | 中（環境依存） |
| **HIGH** | 文字列パース unwrap | 3 | 高（ユーザ入力） |
| **MEDIUM** | `is_str()` 設計問題 | 20 | 高（invalid UTF-8） |
| **LOW-MEDIUM** | 型チェック後 unwrap | 8 | 低（ロジックバグ） |

## テスト結果サマリ

### 体系的テスト (200+ テストケース)

- **SEGV:** 0件
- **パニック (SIGABRT):** 0件
- **ハング (タイムアウト):** 0件

テスト対象: String, Array, Hash, Integer, Float, Numeric, Proc, Lambda, Fiber,
Regexp, Range, Exception, Comparable, Enumerable, Symbol, Struct, IO, Encoding,
Method, Kernel, Class/Module, GC — 各クラスの通常操作およびエッジケース
（invalid UTF-8, 再帰構造, 境界値, detached context 等）

### cargo test

- 568 passed, 10 failed (既存の失敗 — master と同一)
