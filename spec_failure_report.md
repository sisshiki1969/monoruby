# ruby/spec core テスト結果レポート

## 実行サマリ

| 指標 | 今回 | 前回 | 変化 |
|------|------|------|------|
| 正常完了 | 58/58 | 56/58 | +2 |
| タイムアウト | 0 | 0 | ±0 |
| クラッシュ | 0 | 2 | -2 |
| 総 examples | 14,902 | 13,787 | +1,115 |
| failures | 2,220 | 2,214 | +6 |
| errors | 8,910 | 9,458 | -548 |
| **合計不良** | **11,130** | **11,672** | **-542** |

---

## エラー分類

### 1. NoMethodError — 上位20

| 件数 | メソッド | 難度 |
|------|---------|------|
| 190 | `size` | 中 |
| 131 | `[]` | 中 |
| 126 | `raise` | 高 |
| 102 | `step` | 中 |
| 82 | `refine` | 高 |
| 68 | `foreach` | 中 |
| 52 | `parameters` | 中 |
| 41 | `===` | 中 |
| 41 | `spawn` | 高 |
| 40 | `byteindex` | 低 |
| 36 | `not` | 中 |
| 36 | `wakeup` | 高 |
| 35 | `values_at` | 低 |
| 35 | `const_source_location` | 中 |
| 33 | `close` | 低 |
| 33 | `readlines` | 低 |
| 30 | `take` | 低 |
| 30 | `zero?` | 低 |
| 30 | `deconstruct_keys` | 中 |
| 30 | `set_encoding` | 中 |

### 2. NameError — 上位10

| 件数 | 定数 | 難度 |
|------|------|------|
| 171 | `Converter` | 中 |
| 159 | `Buffer` | 高 |
| 129 | `SizedQueue` | 高 |
| 103 | `ARGF` | 高 |
| 101 | `IBM437` | 中 |
| 96 | `ObjectSpace` | 高 |
| 90 | `US_ASCII` | 中 |
| 75 | `TracePoint` | 高 |
| 35 | `IBM866` | 中 |
| 32 | `EUC_JP` | 中 |

### 3. TypeError — 上位10

| 件数 | メッセージ | 難度 |
|------|-----------|------|
| 90 | no implicit conversion of Hash into String | 中 |
| 83 | no implicit conversion of MockObject into Integer | 低 |
| 56 | no implicit conversion of Integer into String | 中 |
| 46 | no implicit conversion of String into Integer | 中 |
| 43 | no implicit conversion of MockObject into String | 低 |
| 33 | no implicit conversion of NilClass into String | 中 |
| 30 | no implicit conversion of NilClass into Integer | 中 |
| 17 | can't convert NumericMockObject into Float | 低 |
| 16 | no implicit conversion of Symbol into Integer | 中 |
| 13 | no implicit conversion of Float into Integer | 中 |

### 4. ArgumentError — 上位10

| 件数 | メッセージ | 難度 |
|------|-----------|------|
| 276 | wrong number of arguments (given 1, expected 0..0) | 中 |
| 114 | wrong number of arguments (given 2, expected 1..1) | 中 |
| 77 | Unknown encoding name - US-ASCII | 中 |
| 75 | Bad type for index. | 中 |
| 74 | Unknown encoding name - UTF-16BE | 中 |
| 73 | Unknown encoding name - ISO-2022-JP | 中 |
| 72 | Unknown encoding name - ISO-8859-1 | 中 |
| 47 | wrong number of arguments (given 2, expected 0..1) | 中 |
| 40 | wrong number of arguments (given 2, expected 0..0) | 中 |
| 33 | String#pack/unpack Unknown template: a | 低 |

### 5. RuntimeError — 上位10

| 件数 | メッセージ |
|------|-----------|
| 113 | Dir.rmdir エラー (非空ディレクトリ) |
| 59 | not supported |
| 52 | StackOverflow |
| 25 | Process.daemon is not implemented |
| 20 | Currently, calling with block is not supported. |
| 17 | not yet implemented: block handler unknown handler |
| 16 | can't access local variable in outer block |
| 8 | Range argument is not supported in Random#rand |
| 6 | block is not supported. |
| 6 | new error |

### 6. Failure メソッド別 — 上位20

| 件数 | メソッド | 難度 |
|------|---------|------|
| 196 | `Array#pack` | 低 |
| 89 | `Numeric#step` | 中 |
| 32 | `Encoding.compatible?` | 中 |
| 29 | `Signal.trap` | 中 |
| 16 | `Thread#inspect` | 中 |
| 16 | `Thread#to_s` | 中 |
| 14 | `Exception#full_message` | 中 |
| 14 | `Thread#raise` | 高 |
| 13 | `Module#const_defined?` | 中 |
| 13 | `Module#name` | 中 |
| 13 | `Process.spawn` | 高 |
| 12 | `Range#cover?` | 低 |
| 12 | `String#gsub` | 中 |
| 12 | `Thread.new` | 高 |
| 12 | `Time.local` | 中 |
| 12 | `Time.mktime` | 中 |
| 11 | `Array#join` | 中 |
| 11 | `NoMethodError#message` | 中 |
| 11 | `IO.pipe` | 中 |
| 10 | `Array#initialize` | 中 |
