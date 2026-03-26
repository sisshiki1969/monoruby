# ruby/spec core テスト結果レポート

## 実行サマリ

| 指標 | 値 |
|------|-----|
| カテゴリ総数 | 58 |
| 正常完了 | 51 |
| タイムアウト | 5 |
| クラッシュ (SIGABRT) | 2 |
| 総 examples | 11,054 |
| failures | 1,756 |
| errors | 7,129 |
| **合計不良** | **8,885** |

---

## タイムアウト / クラッシュ

| カテゴリ | 状態 | 原因 |
|---------|------|------|
| io | TIMEOUT | 大量specのタイムアウト |
| kernel | CRASH | monoruby/src/globals/store/function.rs |
| mutex | TIMEOUT | Thread未実装 |
| process | TIMEOUT | ruby_exe サブプロセス |
| queue | TIMEOUT | Thread未実装 |
| string | CRASH | monoruby/src/bytecodegen/expression.rs |
| thread | TIMEOUT | Thread未実装 |

---

## カテゴリ別結果 (不良数順)

| カテゴリ | examples | failures | errors | 合計 |
|---------|----------|----------|--------|------|
| array | 3052 | 427 | 1698 | 2125 |
| file | 401 | 60 | 1062 | 1122 |
| encoding | 545 | 50 | 542 | 592 |
| enumerator | 226 | 32 | 346 | 378 |
| enumerable | 547 | 88 | 250 | 338 |
| time | 291 | 53 | 244 | 297 |
| range | 435 | 70 | 212 | 282 |
| module | 275 | 63 | 196 | 259 |
| integer | 576 | 112 | 136 | 248 |
| hash | 564 | 102 | 133 | 235 |
| numeric | 273 | 86 | 146 | 232 |
| exception | 246 | 61 | 129 | 190 |
| filetest | 13 | 1 | 172 | 173 |
| complex | 181 | 42 | 119 | 161 |
| argf | 77 | 0 | 154 | 154 |
| float | 328 | 79 | 74 | 153 |
| method | 200 | 41 | 104 | 145 |
| dir | 14 | 3 | 135 | 138 |
| env | 239 | 81 | 51 | 132 |
| sizedqueue | 129 | 0 | 129 | 129 |
| struct | 173 | 31 | 95 | 126 |
| proc | 202 | 45 | 78 | 123 |
| fiber | 140 | 13 | 109 | 122 |
| objectspace | 111 | 5 | 109 | 114 |
| matchdata | 130 | 9 | 102 | 111 |
| data | 93 | 1 | 90 | 91 |
| regexp | 96 | 28 | 53 | 81 |
| tracepoint | 75 | 1 | 75 | 76 |
| rational | 135 | 36 | 35 | 71 |
| symbol | 231 | 15 | 46 | 61 |
| random | 87 | 6 | 52 | 58 |
| math | 226 | 12 | 42 | 54 |
| set | 157 | 20 | 32 | 52 |
| signal | 52 | 30 | 20 | 50 |
| unboundmethod | 78 | 25 | 22 | 47 |
| warning | 27 | 10 | 17 | 27 |
| gc | 25 | 3 | 22 | 25 |
| refinement | 24 | 0 | 24 | 24 |
| class | 46 | 11 | 8 | 19 |
| binding | 15 | 1 | 14 | 15 |
| conditionvariable | 11 | 1 | 11 | 12 |
| nil | 26 | 1 | 8 | 9 |
| threadgroup | 8 | 0 | 8 | 8 |
| main | 1 | 0 | 7 | 7 |
| builtin_constants | 17 | 0 | 6 | 6 |
| marshal | 20 | 0 | 3 | 3 |
| systemexit | 6 | 0 | 3 | 3 |
| basicobject | 178 | 1 | 1 | 2 |
| false | 12 | 0 | 2 | 2 |
| true | 12 | 0 | 2 | 2 |
| comparable | 28 | 0 | 1 | 1 |

---

## エラー分類

### 1. NoMethodError (未実装メソッド) — 上位30

| エラー数 | メソッド |
|---------|---------|
| 707 | `mkdir` |
| 194 | `lazy` |
| 140 | `size` |
| 128 | `step` |
| 124 | `[]` |
| 81 | `refine` |
| 51 | `parameters` |
| 41 | `readlines` |
| 35 | `values_at` |
| 35 | `const_source_location` |
| 31 | `curry` |
| 30 | `define` |
| 30 | `deconstruct_keys` |
| 28 | `closed?` |
| 25 | `raise` |
| 24 | `quo` |
| 22 | `[]=` |
| 22 | `cover?` |
| 20 | `transfer` |
| 19 | `fdiv` |
| 19 | `to_a` |
| 19 | `handled_via_method_missing` |
| 19 | `to_r` |
| 18 | `full_message` |
| 17 | `coerce` |
| 17 | `>>` |
| 16 | `permutation` |
| 16 | `reverse_each` |
| 16 | `<<` |
| 15 | `cycle` |

### 2. NameError (未定義定数) — 上位15

| エラー数 | 定数 |
|---------|------|
| 171 | `Converter` |
| 129 | `SizedQueue` |
| 95 | `ObjectSpace` |
| 75 | `TracePoint` |
| 67 | `ARGF` |
| 49 | `Measure` |
| 30 | `US_ASCII` |
| 30 | `Product` |
| 24 | `FNM_PATHNAME` |
| 17 | `Chain` |
| 17 | `FloatDomainError` |
| 14 | `Lazy` |
| 11 | `ConditionVariable` |
| 11 | `SHIFT_JIS` |
| 10 | `Profiler` |

### 3. TypeError — 上位15

| エラー数 | メッセージ |
|---------|-----------|
| 592 | no implicit conversion of NilClass into String |
| 81 | no implicit conversion of MockObject into Integer |
| 51 | no implicit conversion of MockObject into String |
| 38 | no implicit conversion of String into Integer |
| 26 | no implicit conversion of NilClass into Integer |
| 22 | can't convert MathSpecs::Float into Float |
| 20 | no implicit conversion of Float into Integer |
| 19 | can't convert NumericMockObject into Float |
| 17 | no implicit conversion of Symbol into Integer |
| 17 | no implicit conversion of Rational into Integer |
| 8 | no implicit conversion of Object into Float |
| 7 | no implicit conversion of Rational into Float |
| 6 | no implicit conversion of MockObject into Array |
| 6 | no implicit conversion of Integer into String |
| 6 | no implicit conversion of Range into Integer |

### 4. ArgumentError — 上位15

| エラー数 | メッセージ |
|---------|-----------|
| 190 | wrong number of arguments (given 1, expected 0..0) |
| 96 | Currently, the template character is not supported. |
| 76 | Unknown encoding name - US-ASCII |
| 72 | Unknown encoding name - ISO-8859-1 |
| 72 | Unknown encoding name - UTF-16BE |
| 72 | Unknown encoding name - ISO-2022-JP |
| 47 | wrong number of arguments (given 2, expected 1..1) |
| 38 | wrong number of arguments (given 2, expected 0..1) |
| 33 | String#pack/unpack Unknown template: a |
| 29 | wrong number of arguments (given 7, expected 0..0) |
| 25 | String#pack/unpack Unknown template: M |
| 17 | unknown encoding name - locale |
| 16 | wrong number of arguments (given 3, expected 1..1) |
| 16 | Bad type for index. |
| 13 | wrong number of arguments (given 0, expected 1..1) |

### 5. RuntimeError / NotImplementedError — 上位10

| エラー数 | メッセージ |
|---------|-----------|
| 59 | not supported |
| 46 | StackOverflow |
| 18 | Currently, calling with block is not supported. |
| 17 | not yet implemented: block handler unknown handler 000001090 |
| 8 | Range argument is not supported in Random#rand |
| 5 | the argument is not supported in Random#rand |
| 5 | not yet implemented: block handler unknown handler 00007fd88 |
| 4 | RuntimeError> was returned) |
| 3 | not yet implemented: block handler unknown handler 000001130 |
| 3 | not yet implemented: block handler unknown handler 000001600 |

### 6. SEGV / パニック

- **kernel**: `monoruby/src/globals/store/function.rs:963:18:`
- **kernel**: `library/core/src/panicking.rs:230:5:`
- **numeric**: `monoruby/src/codegen/jitgen/state/slot.rs:1333:17:`
- **string**: `monoruby/src/bytecodegen/expression.rs:451:21:`
- **string**: `library/core/src/panicking.rs:230:5:`
