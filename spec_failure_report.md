# ruby/spec core テスト結果レポート

## 実行サマリ

| 指標 | 今回 | 前回 |
|------|------|------|
| 正常完了 | 56/58 | 51/58 |
| タイムアウト | 0 | 5 |
| クラッシュ | 2 | 2 |
| 総 examples | 13,787 | 11,054 |
| failures | 2,214 | 1,756 |
| errors | 9,458 | 7,129 |
| **合計不良** | **11,672** | **8,885** |

---

## カテゴリ別結果 (不良数順)

| カテゴリ | examples | failures | errors | 合計 |
|---------|----------|----------|--------|------|
| array | 3052 | 429 | 1679 | 2108 |
| io | 1031 | 127 | 1842 | 1969 |
| file | 844 | 114 | 1161 | 1275 |
| string | 1101 | 177 | 613 | 790 |
| enumerator | 403 | 40 | 335 | 375 |
| process | 290 | 33 | 331 | 364 |
| enumerable | 547 | 88 | 249 | 337 |
| thread | 285 | 92 | 205 | 297 |
| time | 291 | 56 | 240 | 296 |
| range | 435 | 73 | 195 | 268 |
| module | 275 | 63 | 196 | 259 |
| integer | 576 | 112 | 136 | 248 |
| hash | 564 | 102 | 134 | 236 |
| numeric | 273 | 118 | 90 | 208 |
| exception | 246 | 73 | 114 | 187 |
| filetest | 73 | 18 | 148 | 166 |
| complex | 181 | 42 | 119 | 161 |
| float | 328 | 79 | 74 | 153 |
| argf | 77 | 0 | 150 | 150 |
| method | 200 | 41 | 104 | 145 |
| dir | 22 | 3 | 137 | 140 |
| sizedqueue | 129 | 0 | 129 | 129 |
| struct | 173 | 31 | 95 | 126 |
| fiber | 140 | 20 | 100 | 120 |
| objectspace | 111 | 5 | 109 | 114 |
| matchdata | 130 | 7 | 100 | 107 |
| proc | 202 | 49 | 53 | 102 |
| data | 93 | 1 | 90 | 91 |
| regexp | 96 | 27 | 52 | 79 |
| tracepoint | 75 | 1 | 75 | 76 |
| rational | 135 | 36 | 35 | 71 |
| random | 87 | 7 | 51 | 58 |
| set | 157 | 20 | 32 | 52 |
| symbol | 231 | 13 | 39 | 52 |
| signal | 52 | 30 | 20 | 50 |
| unboundmethod | 78 | 25 | 22 | 47 |
| queue | 88 | 20 | 22 | 42 |
| mutex | 34 | 3 | 28 | 31 |
| math | 226 | 10 | 18 | 28 |
| warning | 27 | 10 | 17 | 27 |
| gc | 25 | 3 | 22 | 25 |
| refinement | 24 | 0 | 24 | 24 |
| class | 46 | 11 | 8 | 19 |
| binding | 15 | 1 | 14 | 15 |
| conditionvariable | 11 | 1 | 11 | 12 |
| nil | 26 | 1 | 8 | 9 |
| threadgroup | 8 | 1 | 7 | 8 |
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

### 1. NoMethodError — 上位30
| 件数 | メソッド |
|------|---------|
| 1140 | `delete` |
| 193 | `sysopen` |
| 177 | `size` |
| 141 | `entries` |
| 129 | `[]` |
| 97 | `raise` |
| 89 | `chmod` |
| 81 | `refine` |
| 77 | `step` |
| 75 | `readlines` |
| 68 | `foreach` |
| 52 | `parameters` |
| 50 | `symlink` |
| 41 | `===` |
| 40 | `spawn` |
| 40 | `byteindex` |
| 36 | `not` |
| 36 | `wakeup` |
| 35 | `values_at` |
| 35 | `const_source_location` |
| 33 | `close` |
| 30 | `define` |
| 30 | `take` |
| 30 | `zero?` |
| 30 | `deconstruct_keys` |
| 30 | `set_encoding` |
| 29 | `kill` |
| 29 | `open` |
| 28 | `lock` |
| 26 | `backtrace_locations` |

### 2. NameError — 上位15
| 件数 | 定数 |
|------|------|
| 158 | `Buffer` |
| 129 | `SizedQueue` |
| 97 | `IBM437` |
| 95 | `ObjectSpace` |
| 75 | `TracePoint` |
| 73 | `US_ASCII` |
| 67 | `ARGF` |
| 49 | `Measure` |
| 35 | `IBM866` |
| 30 | `Product` |
| 29 | `EUC_JP` |
| 24 | `FNM_PATHNAME` |
| 23 | `ISO_8859_1` |
| 17 | `Chain` |
| 17 | `FloatDomainError` |

### 3. TypeError — 上位15
| 件数 | メッセージ |
|------|-----------|
| 78 | no implicit conversion of MockObject into Integer |
| 65 | no implicit conversion of MockObject into String |
| 59 | no implicit conversion of NilClass into String |
| 56 | no implicit conversion of Integer into String |
| 50 | no implicit conversion of Hash into String |
| 46 | no implicit conversion of String into Integer |
| 30 | no implicit conversion of NilClass into Integer |
| 17 | no implicit conversion of Symbol into Integer |
| 17 | can't convert NumericMockObject into Float |
| 16 | no implicit conversion of Float into Integer |
| 9 | can't convert Integer into Integer |
| 8 | no implicit conversion of Object into Float |
| 7 | no implicit conversion of Rational into Float |
| 6 | no implicit conversion of MockObject into Array |
| 6 | no implicit conversion of Range into Integer |

### 4. ArgumentError — 上位15
| 件数 | メッセージ |
|------|-----------|
| 276 | wrong number of arguments (given 1, expected 0..0) |
| 97 | Currently, the template character is not supported. |
| 74 | Bad type for index. |
| 66 | wrong number of arguments (given 2, expected 1..1) |
| 46 | wrong number of arguments (given 2, expected 0..1) |
| 40 | wrong number of arguments (given 2, expected 0..0) |
| 33 | String#pack/unpack Unknown template: a |
| 29 | wrong number of arguments (given 7, expected 0..0) |
| 25 | String#pack/unpack Unknown template: M |
| 21 | wrong number of arguments (given 3, expected 1..1) |
| 13 | wrong number of arguments (given 0, expected 1..1) |
| 13 | invalid byte sequence in UTF-8 |
| 11 | wrong number of arguments (given 3, expected 2..2) |
| 11 | String#pack/unpack Unknown template: u |
| 10 | String#pack/unpack Unknown template: m |

### 5. RuntimeError — 上位10
| 件数 | メッセージ |
|------|-----------|
| 132 | closed stream |
| 59 | not supported |
| 46 | StackOverflow |
| 25 | Process.daemon is not implemented |
| 20 | Currently, calling with block is not supported. |
| 17 | not yet implemented: block handler unknown handler 0000016c0 |
| 16 | can't access local variable 'f' in outer block |
| 8 | Range argument is not supported in Random#rand |
| 6 | block is not supported. |
| 5 | Bad file descriptor (os error 9) |
