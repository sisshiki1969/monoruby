# ruby/spec core テスト結果レポート

## 実行サマリ

| 指標 | 値 |
|------|-----|
| カテゴリ総数 | 58 |
| 総 spec ファイル数 | 1,386+ |
| 総 examples | 7,072+ |
| 総 expectations | 14,497+ |
| failures | 1,146 |
| errors | 5,644 |
| SEGV | 1 |
| PANIC (SIGABRT) | 7 |
| HANG | 6 |

## カテゴリ別結果

### 正常完了（failures/errors ゼロ）

- comparable (28 examples, 107 expectations)
- conditionvariable (部分)
- data (部分)
- filetest (部分)
- main (部分)
- nil (部分)
- threadgroup (部分)
- tracepoint (部分)

### クリティカル問題（SEGV/PANIC/HANG）

| カテゴリ | 問題 | spec ファイル | 原因 |
|---------|------|-------------|------|
| **range** | SEGV | `to_a_spec.rb` | JIT生成コードの不正メモリアクセス (release only) |
| **range** | HANG×4 | `count_spec.rb`, `min_spec.rb`, `max_spec.rb`, `minmax_spec.rb` | 未実装メソッド→Enumerable fallback→endless range 無限ループ |
| **array** | PANIC | カテゴリ全体実行時 | JIT再コンパイル時 `get_cache_map().unwrap()` 失敗 (`iseq.rs:431`) |
| **string** | PANIC | `modulo_spec.rb` | bytecodegenのassert失敗 (`expression.rs:451`) |
| **fiber** | PANIC | `transfer_spec.rb` | `as_iseq()` unreachable (`function.rs:963`) |
| **numeric** | PANIC | `step_spec.rb` | JIT gen_bridge unreachable (`slot.rs:1333`) |
| **set** | PANIC | `flatten_spec.rb` | スタックオーバーフロー（サイクル検出なし） |
| **process** | PANIC | `getrlimit_spec.rb` | `Array#flatten` unimplemented (`array.rs:1788`) |
| **process** | HANG | `exit_spec.rb` | Thread+SystemExit伝播未対応 |
| **io** | HANG | `close_spec.rb` | IO.popen サブプロセス管理問題 |

### failures/errors が多いカテゴリ（上位15）

| カテゴリ | examples | failures | errors | 合計 | 主な原因 |
|---------|----------|----------|--------|------|---------|
| file | 395 | 54 | 1,036 | 1,090 | File API の多くが未実装 |
| encoding | 515 | 7 | 635 | 642 | Encoding API の大部分が未実装 |
| thread | 272 | 33 | 341 | 374 | Thread 関連機能の多くが未実装 |
| enumerator | 226 | 34 | 348 | 382 | Enumerator メソッドの多くが未実装 |
| enumerable | 547 | 86 | 252 | 338 | lazy, chunk_while, each_with_object 等 |
| kernel | 373 | 110 | 265 | 375 | open, system, `, pp, sprintf 等 |
| integer | 559 | 115 | 168 | 283 | bit操作, digits, pow, ceil/floor 等 |
| hash | 564 | 109 | 167 | 276 | transform, filter, compare_by_identity 等 |
| module | 275 | 63 | 196 | 259 | define_method, const_source_location 等 |
| time | 289 | 42 | 261 | 303 | Time API の多くが未実装 |
| exception | 246 | 57 | 134 | 191 | Exception hierarchy, message format |
| float | 324 | 76 | 105 | 181 | floor/ceil/round precision, infinite? 等 |
| argf | 61 | 0 | 172 | 172 | ARGF 全体が未実装 |
| complex | 181 | 42 | 119 | 161 | Complex 演算の多くが未実装 |
| method | 193 | 38 | 122 | 160 | Method#parameters, #owner, #unbind 等 |

### errors の主要パターン

| パターン | 出現頻度 | 説明 |
|---------|---------|------|
| `NoMethodError: undefined method` | 非常に多 | メソッド未実装 |
| `LoadError: can't load` | 多 | require する Ruby ライブラリが読めない |
| `NameError: uninitialized constant` | 多 | 定数未定義（SystemStackError, IOError等） |
| `SyntaxError` | 少 | パーサが未対応の構文 |
| `TypeError` | 少 | 型変換の互換性問題 |

## 再現手順

```bash
# 全体実行
cd /home/user
/home/user/mspec/bin/mspec run -t ./monoruby/target/release/monoruby /home/user/spec/core/<category>

# 個別spec実行
/home/user/mspec/bin/mspec run -t ./monoruby/target/release/monoruby /home/user/spec/core/<category>/<method>_spec.rb
```

## repro ファイル

`repro/` ディレクトリに各問題の再現スクリプトを格納:

| ファイル | 問題 |
|---------|------|
| `segv_range_to_a.sh` | Range#to_a SEGV |
| `panic_array_jit_recompile.sh` | Array JIT再コンパイルPANIC |
| `panic_string_modulo.sh` | String#% PANIC |
| `panic_fiber_transfer.sh` | Fiber#transfer PANIC |
| `panic_numeric_step.sh` | Numeric#step PANIC |
| `panic_set_flatten.sh` | Set#flatten スタックオーバーフロー |
| `panic_process_getrlimit.sh` | Process.getrlimit PANIC |
| `hang_io_close.sh` | IO#close HANG |
| `hang_process_exit.sh` | Process.exit HANG |
| `hang_range_count_min_max.sh` | Range#count/min/max/minmax HANG |
