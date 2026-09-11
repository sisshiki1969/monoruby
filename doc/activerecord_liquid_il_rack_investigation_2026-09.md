# activerecord / liquid-il / rack が CRuby+YJIT より遅い原因（2026-09 調査）

対象: ruby-bench の 3 本。比較対象は CRuby 4.0.2 `--yjit`、monoruby は remote master
`925d1af`（PR #1310 マージ後、Nokogiri stage 5 まで）。計測はすべてこのコンテナ
（4 コア、`setarch -R`、ハーネス `harness-warmup`）で行い、数字はコンテナ内の相対値。
YJIT との比較で「差が大きい」3 本（monoruby / YJIT 比 1.6〜1.8 倍）を、
`--features profile` の統計、callgrind の 1 反復差分（2 反復 − 1 反復）、
層ごとの分解、マイクロベンチで切り分けた。

## 1. 結論（先に要点）

| ベンチ | monoruby | YJIT | 比 | 主因（1 反復の割合は callgrind の命令数） |
|---|---:|---:|---:|---|
| activerecord | 168〜178 ms | 99〜114 ms | 1.6〜1.7 | **Fiddle 経由の sqlite3 が 35 %**（§3.1 で解消済み）（`fiddle_invoke` inclusive。実 DB 処理 `sqlite3VdbeExec` は 2 %）。次点: String キー Hash 10 %、`defined?(@ivar)` の名前表引き 3 %、megamorphic な `is_a?` サイトの deopt 2.1 万回/反復、`Class#===` 連鎖 |
| liquid-il | 280〜287 ms | 156〜159 ms | 1.8 | **`Klass === value`（`case … when Klass` と明示的な `String === v`）の `Module#===` が 16.6 %**（123 万回/反復、1 回 ≈ 310 命令。JIT 済みでも毎回グローバルメソッドキャッシュ + builtin 呼び出し）、**汎用ディスパッチ（`find_method`）11.6 %**（`nil?` の PIC 超過 34 万 deopt/反復、`String#<<` 汎用経路、`to_liquid` / `is_a?`）、`Float#to_s` 7.2 %（31 万回/反復、1 回 ≈ 530 命令） |
| rack | 53〜56 ms | 36 ms | 1.5 | 1 リクエストの**命令数は YJIT 比 +20〜30 %（47.7k vs 36.6k）なのに時間は 1.8 倍** — 命令あたりの実行効率の差が半分以上。層別では Static（`clean_path_info` + `unescape_path`）+2.5 µs vs +1.2 µs、ETag +0.6 vs 0、ContentLength +0.66 vs +0.26、Deflater +0.37 vs 0。個々の操作のマイクロベンチは monoruby が同等〜速いので、合成したときに遅い。プロファイル上は `split` 10 %、正規表現 10 %、Hash 10 %、malloc / free 8 %、GC 5 %（`--no-gc` で 7 %）、`$~` 保存 3.7 %、`Hash#dup` 3.3 %、引数処理 3〜4 %、ブロックからの非ローカル `return`（URLMap）3 % |

activerecord と liquid-il は主因がはっきりしていて、それぞれ 1 つの施策で 3 割前後が
消える見込み（§6）。rack は単独の hot spot がなく、runtime 側の小さな固定費
（String キー Hash、malloc される Hash 本体、`$~` 保存、引数処理）の積み重ねと
実行効率の差で、施策は複数に分かれる。

## 2. ベンチマーク

### 2.1 現在の HEAD vs CRuby 4.0.2+YJIT（headline + graphql / rack / optcarrot）

`run_benchmarks.rb --headline --harness=harness-warmup --rss --interleave`。gem の
インストールに失敗する chunky-png / erubi / erubi-rails / hexapdf / liquid-c / lobsters /
railsbench / rubocop / ruby-lsp / shipit は N/A。

| bench | monoruby (ms) | RSS (MiB) | YJIT (ms) | RSS (MiB) | monoruby/YJIT |
|---|---:|---:|---:|---:|---:|
| activerecord | 167.8 ± 7.1 % | 154.1 | 99.1 ± 10.1 % | 79.2 | 1.69 |
| liquid-compile | 40.4 ± 11.6 % | 72.7 | 36.6 ± 12.1 % | 37.7 | 1.10 |
| liquid-il | 280.1 ± 6.5 % | 98.0 | 155.7 ± 10.4 % | 47.3 | 1.80 |
| liquid-render | 99.8 ± 2.9 % | 83.4 | 65.5 ± 10.0 % | 41.6 | 1.52 |
| mail | 164.9 ± 7.5 % | 108.0 | 106.7 ± 14.2 % | 61.9 | 1.55 |
| psych-load | 1378.9 ± 8.1 % | 71.6 | 1226.6 ± 8.9 % | 30.7 | 1.12 |
| sequel | 62.2 ± 14.0 % | 85.1 | 43.6 ± 13.1 % | 36.4 | 1.43 |
| graphql | 27.6 ± 13.8 % | 71.7 | 22.9 ± 10.1 % | 30.8 | 1.20 |
| rack | 55.7 ± 10.8 % | 71.2 | 35.6 ± 11.4 % | 29.2 | 1.57 |
| optcarrot | 697.5 ± 7.0 % | 98.9 | 1017.8 ± 6.2 % | 53.2 | 0.69 |

このコンテナは前回の調査時（`sequel_mail_liquid_investigation_2026-09.md`）より両者とも
3〜4 割速く、ばらつきも ±7〜14 % と大きい。絶対値の比較は同じ実行の中でだけ行う。

### 2.2 セッション開始時の master `3e296a3` との 3 者比較（同条件、交互実行）

| bench | master `3e296a3` | HEAD | YJIT |
|---|---:|---:|---:|
| activerecord | 185 ms | 177 ms | 114 ms |
| graphql | 29 ms | 27 ms | 27 ms |
| optcarrot | 742 ms | 721 ms | 1064 ms |
| liquid-il | （起動時に panic） | 287 ms | 159 ms |
| liquid-render | （同上） | 106 ms | 70 ms |
| mail | （同上） | 179 ms | 114 ms |
| psych-load | — | 1440 ms | 1309 ms |

`3e296a3` のバイナリは liquid-* / mail で起動時に落ちる（当時の既知の不具合）ので、
これらの before / after は `sequel_mail_liquid_investigation_2026-09.md` の A/B
（同一条件で交互実行）を参照: liquid-il 524 → 355 → 340 ms、liquid-render 152 → 120 →
118 ms、mail 189 → 175 ms、sequel 87 → 82 ms（いずれも当時の絶対値）。

## 3. activerecord

`--features profile`（8 秒、41 反復）と callgrind の 1 反復差分（14.99 億命令）。

### 3.1 Fiddle 経由の sqlite3: 35 %

| 関数（self） | 命令 | 割合 |
|---|---:|---:|
| `ffi_call_int`（libffi） | 104.6 M | 7.0 % |
| `fiddle::fiddle_invoke` | 68.6 M | 4.6 % |
| `classify_argument`（libffi の x86-64 ABI 分類） | 30.4 M | 2.0 % |
| `fiddle::value_to_carg` | 26.1 M | 1.7 % |
| `SmallVec<[libffi::Arg; 8]>::extend` + `try_reserve` | 36.5 M | 2.4 % |
| `ffi_call` + `ffi_call_unix64` | 36.9 M | 2.5 % |
| `pthread_mutex_lock` / `unlock` | 29.5 M | 2.0 % |
| `fiddle::bits_to_value` + `call_raw` + `integer_arg_to_i64` | 36.5 M | 2.4 % |
| `sqlite3VdbeExec`（実際の DB 処理） | 29.2 M | 2.0 % |

`fiddle_invoke` の inclusive は **34.7 %**。`gem/sqlite3` は Fiddle で libsqlite3 を
呼んでいて、`sqlite3_step` の後に `sqlite3_column_type` / `_int64` / `_text` /
`_bytes` … を列ごとに 1 回ずつ FFI 呼び出しする（先行調査 §5.8 のとおり）。
1 呼び出しごとに引数の `Value` → C 値変換、libffi の型分類、戻り値の `Value` 化、
SmallVec の組み立て、mutex が走る。CRuby の sqlite3 は C 拡張で直接呼ぶ。

**対処（済、コミット `50b9442`）**: sqlite3 を Rust 側の builtin にした。
SQLite の amalgamation（3.48.0）を `libsqlite3-src` crate に vendor して `cc` で
静的リンクし（libxml2 / libz と同じ方式。ホストの libsqlite3 に依存しなくなった）、
gem の Ruby 半分（2.7.3）も nokogiri / psych と同様 `gem/sqlite3/` に vendor して
ホストの gem に依存しないようにしたうえで、
`src/builtins/sqlite3.rs` が C 拡張の API をそのまま実装する。`SQLite3::Database` /
`SQLite3::Statement` は `ObjTy::NATIVE` のクラスで、`sqlite3*` / `sqlite3_stmt*` を
payload として所有し、GC 時に閉じる（接続は `sqlite3_close_v2` なので statement が
残っていても順序に関係なく安全）。**`Statement#step` が step と全列読み出しを 1 回の
builtin で行う**ので、1 行あたり `1 + 2n` 回あった C 境界の往復が 1 回になる。
Fiddle と libffi は経路から完全に消えた。

ベンチ（3 ラウンド交互、中央値）:

| | 前（Fiddle） | 後（native） |
|---|---:|---:|
| activerecord | 279 / 285 / 263 ms | 228 / 219 / 201 ms（**−22 %**） |
| sequel | 104 / 103 / 103 ms | 88 / 94 / 91 ms（**−12 %**） |

これで activerecord / YJIT 比は 1.7 倍から 1.3 倍に、残る主因は String キー Hash と
`defined?(@ivar)` に移る（§3.2）。

C 拡張との差分は `tests/sqlite3.rs` で CRuby と突き合わせて潰した（BLOB 列は
`SQLite3::Blob` ではなく BINARY の String、`execute_batch2` は全列を text で返す、
閉じた statement / connection は `SQLite3::Exception`、`step` は DONE 後に nil を
返して再実行しない、bind は nil / Integer / Float / String 以外を
`can't prepare <Class>` で拒否し、i64 に収まらない Integer は double で束縛する）。
接続の open / close はグリーンスレッドを止めないよう native pool のワーカーで走る
（Fiddle 版の `blocking: true` と同じ）。`create_function` / `create_aggregate` は
Fiddle 版と同じく未対応のまま。

### 3.2 その他（合計で 2 割程度）

- **String キー Hash 10 %**: `HashRef::get` 2.0 %、`inline_pos_noobs` 1.9 %、
  `Hashmap::index` 1.5 %、`inline_pos` 1.3 %、`RStringInner::hash` 0.7 %、
  `hashindex` 0.65 %、`insert` 0.6 %、`string_key_eq_c` 0.5 %。属性 Hash
  （`@attributes`、`attribute_aliases`、型キャストの結果）の String キー引き。
- **`defined?(@ivar)` 3.3 %**: `runtime::defined_ivar` が 49.8 万回/反復、1 回 ≈ 100
  命令。`get_ivar(self, name)` が名前 → IvarId の表を毎回引く（callgrind では
  `GvarTable::lookup` に混ざって見える）。ActiveModel は `defined?(@x)` で
  メモ化する箇所が多い。JIT の `@x` 読み出しと同じ IvarId のインラインキャッシュに
  載せれば消える。コスト低。
- **megamorphic な `is_a?` サイト**: `AcceptsMultiparameterTime::InstanceMethods#cast`
  の `value.is_a?(Hash)` が 86.3 万 deopt / 8 秒（2.1 万回/反復）。受信側が
  String / Integer / nil / Time / Date / Float / BigDecimal … と 5 種類以上で PMC
  （4 way）が溢れ、クラス集合ガードを作れず、呼び出しごとに deopt して残りを VM で
  走らせる。liquid-il の `nil?` と同じ形（§4.2）。
- **`Module#===` 連鎖**: `case value when BigDecimal / String / Hash / Symbol /
  Numeric / Integer / Multibyte::Chars / Binary::Data …`（型キャスト）が各 5.6〜11 万回
  / 8 秒、グローバルメソッドキャッシュ経由。liquid-il の主因と同じ（§4.1）。
- `respond_to?` 6.1 万回 → `respond_to_missing?` + `method_missing` の探索が各 6.1 万回、
  `Encoding.find` 11.8 万回（`find_encoding_object` の線形走査）、`Kernel#Array` の
  `to_ary` / `to_a` の否定探索 84 万回（キャッシュ済みなので 1 %）、`time_build` 0.65 %、
  `fill_positional_args` + `handle_invoker_arguments` 1.2 %、malloc / free / alloc 8 %。

## 4. liquid-il

`--features profile`（8 秒、25 反復）と callgrind の 1 反復差分（22.95 億命令）。

### 4.1 `case … when Klass` の `Module#===`: 16.6 %

inclusive: `cmp_teq_values_impl` 376 M（16.4 %、`case` 経由の `cmp_teq_case_values` は
そのうち 20 万回分、残りは `String === v` のような明示的な `===`）。`Module#===`
（`module::teq`）の呼び出しは 2 反復 − 1 反復で **123 万回/反復、1 回 ≈ 310 命令**。内訳は `invoke_method`（builtin へのフレーム構築）→
`find_method` → `GlobalMethodCache::get`（self 2.7 %）→ `module::teq`（2.6 %）→
`expect_class_or_module` → `is_kind_of` の祖先走査。`case value when String … when
Integer … when Hash …`（`output_append`、`to_number`、`compare`）が 8 秒で
`===` `#<Class:String>` 1,003 万回、`#<Class:Integer>` 415 万回、`#<Class:Hash>`
128 万回。

JIT は `BinCmpBr(TEq)` を `generic_binop(cmp_teq_case_values)` に落とし
（`compile/binary_op.rs`）、`cmp_teq_values_impl` は受信側が Class のとき `_ =>`
腕で `invoke_method(TEQ)` する。マイクロベンチ（`teq-micro2.rb`）を `--features profile`
で回すと、JIT 済みの `case x when A` でも実行回数と同じ 210 万回のグローバルメソッド
キャッシュ引きが出るので、この経路に inline は無い。ホットな状態では 1 腕 15 ns
（YJIT 39 ns）で済むが、liquid-il のように受信側・引数のクラスが多く、キャッシュが
冷えている状況では 1 回 ≈ 310 命令（≈ 100 ns）になる。

祖先走査そのものは安い。`case x when A` で x が A の直接のインスタンスなら 13.9 ns、
4 段上のスーパークラスなら 20.0 ns、`when M`（include したモジュール）16.6 ns、miss
（BasicObject まで走査）20.4 ns、x のクラスが 7 種類混在でも 15 ns。`is_kind_of` は
superclass 鎖（include の iclass を含む）を 1 段 ≈ 30 命令で辿るだけで、遅いのは
その手前のディスパッチ（`invoke_method` inclusive 486 命令/回）。`BASIC_OP_DEFS` には Integer / Float / Symbol / nil /
true / false の `===` しかなく、`Module#===` は BOP 扱いではない。

**対処（済、コミット `575ac6a`）**: 受信側が定数の Class / Module で、その `===` が
builtin の `Module#===` に解決されるサイト（`case … when Klass` と `Klass === v`）を、
JIT で `AsmInst::KindOfConst` に落とす。機械語で (1) 値のクラス ID を求め（即値はタグ
判定、ヒープ値は `RValue.class`）、(2) 目的のクラス ID と比較、(3) 外れたらクラス
オブジェクト表（`ClassInfoTable::objects`、`GLOBALS_CLASS_OBJECTS` 経由の
`MonoVec<Option<Module>>`）からクラスオブジェクトを引き、superclass 鎖（include の
iclass を含む）を目的の ID まで辿る。走査はすべて実行時に読むので `include` は
再コンパイル不要。唯一の前提「`Klass.===` が builtin」は `inline_method_cache` に
記録して class version salvage に再検証させる（`def self.===`、`Module#===` の再定義、
refinement で外れる）。x86-64 と aarch64 の両方に実装。

| マイクロベンチ（1 実行） | 前 | 後 | YJIT |
|---|---:|---:|---:|
| `case x when A`（x が A の直接のインスタンス） | 13.9 ns | 6.7 ns | 33.4 ns |
| `case x when A`（4 段上のスーパークラス） | 20.0 | 9.8 | 34.4 |
| `case x when M`（include したモジュール、x 7 クラス） | 52.9 | 10.2 | 43.2 |
| `case String / Integer / Hash / Array`（x 7 クラス混在） | 147 | 19.6 | 46.8 |

ベンチ（2 ラウンド交互、中央値）: liquid-il 279 / 291 → 251 / 248 ms（**−12 %**）、
activerecord 184 / 181 → 176 / 167 ms（−5 %）、liquid-render・mail・sequel はばらつきの範囲内。

**対策**（コスト低〜中）: 受信側が Class / Module で、その `===` の解決結果が
builtin `Module#===` なら `is_kind_of` を直接呼ぶ。`def self.===` を持つクラス
（Rails にも複数ある）を正しく外すには、`(MODULE_CLASS, "===")` を BOP に足すだけでは
足りない（singleton への定義は pair 判定に掛からない）ので、JIT では `when Klass` の
定数を畳んだ時点で `Klass.===` を解決し、builtin なら class version ガード下で
`is_kind_of` のインライン（runtime helper）を出す。VM 側は `cmp_teq_values_impl` に
インラインキャッシュ（lhs のクラス → fid）を持たせて `invoke_method` を飛ばす。
1 回 1,900 → 150 命令程度で、liquid-il の 14 % 前後、activerecord / mail /
liquid-render の `case` 全般に効く。

### 4.2 汎用ディスパッチ: `find_method` inclusive 11.6 %

| サイト | 8 秒 | 内容 |
|---|---:|---|
| `RuntimeHelpers.lookup_prop_fast` `v.nil?` | 855 万 deopt（34 万回/反復） | String / Float / Integer / BOOL + overflow 90 万: PMC 超過で plain deopt |
| `String#<<`（`output_append`） | 348 万回 | 受信側が確定しないサイトの `shl_values` 汎用経路（inclusive 5.4 %） |
| `RuntimeHelpers.t` / `compare` / `lookup` の `to_liquid` / `to_liquid_value` | 78 万 / 43 万 / 23 万 deopt | 3〜4 クラス、`IdentityToLiquid` に収束する same-target |
| `is_a?`（String / Float / Integer） | 135 万回 | `Kernel#is_a?` の汎用呼び出し（inclusive 4.1 %） |

PR #1310 でラチェット化した PIC は「4 クラスまで」なので、5 クラス以上のサイトは
plain deopt のままで、deopt 1 回 + 残りの VM 実行が呼び出しごとに乗る。**対策**:
PMC が overflow したサイトの残余腕を deopt ではなく汎用呼び出し（VM と同じ
インラインキャッシュ付き `find_method` → 呼び出し）にする。`nil?` / `is_a?` /
`to_liquid` のように全クラスが同じ builtin / 同じメソッドに収束する場合は、
クラス集合ガード + 残余の汎用呼び出しで deopt が消える。コスト中。

### 4.3 `Float#to_s`: 7.2 %

`float_to_s` inclusive 164 M、31 万回/反復で **1 回 ≈ 530 命令**。`ruby_float_to_s`
self 145 M のうち Rust の `format_inner`（`format!`）が 84 M（3.7 %）と grisu
`format_shortest_opt` 21 M。価格などの Float を文字列化するテンプレートで効く。
`format!` を通さず、grisu の桁列から直接バッファに書けば半分以下になる。コスト低。

### 4.4 その他

String キー Hash（`string_key_eq_c` 1.5 % + `RStringInner::hash` 1.3 % +
`string_digest_c` 0.9 % + `inline_pos_noobs` 0.6 %）4 %、malloc / free 5 %、
`RStringInner::extend` 0.9 %、`vm_get_constant` 0.6 %。生成コード（`render`）は
partial のラムダをローカルに束縛するのでフレームが捕捉され loop JIT の対象外
（先行調査）。`Proc#call` inclusive 39 %。

## 5. rack

1 リクエスト = `stack.call(env.dup)`。ミドルウェアは MethodOverride → ConditionalGet
→ ETag → Deflater → Sendfile → Static → ContentLength → URLMap（`map "/ok"`）→ app。

### 5.1 命令数は +20〜30 %、時間は 1.8 倍

callgrind で 1 リクエストあたりの命令数（`rack-layers-cg.rb`、層を 1 つずつ足して
各 3,000 リクエスト × 2 ラウンド、2 ラウンド目）:

| 層 | monoruby 命令/req | 累積差分 | YJIT 命令/req | 累積差分 | monoruby ns/req（native） | YJIT ns/req |
|---|---:|---:|---:|---:|---:|---:|
| app only | 3.9 k | | 9.6 k | | 601 | 653 |
| + URLMap | 14.5 k | +10.6 k | 16.5 k | +6.9 k | 1,302 | 1,502 |
| + ContentLength | 21.2 k | +6.7 k | 20.3 k | +3.8 k | 1,966 | 1,765 |
| + Static | 39.7 k | +18.5 k | 32.2 k | +11.9 k | 4,472 | 2,946 |
| + Sendfile | 34.0 k | | 32.4 k | | 4,672 | 3,088 |
| + Deflater | 36.7 k | | 34.3 k | | 5,044 | 3,047 |
| + ETag | 39.3 k | | 34.1 k | | 5,635 | 2,987 |
| + ConditionalGet | 46.7 k | | 36.2 k | | 5,671 | 3,008 |
| + MethodOverride（全部） | 47.7 k | | 36.6 k | | 5,664 | 3,036 |

（命令数はラウンド間で ±5 k のばらつきがある。ハーネスの 10,000 リクエストの反復では
monoruby 42.6 k / req。）

全部入りで monoruby 47.7 k 命令 / 5.66 µs、YJIT 36.6 k / 3.04 µs。命令数の差は
1.3 倍なのに時間は 1.86 倍で、**命令あたりの実行効率（IPC）が YJIT の 7 割**。
GC は `--no-gc` で 7 % しか変わらないので主因ではない。perf が使えない環境なので
確定はできないが、候補は (1) Hash 本体が必ず malloc される（rubymap の entries +
hashbrown の table = 2 回。String キーはインライン表現の対象外）ことによる
malloc / free の多さと 2 段の間接参照、(2) `LocalKey::with`（TLS）経由のアロケータ
アクセス、(3) runtime helper への呼び出しの多さ（下表）。

### 5.2 どこで時間を使っているか（callgrind、1 反復 = 10,000 リクエスト、4.26 億命令）

| 項目（inclusive） | 割合 | リクエストあたり |
|---|---:|---:|
| `String#split`（`clean_path_info` の `split PATH_SEPS`） | 10.1 % | 4.3 k 命令 |
| 正規表現（`captures_from_pos_with` 10.1 %、`rmatch` 6.0 %、`onig_search` 5.5 %） | ≈ 10 % | 4.3 k |
| `save_capture_special_variables`（`$~` の保存） | 3.7 % | 1.6 k |
| `gsub`（`unescape_path` のブロック付き `gsub`、マッチなし） | 3.5 % | 1.5 k |
| GC（`execute_gc`） | 5.3 % | 2.3 k |
| malloc / free / `RValue` alloc / free（self 合計） | ≈ 8 % | 3.4 k |
| Hash（`Hashmap::index` 4.1 %、`inline_pos` 3.5 %、`HashRef::get` 3.2 %） | ≈ 10 % | 4.3 k |
| `Hash#clone`（`env.dup`、`clone_body`） | 3.3 % | 1.4 k |
| `vm_handle_arguments` + `fill_positional_args` + `set_callee_frame_arguments` | ≈ 4 % | 1.7 k |
| `concatenate_string` / `Array#join` / `append_piece` | ≈ 6 % | 2.6 k |
| `set_index` / `get_index`（受信側が確定しないサイトの `[]` / `[]=`） | 5.8 % | 2.5 k |
| `handle_error` + `chain_deopt_into`（URLMap の `@mapping.each { … return … }` の非ローカル return） | 3.0 % | 1.3 k |
| `respond_to?` | 0.8 % | 0.3 k |

profile の統計では warm 状態の deopt は無く、グローバルメソッドキャッシュは
`to_ary` / `to_path` / `respond_to_missing?`（`respond_to?` の探索）が各 1〜3 回 /
リクエスト。

### 5.3 個々の操作は遅くない

`rack-micro2.rb`（1 操作 ns、monoruby / YJIT）: `Rack::Utils.clean_path_info('/ok')`
960 / 1,155、`unescape_path` 722 / 652、`'/ok'.split(/[\/\\]/)` 444 / 732、
ブロック付き `gsub`（マッチなし） 125 / 217、`['ok'].join('/')` 256 / 255、
`env.dup`（27 String キー） 365 / 447、`headers['content-type']` 71 / 101、
`body.respond_to?(:to_ary)` 3 / 40、`body.sum(&:bytesize).to_s` 161 / 172、
`/\bno-transform\b/.match?('')` 105 / 85、`%(W/"#{d}")` 95 / 135、多重代入 22 / 72、
URLMap 風の `each { return }` 536 / 516、`Rack::Request.new(env).path_info` 101 / 153。
遅いのは `'ok'.dup.prepend('/')` 336 / 212 と `Digest::SHA256`（2,367 / 1,335、
ただし ETag の `digest_body` は `last-modified` があるので走らない）だけ。

つまり Static 層の +2.5 µs（YJIT +1.2）は、`clean_path_info` + `unescape_path` +
`can_serve` を単独で回した合計（≈ 1.75 µs、YJIT ≈ 1.95 µs）より大きい。合成したときに
だけ遅くなる分が、§5.1 の実行効率の差に相当する。

### 5.4 rack への施策

1. **String キーの小さな Hash をインライン表現に**（CRuby の ar_table 相当、8 エントリ
   以下をハッシュ値付き線形探索、本体を malloc しない）。`headers`、`response`、
   `Rack::Request` 内部の小 Hash が対象。malloc / free と 2 段参照が減る。コスト中。
2. **`$~` の保存を必要なときだけ**: `split` / `match?` / ブロックなし `gsub` は
   `$~` を設定しない。`save_capture_special_variables` 3.7 % は `=~` /
   `match` / ブロック付き `gsub` が呼び出し元フレームに `$~` を書く分で、
   `Regexp#match` → 直後に `$~` を読まないサイトが多い。JIT で `$~` の読み出しが
   無いメソッドではフレームに書かない最適化が可能。コスト中。
3. **`[]` / `[]=` の受信側が確定しないサイト**（`set_index` / `get_index` 5.8 %）:
   Hash / Array 向けの builtin 直呼び fast path（先行調査 §5.4）。コスト低。
4. **`defined?(@ivar)` / `respond_to?` のキャッシュ**（§3.2）。コスト低。
5. **非ローカル `return`** を例外機構ではなく、捕捉フレームへの直接アンワインドに。
   URLMap の 1 回 / リクエストで 3 %。コスト中。

## 6. 提案（コスト順）

| 順 | 施策 | 効くベンチ | コスト | 見込み |
|---|---|---|---|---|
| 1 | `Module#===` の機械語インライン化（**済**、§4.1）。残りは VM 側のインラインキャッシュと、受信側が定数でないサイト | liquid-il −12 %、activerecord −5 % | 低〜中 | 1 回 310 → 20〜40 命令 |
| 2 | `defined?(@ivar)` を IvarId インラインキャッシュに | activerecord −3 %、Rails 全般 | 低 | 50 万回/反復 × 100 命令 |
| 3 | `Float#to_s` を `format!` を通さずに書く | liquid-il −4 %、Float を出力する全般 | 低 | 530 → 250 命令 |
| 4 | `respond_to?` の (class, name, version) キャッシュ、`Encoding.find` の表引き | activerecord、mail、rack | 低 | |
| 5 | sqlite3 のネイティブ builtin 化（**済**、§3.1） | activerecord −22 %、sequel −12 % | 中 | Fiddle 35 % → 0 |
| 6 | PMC 超過サイトの残余腕を汎用呼び出しに（deopt しない） | liquid-il −10 % 前後、activerecord の `cast`、liquid-render | 中 | 34 万 deopt/反復 → 0 |
| 7 | String キーの小 Hash のインライン表現（ar_table） | rack、activerecord、erubi | 中 | malloc 2 回/Hash → 0 |
| 8 | `$~` 保存の省略、`[]` / `[]=` の builtin 直呼び、非ローカル return の直接アンワインド | rack | 低〜中 | 各 3〜6 % |

## 7. 計測手順

- profile 統計: `cargo build --release --features profile` を `HOME` を分けて（共有の
  `~/.monoruby/v0.3.0` を壊さないため）ビルドし、`MAX_TIME=8 MIN_ITERS=5 monoruby
  -I harness-warmup benchmark.rb`。
- 1 反復の callgrind 差分: `MIN_ITERS=1` と `MIN_ITERS=2`（`MAX_TIME=1000000`、
  `MAD_TARGET=1000000000`）を `valgrind --tool=callgrind --smc-check=all` で走らせ、
  `callgrind_annotate --threshold=100` の per-function 値を引き算（`cgdiff.py`、
  `--inclusive=yes` 版も同様）。`--dump-before=clock_gettime` 方式は activerecord の
  計測（ActiveSupport::Notifications が clock_gettime を頻繁に呼ぶ）で断片化するので使わない。
- rack の層別: `rack-layers.rb`（ミドルウェアを 1 つずつ足して 20,000 リクエスト × 4
  ラウンドの最良値）、命令数は同じスクリプトを 3,000 × 2 で callgrind
  `--dump-before=<clock_gettime>` にかけ、各ラウンドのダンプの `PROGRAM TOTALS` を
  リクエスト数で割る。CRuby 側は `--dump-before=rb_clock_gettime`。
- マイクロベンチ: `rack-micro.rb` / `rack-micro2.rb`（`while` ループで 20 万回、
  結果は `@k` に足し込む）。
