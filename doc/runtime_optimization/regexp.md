# Regexp の実装と最適化

Regexp は graphql の実行時間の 3 分の 1 を占め（`perf` で 33.9 %）、
StringScanner を介して activerecord / rack のパーサ群にも効いてくる
（[`../yjit_bench_slow_investigation_2026-09.md`](../yjit_bench_slow_investigation_2026-09.md)
§5.6、§8.1）。

この文書は、正規表現エンジンとコンパイル済みパターンのキャッシュ、`$~` と
`MatchData` の置き方、正規表現を使う `String` メソッドがどこで高速経路を
取り、どこで取らないかをまとめ、最後に CRuby との実装差異を並べる。
String 側の表現（共有部分文字列、code range、`regex_view`）は
[string.md](string.md) が前提になる。共通の前提は [README.md](README.md)。

---

## 1. エンジンとコンパイル済みパターン

### 1.1 `onigmo-regex` クレート

エンジンは CRuby と同じ Onigmo で、`onigmo-regex` クレート（git 依存）が
束ねている。`Regex` は生の `re_pattern_buffer` ポインタ、コンパイル時の
パターンバイト列、オプション語、`OnigmoEncoding`、コンパイル時の診断
`Vec<String>` を持ち、`unsafe impl Send + Sync` なので、プロセス全体で 1 つの
`Arc<Regex>` をグリーンスレッド間で共有できる。

構築は `Regex::new_bytes_with_encoding` の 1 本に集約され、`onig_new` を
`OnigSyntaxRuby` で呼び、Onigmo の `onig_syntax_warn` 出力をフックで拾って
`warnings` に溜める。

照合の入口は 3 つ:

| 入口 | Onigmo | Region |
|---|---|---|
| `captures_from_pos` | `onig_search` | **呼び出しごとに新規確保**（`onig_region_new` / `free` が 1 回ずつ） |
| `match_at_with_region` | `onig_match`（アンカー、前方探索無し） | 呼び出し側が持つ再利用 `Region` |
| `search_with_region` | `onig_search` | 同上 |

後ろ 2 つは StringScanner のために追加された（§3.6）。`Captures` は
`Region` と haystack への `&str` を持ち、グループ参照はレジスタ配列を直接
読むのでグループごとの確保は無い。

### 1.2 `RegexpInner`

```
regex: Arc<Regex>            // 共有されるコンパイル済みパターン
source: Arc<[u8]>            // CRuby から見える #source の生バイト列（\u{} 展開前）
encoding: OnigmoEncoding     // エンジンが実際に使うエンコーディング（UTF8 / ASCII のみ）
declared_encoding: Encoding  // Regexp#encoding
fixed_encoding: bool         // Regexp#fixed_encoding?
initialized: bool            // Regexp.allocate の placeholder だけ false
```

重いフィールドは両方 `Arc` なので `clone` は参照カウント 2 回。`PartialEq` は
`Arc::ptr_eq` で短絡する。**`Value` を 1 つも持たない**ことが `ObjTy::REGEXP`
を GC で昇格可能にしている（§2.6）。

### 1.3 プロセス全体の正規表現キャッシュ `REGEX_CACHE`

`static REGEX_CACHE: LazyLock<RwLock<HashMap<(String, u32, OnigmoEncoding),
Arc<Regex>>>>`。キーは **`\u{}` 展開後のエンジン向けパターン文字列、Onigmo
だけのオプションビット、エンジンエンコーディング**。Ruby だけのビット
（`NOENCODING` / `FIXEDENCODING` / `KCODE_*`）はキーを作る前に落とすので
`/x/` と `/x/n` はコンパイル済み `Regex` を共有する
（`ruby_only_option_bits_dont_split_cache` テスト）。

探索は `RegexpInner::with_option_kcode_source` の 1 本に集まっていて、
インタプリタのあらゆる構築経路（`with_option` / `with_option_and_encoding` /
`with_option_kcode` / `from_escaped`）がここを通る。ヒットなら `Arc` を
複製して**コンパイル警告を再キューする**（CRuby は同じパターンを
コンパイルするたびに警告し直す）。ミスならコンパイルして挿入し、Onigmo の
エラーを CRuby の `"<msg>: /<source>/"` に整形する。

コストの注記: 探索は常に**書き込みロック**を取り、ヒットでもキーの
`String` を確保する（`reg_str.clone()`）。マップは**決して evict されない**
ので、`Regexp.new` で無限に異なるソースを作るプログラムは際限なく育つ。

### 1.4 第 2 のキャッシュ（native エンコーディング）

`RegexpInner::native_regex` は `NATIVE_CACHE`（生ソースバイト列 + オプション
+ コーデックがキー）を持ち、被照合文字列が EUC-JP / Windows-31J / ISO-8859-* /
KOI8 / Windows-125x のとき、そのコーデックでパターンを再コンパイルする。
こちらは `read()` の高速経路が先にある。コーデック対応表 `onigmo_encoding_for`
は多バイトの `NamedByte` コーデック（Big5 / GB18030 / EUC-KR）を意図的に
外している — monoruby の文字イテレータが Onigmo の境界と食い違うため。
これが CRuby の `rb_reg_prepare_re` のエンコーディング別再コンパイル
キャッシュに相当する（§5）。

### 1.5 Onigmo に渡す前のソース前処理

- `pre_validate_regex`: Onigmo が黙って受けてしまう入力に CRuby の文言で
  `RegexpError` を出す（末尾の `\`、桁ゼロの `\x`、閉じていない `\p{`）。
- `expand_unicode_braces`: Onigmo の `\u` は 4 桁ちょうどしか受けないので
  `\u{XXXX}` / `\u{XX YY}` を `\uHHHH`（BMP）か生 UTF-8（補助面）に書き換え、
  `\uXYZ` 形のエラーは CRuby の文言で出す。`\u` を含まなければ即座に抜ける。
- `resolve_declared_encoding` 系: CRuby のエンコーディング解決の梯子
  （n → u/e/s → FIXEDENCODING → 非 ASCII 内容 → US-ASCII 非固定）。

### 1.6 `Executor` 無しでのコンパイル時警告

bytecodegen はリテラルを `Executor` の届かないところでコンパイルするので、
`thread_local! PENDING_REGEXP_WARNINGS` に溜めて `queue_regexp_warnings` /
`drain_pending_warnings` で受け渡す。`Executor::flush_compile_warnings` が
`Regexp.new` / `Regexp#initialize` / `eval` / `Binding` / スクリプトコンパイル
後に流す。

### 1.7 `Regexp.new` / `.escape` / `.union` / その他

- `regexp_new`: レシーバが `REGEXP_CLASS` ちょうどなら直接構築（allocate +
  initialize の往復無し）。サブクラスは allocate してから `#initialize` を
  dispatch し、インスタンスを `vm.temp_push` で root する。
- `Regexp.allocate` は空パターンをコンパイルし（必ずキャッシュヒット）、
  `initialized = false` を立てる。`#match` / `#=~` / `#options` は
  `TypeError`、`#==` / `#hash` は動く。
- `Regexp.escape` / `.quote` はバイト単位の一巡 `escape_bytes`
  （`rb_reg_quote` 相当、`Vec::with_capacity(len)`）。「壊れた」文字列も
  raise せずバイト単位でエスケープする。
- `Regexp.union`: 空 → `/(?!)/`、`Regexp` 1 個 → **再コンパイル無しで
  そのまま返す**、`to_regexp` に応える 1 個 → その regexp。それ以外は
  `parts.join("|")` して 1 回コンパイル（キャッシュが効きうる）。
  エンコーディング合成の状態機械 `UnionEnc` / `ArgEnc` がある。
- `Regexp.linear_time?` は `\1..\9` / `\k` / `\g` の純粋な構文走査。
  `Regexp.timeout` / `timeout=` はスレッドローカルに**保存されるだけで
  強制されない**（ReDoS の中断は無い）。

### 1.8 bytecodegen でのリテラル

- パーサ（`prism_backend.rs`）は `NodeKind::RegExp(parts, flags, is_const)` を
  作る。フラグ抽出 `regex_flags_from_closing` は `i m x n u e s` だけを残し、
  **`o` を落とす**（§5）。
- **非補間リテラル**（`is_const`）: `const_regexp` が bytecode 生成時に
  **1 回だけ**コンパイルし、結果の `Value` を frozen にして `emit_literal` に
  渡す。`REGEXP_CLASS.is_always_frozen()` が真なので命令は
  **`FrozenLiteral`**（`Literal` ではない）: VM は共有オブジェクトを
  複製無しでロードし、JIT の `TraceIr::FrozenLiteral` は `def_lit2gp` で
  GP レジスタへの即値ロードになる。**`/re/` を回すループは 1 反復あたり
  確保ゼロ・エンジン仕事ゼロ**で、同じサイトの `/re/.equal?(/re/)` が成り
  立つ。リテラルのコンパイルエラーはロード時の**構文エラー**になる。
- **補間リテラル**: `gen_regexp` が `ConcatRegexp` 命令を出し、オプション語
  は `(?imx)` のソース接頭辞ではなく先頭の Fixnum オペランドで渡す
  （`(?n)` は Onigmo のグループオプションとして無効、かつ `Regexp#source` に
  フラグが見えてはいけない）。実行時の `runtime::concatenate_regexp` は
  まず通常の `"#{}"` 結合（`concatenate_string_inner`、[string.md](string.md)
  §4.2）で補間 **String** を組み、そのバイト列と lossy な UTF-8 ビューを
  `with_option_kcode_source` に渡す。つまり**補間 regexp は評価のたびに
  組み直されるが、補間結果が同じなら Onigmo のコンパイルは `REGEX_CACHE` が
  省く**。VM / JIT の lowering は素の実行時呼び出し。
- 条件式の裸のリテラル（`if /pat/`）はパーサが `regex =~ $_` に脱糖する。
  `/(?<name>…)/ =~ str` の名前付きキャプチャのローカル束縛は、パーサが
  `Regexp.last_match(:name)` の代入列に書き換える。

---

## 2. `$~` と `MatchData`

### 2.1 `LFP_SVAR` コンテナ

フレームスロット `LFP_SVAR`（= 16）の `0` は「未確保」の番兵で、非ゼロなら
**2 要素の `Array` `[$~, $_]`**（`SVAR_BACKREF = 0` / `SVAR_LASTLINE = 1`）。
所有するのはメソッドを導入するフレームだけで、ブロックと **lambda** は
`outer` を辿って LEP に至り、`def` / クラス本体 / トップレベル /
`define_method` 本体で止まる（`Lfp::mfp`、
[`../stack_frame.md`](../stack_frame.md)）。

native な builtin フレームは svar スコープを作らない。`Executor::current_mfp`
は**動的な** CFP 連鎖を `is_native()` と `is_svar_transparent()` のフレーム
越しに歩く。`is_svar_transparent` は monoruby 自身の `builtins/*.rb` から
コンパイルされた全 iseq に立つので、`"wawa".to_enum(:scan, /./).map { $& }`
は monoruby の `Enumerable#map` が Ruby フレームでも `scan` の match を見る。

**遅延確保**: `svar_container_of` はスロットがゼロなら `None` を返し、
`svar_container_of_create` だけが `Value::array2(nil, nil)` を確保する。
`clear_capture_special_variables` は nil を格納するためだけにコンテナを
作ったりしない。すべてのフレーム設定コード（VM / JIT / invoker、両アーキ）
がゼロの番兵を書く。

GC: `Lfp::mark_contents` は非ゼロのときだけ mark。`c.as_array()[SVAR_BACKREF]
= val` の格納は `Array` の `DerefMut` が `write_barrier_bulk()` を走らせる
のでバリア済み。

実行コンテキストごとの上書き `root_lep` / `root_svar`: スレッド／ファイバー
本体は spawner の LEP を持つブロックなので、そこを通して解決すると `$~` が
双方向に漏れる。CRuby の `ec->root_lep` を鏡写しにしていて、
`FiberInner::svar_isolated` で opt-out できる（Enumerator の内部は意図的に
**共有**のまま）。

### 2.2 フックされたグローバル

`$~`（get / set、nil か MatchData のみ）、`$&`、`$'`、`` $` ``、`$+`、
`$1..$N`（`IdentId` から桁を読む）、`$_` は全部、格納された 1 つの
`MatchData` から**導出する読み手**で、別の格納は無い。`$~` が設定される
までは `None`（`defined?` が nil）を返し、`$~` 自身は常に `Some`。
`stdlib/stringio.rb` が `rb_lastline_set` を真似るための
`__set_lastline_in_caller` もある。

### 2.3 `MatchDataInner` — RValue のセルに収める

```
regex:    Option<Regexp>        // 8
heystack: Value                 // 8   (String のスナップショット)
matches:  SmallVec<[Span; 2]>   // 24
                                 = 40 ≤ RValue の 48 バイト
```

`Span = (u32, u32)` で `NO_MATCH = (u32::MAX, u32::MAX)` が不参加グループ。
`usize` 対の半分の幅で、inline 容量 2 は「全体一致のみ」と「キャプチャ 1 つ」
を位置の確保無しで賄う。被照合文字列は 4 GiB 未満に制限（構築時に assert）。

**コピー無しの haystack スナップショット**: `MatchDataInner::snapshot` は
照合側が被照合 `Value` を知っていれば、それの**共有 copy-on-write 部分文字列**
（`string_substring`）として保持し、知らなければ所有コピーにする。
`from_captures_snap` が `$~` のホットな構築子で、`Captures::iter` から
中間の位置 `Vec` 無しで span を組む。アクセサ `at_value` / `pre_match_value` /
`post_match_value` / `captures_values` も CoW 部分文字列を返し、
`string_value` は**毎回同じ** frozen スナップショットを返す
（`md.string.equal?(md.string)`）。`at` は `&[u8]` を返す — `/n` のバイト
クラス一致で文字境界のスライスをすると `extern "C"` 境界で abort するため。

### 2.4 一時の stash と保存経路

- `sp_match_regex`: 進行中の `Regexp`。次の `save_capture_special_variables`
  が名前付きキャプチャ用に付ける。消費されたら消える。
- `sp_match_haystack`: 被照合 String の `Value`。`resolve_haystack` は照合側
  の `&str` が本当にそのバッファを借りているかを**ポインタの包含**で
  検証し、`(subject, byte offset)` を返す。古い stash は単に `None` に
  解決してコピーに落ちるだけで、誤帰属はしない。
- 両方とも `Executor::mark` の GC root（`MatchData` 確保が GC を起こす）。
- `save_capture_special_variables` / `_bytes` が `MatchData` を組み、regexp を
  付け、`SVAR_BACKREF` に格納する。呼び出し側は `String#match` / `=~` /
  `[]` 系 / `split` / `sub` / `gsub` / `scan`、`Regexp#=~` / `===` / `match`。

### 2.5 `MatchData` を**確保しない**場合

- `RegexpInner::match_pred`: 真偽だけの照合で、**`$~` に触れず `MatchData`
  も作らない**。`Regexp#match?`、`String#match?`、`Array#grep` /
  `Enumerable#grep`（grep は `$~` を乱してはいけない、CRuby 準拠）。
- `captures_from_pos_no_save`: 特殊変数の副作用無しの生の照合。`gsub` +
  ブロックの先行探索と `replace_all_block_inner` の走査。
- `strscan_match`: `onig_match` / `onig_search` を再利用 region に、`$~`
  無し（§3.6）。
- `String#match` / `Regexp#match` は**保存したばかりの `$~` を返り値として
  再利用**し、別の haystack ビュー上に 2 個目の `MatchData` を作らない
  （`match_one` / `rmatch`）。`str.match(re).equal?($~)` が CRuby と同じく
  成り立つ。

**避けられていないもの**: `str =~ /x/`（どちらのレシーバ順でも）は必ず
確保する。`Regexp#=~` → `find_one` → `captures` → `captures_from_pos` →
無条件の `save_capture_special_variables`。bytecodegen にも JIT にも
**`$~` の死活解析は無い**ので、JIT されたループ内の `if str =~ /x/` は
1 反復あたり `MatchData` 1 個（+ `onig_region_new` / `free` 1 回）を確保する。
`Regexp#===`（`teq`）も `find_one` を通るので `case s when /x/` はテストごと
に `MatchData` を確保する — CRuby も同じだが、CRuby は busy でない
`MatchData` オブジェクトを再利用する（§5）。

### 2.6 GC での昇格

`RValue::is_promotable` で `ObjTy::REGEXP` は `TIME` / `UMETHOD` とともに
昇格可能（「この 3 つは `Value` を一切持たない」）。remember-on-promote
（`young_child_exists`）からも参照無しの腕で除外される。効果は
[`../yjit_bench_slow_investigation_2026-09.md`](../yjit_bench_slow_investigation_2026-09.md)
§8.6（minor GC ごとの REGEXP の mark が erubi で 291 → 5）。
`ObjTy::MATCHDATA` は昇格**しない**（regexp と haystack の `Value` を持ち、
`_ => false` の腕に落ちる）。

---

## 3. VM / JIT の経路と正規表現を使う String メソッド

### 3.0 無いもの

- **`Regexp` にも正規表現駆動の `String` メソッドにも、インライン生成器・JIT
  intrinsic・LIR ノードは 1 つも無い。** `builtins/regexp.rs` に `add_inline` /
  `inline_gen2!` は無く、`codegen/` にある regexp 形の機械語は
  `ConcatRegexp` の実行時呼び出しだけである。
- `Regexp#===` は基本演算表に**無い**（`===` は Integer / Float / Symbol /
  nil / true / false だけ）。`case/when` に regexp が来ると `Cmp(TEq)` →
  `cmp_teq_case_values` → `cmp_teq_values_impl` に Regexp の腕が無く、汎用の
  `invoke_method(_TEQ)` でフルの dispatch になる。`OptCase`（ジャンプ表）は
  小さな Integer の `when` が 8 個以上のときだけで、regexp には効かない。

### 3.1 `regex_view` の継ぎ目

`RStringInner::regex_view` はエンジン向けに UTF-8 として妥当な `Cow<str>` を
返す: UTF-8 / US-ASCII / ASCII のみの内容なら `check_utf8` 経由の**借用**
ビュー、8 ビット内容を持つバイト指向エンコーディングなら各バイト `b` を
`U+00bb` にした**所有の代理像**（元の 1 バイト ↔ ビューの 1 文字）。述語
`needs_byte_mapping` は**宣言された**エンコーディングで決め、バイト列が
たまたま UTF-8 として妥当かどうかは見ない。逆写像は `from_mapped_utf8`。
全 `String` 正規表現メソッドが `mapped` フラグを持ち回って結果を戻す。
`check_utf8` はキャッシュ済み `CodeRange` が `SevenBit` か `Utf8 + Valid`
なら O(n) の再検証を飛ばす。

### 3.2 `CodeRange` — O(1) の分類キャッシュ

[string.md](string.md) §2 の `cr` が regexp 経路を安くしている当のもの:
`is_ascii_only()` が「文字添字 == バイト添字」の全ショートカットをゲートし、
`char_length()` は SevenBit で O(1)、`Regexp#match` は
`code_range() != SevenBit` で native コーデック経路が要るかを決め、
`String#match` / `#match?` は ASCII のみのレシーバで `chars().count()` を
飛ばす。

### 3.3 バイト ↔ 文字オフセット変換

**文字添字 → バイト添字のキャッシュは無い。** 変換は毎回線形走査
（`byte_to_char_index` は `char_indices().enumerate()`、`char_to_byte_pos` は
`char_indices().nth(cp)`、`Regexp#match` の native 経路は
`iter_char_bytes().take(cp).map(len).sum()`）。ASCII のみのショートカットが
緩和のすべてで、背景と計画は
[`../encoding_char_iteration_design.md`](../encoding_char_iteration_design.md)
（`CharByteIter`、「Onigmo coupling」の制限、多エンコーディングの Onigmo
走査は別件という注記）。

### 3.4 部分文字列の共有と一巡の splice

- `string_substring`: `len > 32` で親がヒープ／共有ならコピー無しの CoW
  共有ビュー。全キャプチャ / pre / post 文字列と `scan` の結果に使う。
- `string_snapshot`: ユーザーコードが生きたレシーバを書き換えても
  `invoke_block` をまたいで `&str` を借りられる **frozen** スナップショット。
  `scan`、`gsub` + ブロック、`gsub` + Hash。
- `check_string_not_modified`: CRuby の `str_mod_check`（長さ基準、
  `RuntimeError: string modified`）。
- `RStringInner::splice_all`: `gsub` / `scan` の全置換を**前方一巡**で適用し、
  N 回の末尾シフト `bytesplice_with` の `O(haystack · matches)` を
  `O(haystack + Σ replacements)` にする（回帰テスト
  `gsub_many_matches_linear`）。
- `bytesplice_with`: 単一一致の `sub` 用。両側が SevenBit / Valid で splice
  端点が UTF-8 境界なら変更後の code range 分類を O(1) に短縮。

### 3.5 メソッドごとの注記

| メソッド | 最適化の要点 |
|---|---|
| `Regexp#=~` | nil 引数は `$~` を消して nil（エンジン呼び出し無し）。1 回の `from_utf8` 検証後 `from_utf8_unchecked` でバイト列を借り、CoW スナップショットのために被照合を stash。**バイト**オフセットを返す |
| `Regexp#===` | nil / Regexp / 非文字列様の引数は照合せず false。Symbol → String、他は `to_str` |
| `Regexp#match?` | `match_pred`。`MatchData` 無し、`$~` 無し、`sp_match_regex` 無し |
| `Regexp#match` | まずエンコーディング検証。非 SevenBit でコーデックのある被照合は **native コーデックのバイト経路**（`.` が宣言エンコーディングに従う）、それ以外は借用 UTF-8 経路で、保存したばかりの `$~` を返す |
| `String#=~` | Regexp 右辺が高速経路で**文字**添字を返す。String 右辺は `TypeError`、他は `to_str` の前に `rhs =~ self` を逆 dispatch |
| `String#match` | 両引数を借用の前に変換（ユーザーの `to_str` がレシーバを変えうる）。文字 → バイト位置は ASCII のみで O(1)、末尾超えは CRuby どおり clamp |
| `String#match?` | `match_pred`。範囲外の位置は clamp せず拒否 |
| `String#[]` / `slice`（Regexp） | 名前付きは `get_group_members` でグループ番号を解決し、最右の参加グループ。`$~` を設定 |
| `String#index` | **String の needle はエンジンに入らない** — バイト／文字境界の部分文字列探索で `$~` を触らず（issue #721）、壊れた UTF-8 も許容。Regexp 経路はミス時に `$~` を消し、末尾のゼロ幅一致を専用に扱う |
| `String#rindex` | 前方走査ループ。最終的な `$~` は返す位置での追加 1 プローブで再確立 |
| `String#sub` / `sub!` | Hash / 置換 String / ブロックの 3 腕が `with_coerced_regexp`（Regexp・String（エスケープ）・`to_str`）に集まる。結果は `apply_template_encoding` で再タグ |
| `String#gsub` / `gsub!` | ブロック形は `replace_all_block` が **frozen スナップショットを取る前に生きたレシーバで最初の一致を先行探索**する: 「一致無し」の一般的なケースがスナップショット + splice でなくコピー 1 回で済み、本走査は 0 でなく最初の一致位置から始まる（`gsub_block_without_a_match_probes_first` テスト）。ブロック結果のエンコーディング互換は文字列化前に検査 |
| `String#scan` | 先に frozen スナップショット。yield する各断片は CoW `string_substring`。CRuby のゼロ幅一致を落とさないよう `captures_iter` でなく手動走査。ループ後 `$~` を**最後の**一致に戻す（終端の不一致で消えるため） |
| `String#split` | 区切り `SepKind::{Awk, Chars, Str, Re}`: **空ソースの Regexp は文字分割、単一スペースのソースはリテラル文字列分割に降格し、どちらもエンジンに入らない**。非 UTF-8 + 空区切りの専用文字経路、`limit == 1` の短絡、CRuby の `split_string` を写した空フィールドの遅延カウンタ |
| `String#start_with?` | Regexp はエンジン（`m.start() == 0` でアンカー）。**String は生の `starts_with` バイト検査** + `enc_char_boundary` で、エンジンも確保も無し |
| `String#end_with?` | Regexp は `TypeError`（CRuby どおり）。String は `ends_with` + 境界検査 |
| `String#include?` | 2 つの `regex_view` に `str::contains` |
| `String#tr` / `tr_s` / `delete` / `count` / `squeeze` | 3 段: (1) ASCII のみのレシーバ **かつ** ASCII のみの指定なら `[u64; 2]` の 128 ビットマップ（削除／否定）か平坦な `[u8; 128]` 変換表で全部バイト単位、(2) 非 ASCII レシーバに ASCII 指定は `tr_translate_bytes`、(3) 汎用の `chars()` 走査。`Charset` は `ascii_bitmap: [u64; 2]` + `non_ascii: BTreeSet<char>` + `negated` で、`contains_ascii_byte` はシフト / AND / XOR 各 1 回 |

### 3.6 StringScanner — 唯一の手調整されたエンジン経路

Ruby 側は `stdlib/strscan.rb`。`_anchored` が `\A(?:…)` で包んだ regexp を
作るのは**fallback 経路だけ**で、サイズ上限 512 の 3 つの Hash
（`ANCHORED_RE`（identity キー）/ `ANCHORED_STR` / `PLAIN_STR`）にキャッシュ
する。

Rust 側の `String#__strscan_match`:

- `thread_local! STRSCAN_REGION: RefCell<onigmo_regex::Region>` — **スレッド
  全体で 1 つの再利用 region** なので、グループ無しのヒットは確保ゼロ。
- String パターンはリテラルバイト列: アンカー時 `starts_with`、それ以外は
  `windows().position()` — Onigmo に入らない。
- Regexp パターンはエンジンビューがバッファそのもの（`is_ascii_only() ||
  (Utf8 && valid)`）のときだけバイトバッファ上で**その場**照合し、それ以外は
  `false` を返して Ruby 側がコピーに対する `String#match` に落ちる。
- アンカーは suffix に対する `onig_match`（`strscan_match`）で、高速経路では
  **`\A(?:…)` の包み regexp を決して作らない**。
- 返り値は確保を意識している: 走査位置から始まるグループ無しの全体一致は
  素の Fixnum、それ以外は平坦な `[b0, e0, b1, e1, …]` Array。
- `$~` は触らない（CRuby の C 実装 strscan もレジスタを自分で持つ）。

計測と経緯は
[`../yjit_bench_slow_investigation_2026-09.md`](../yjit_bench_slow_investigation_2026-09.md)
§5.6、§8.1。

---

## 4. エンジンとエンコーディングの相互作用

- 主経路でエンジンに渡るのは **UTF-8 か ASCII だけ**。より豊かな宣言
  エンコーディングは `declared_encoding` に残り、`#encoding` /
  `#fixed_encoding?` / `Regexp.union` の互換判定に使われ、走査自体は
  UTF-8 / 代理ビュー上で走る。
- 例外は非 SevenBit でコーデックのある被照合に対する `Regexp#match`:
  `onigmo_encoding_for` → `native_regex`（`NATIVE_CACHE`）→
  `captures_bytes_from_pos` → `MatchDataInner::from_captures_bytes`。Onigmo が
  多エンコーディング照合をするのはここだけ。
- 互換ゲートは CRuby の `rb_reg_prepare_enc` を写す: `check_match_encoding`
  の 3 規則、`regexp_encoding_mismatch`、`check_subject_match_encoding`、
  `warn_binary_regexp_match`（「historical binary regexp match /.../n
  against X string」）、`Regexp#match` の `ArgumentError: invalid byte
  sequence`、String 側の `check_pattern_encoding_compat` 等。
- 代理写像は多バイトのバイト指向エンコーディングに対する CRuby のエンコー
  ディング別文字走査の**バイト単位の近似**である。
- 結果のエンコーディング: `apply_template_encoding` が `sub` / `gsub` の出力を
  レシーバのエンコーディングに再タグし（置換機構は既定で UTF-8 を組む）、
  `Regexp#source` は `fixed_encoding?` に従い、補間 regexp は修飾子で固定
  されない限り補間内容から取る。

---

## 5. CRuby との実装差異

| 項目 | CRuby | monoruby |
|---|---|---|
| **`/o`（once）** | 補間リテラルを 1 回だけコンパイルして再利用 | `o` を**パーサが落とす**ので `/#{x}/o` は評価ごとに組み直す。`REGEX_CACHE` のヒットで再構築はハッシュ探索で済むが、`Regexp` オブジェクトの同一性は違う。ruby/spec の regexp 関連で唯一の失敗タグ（`spec/tags/language/regexp/modifiers_tags.txt` の "supports /o (once)"） |
| 非補間リテラル | 1 回コンパイル、frozen、共有 | 同じだがさらに早く、*bytecodegen* 時にコンパイルして `FrozenLiteral` で出す。コンパイルエラーは実行時 `RegexpError` でなく**ロード時の構文エラー** |
| エンコーディング別の再コンパイル | `rb_reg_prepare_re` が regexp オブジェクト上にキャッシュ | **プロセス全体**の 2 つのキャッシュ（`REGEX_CACHE` / `NATIVE_CACHE`）、どちらも `RwLock<HashMap>` で**上限無し**。`Arc<Regex>` でスレッド間共有 |
| エンジンが見るエンコーディング | 被照合の実エンコーディングで Onigmo が動く | 既定で UTF-8 / ASCII。`Regexp#match` + 非 SevenBit + 対応コーデックだけ native 経路。多バイト `NamedByte`（Big5 / GB18030 / EUC-KR/TW）は意図的に除外。他はバイト ↔ `U+00XX` の代理空間 |
| `$~` の格納 | MFP の `vm_svar` | 同じ設計を意図的に写す: LEP の `LFP_SVAR`、遅延確保の `[$~, $_]`、ブロック / lambda が `outer` を辿る。追加で `is_svar_transparent`（monoruby の Ruby 実装コアメソッド用。CRuby はそれらを C で書くので相当物が無い） |
| `Regexp#match?` の `MatchData` 回避 | あり（`rb_reg_match_p`） | あり（`match_pred`）。CRuby より広く `Array#grep` / `Enumerable#grep` も使う |
| `MatchData` オブジェクトの再利用 | `rb_reg_search0` が backref スロットの busy でない `MatchData` を再利用 | **常に新規確保**。40 バイトのペイロード、`SmallVec<[Span; 2]>` の inline 容量、コピー無しの CoW haystack スナップショットで緩和 |
| `onig_region` の再利用 | 照合ごとに再利用 | `captures_from_pos` は照合ごとに `OnigRegion` を確保・解放。再利用は StringScanner のプリミティブ（スレッドローカル `Region`）**のみ**（`onig_region_clear` + memset で約 3.3 %） |
| リテラル接頭辞の最適探索 / `ONIG_OPTION_FIND_NOT_EMPTY` | 使う | 使わない。代わりに `scan` / `replace_repeat` / `replace_all_block_inner` / `scan_block_loop` で**手動走査**（クレートの `captures_iter` は CRuby が yield するはずのゼロ幅一致を落とすため） |
| `Regexp.timeout` | 強制（ReDoS 中断） | スレッドローカルに保存して読み返すだけで**強制しない** |
| `Regexp#==` / `#hash` | `hash` はエンコーディングフラグを無視、`==` は無視しない | 同じ非対称を写す: `==` は `REGEXP_EQ_OPTION_MASK`（m / i / x）+ `declared_encoding`、`hash` はソース + マスクのみ |
| エラーメッセージ | Onigmo の文言 + `: /<source>/` | `pre_validate_regex` / `expand_unicode_braces` / キャッシュミスのエラー腕が CRuby の文言を手で再現 |
| `String#index`（String needle） | `$~` を設定しない | 同じ（issue #721）。さらにエンジンにも入らない |
| `\k<name>` の重複名 | 最後に参加したグループ | 同じ（`get_group_members` 上で実装） |
| `Regexp#initialize` | private、再初期化は raise | private で常に raise: frozen リテラルは `FrozenError`、それ以外は `TypeError: already initialized regexp`。埋めるのは `allocate` の placeholder 経路だけ |

---

## 6. 現在の限界

1. `REGEX_CACHE` と `NATIVE_CACHE` は上限が無く evict されない。`REGEX_CACHE`
   は構築のたびに**書き込み**ロックを取りキー `String` を確保する。
2. `captures_from_pos` は照合ごとに `OnigRegion` を確保・解放する。再利用は
   StringScanner だけ。
3. `$~` の死活解析が無いので `str =~ /x/` と `case … when /x/` は結果を
   使わなくても評価ごとに `MatchData` を確保する。
4. `Regexp.timeout` は不活性。
5. `Regexp#multiline?` は未実装（[`../plan-activerecord.md`](../plan-activerecord.md)）。
6. 非 UTF-8 レシーバに対する Regexp 付き `String#slice!`、同一エンコーディング
   の多バイト `tr` / `count` 集合は追記予定として `string.rs` に記録がある。

---

## 7. テストと関連文書

| 場所 | 内容 |
|---|---|
| `value/rvalue/regexp.rs` の `regex_cache_tests` | キャッシュの契約（Ruby だけのビットがキャッシュを分割しない、等） |
| `builtins/regexp.rs` のテスト | `regexp_match_pred_does_not_set_special_vars`、`gsub_block_without_a_match_probes_first`、`regexp_hash_ignores_encoding`、`initialize` の raise |
| `builtins/string.rs` の `gsub_many_matches_linear` | `splice_all` の線形性 |
| `builtins/fiber.rs` / `builtins/thread.rs` の svar テスト | `root_svar` の分離 |
| [`../stack_frame.md`](../stack_frame.md) | `LFP_SVAR` スロットと LEP の所有規則 |
| [`../gc.md`](../gc.md) | `sp_match_regex` / `sp_match_haystack` の root |
| [`../encoding_char_iteration_design.md`](../encoding_char_iteration_design.md) | バイト ↔ 文字オフセット、`CharByteIter` |
| [`../bop_redefinition.md`](../bop_redefinition.md) | `Regexp#===` が表に無い理由の背景 |
| [`../lir.md`](../lir.md) / [`../arch_difference.md`](../arch_difference.md) | `ConcatRegexp` マクロ命令 |
| [`../threads.md`](../threads.md) / [`../refinements.md`](../refinements.md) | 実行コンテキストごとの `$~` / `$_`、LEP 走査の再利用 |
