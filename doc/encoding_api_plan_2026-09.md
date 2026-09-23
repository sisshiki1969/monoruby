# String とエンコーディング: CRuby 4.0.6 との比較と実装計画

Status: **plan**（2026-09-23、master `38f2bb0` で測定）

## 0. 要約

- **API 表面はほぼ完備**。`String` のインスタンスメソッドは CRuby 134 に対し 141
  （欠けは `dup` / `freeze` / `inspect` を自クラスで定義していないことだけで、継承で
  動く）。差は表面ではなく**挙動**にある。
- 37 種の入力（9 エンコーディング系統 × 正常 / 不正 / 切断）× 170 操作の行列
  6,289 ケースで **601 差異（9.6 %）と 1 クラッシュ**。ruby/spec の `core/string`
  3,905 例は **0 失敗**で、spec が非 UTF-8 をほとんど踏まないことの裏返し。
- 差異は 11 の根本原因に畳める（§3）。最大は **UTF-16/32 の不可逆な取り扱い**
  （158 行、データ破壊）、**CJK コードページ上の正規表現・`ord`・`chr`**（146 行）、
  **ダミーエンコーディングの拒否漏れ**（57 行）。
- IO 層は 98 ケース中 14 差異と良好。パーサは文字列 / シンボル / ヒアドキュメント
  のマジックコメント伝播が正しく、**非 UTF-8 ソースの正規表現リテラルだけが
  FatalError**（#1622）。
- 変換器は CRuby の 8,742 対に対し 6,006 対。不足 2,890 対は**変換器のない 17
  エンコーディング**そのもの。
- 関連 issue は open 30 件中 22 件がこの領域。今回 3 件を修正済みとして閉じ
  （#1590 / #1593 / #1612）、2 件を新規に立てた（#1621 クラッシュ、#1622 正規表現
  リテラル）。
- 計画は 9 フェーズ（§6）。順序は「落ちる・壊れる → 文字境界の完成 → 符号位置 →
  正規表現 → 大小文字 → 数値・書式・JSON → 変換器 → IO → 識別子」。

## 1. 測定方法

すべて同じ環境の CRuby 4.0.6（`vendor/ruby-stdlib/.ruby-version` の pin）と
`target/debug/monoruby` を同じスクリプトで走らせ、`inspect` / エンコーディング名 /
`valid_encoding?` / `ascii_only?` / `frozen?` / 例外クラスと 1 行目までを突き合わせた。
スクリプトは `/tmp/…/scratchpad/`（§7 に一覧）。

| 測定 | 規模 | 結果 |
|---|---|---|
| API 表面（`instance_methods(false)` 等、String / Symbol / Encoding / Converter / Regexp / MatchData） | 6 クラス | String 欠け 3（継承で動く）、`Encoding._dump` / `_load` / `UNICODE_VERSION`、`Converter#insert_output` 無し |
| String 挙動行列 `strmatrix.rb` | 37 入力 × 170 操作 = 6,289 | **601 差異、1 クラッシュ** |
| エンコーディング規則 `misc.rb` / `misc2.rb`（互換性判定、結合、`join`、`chr` / `ord`、識別子名、`Encoding` クラス、変換器網羅） | 95 + 96 行 | 71 + 40 一致（要素単位で差を採取） |
| IO `ioenc.rb`（`LANG` 3 種） | 98 | 14〜15 差異 |
| リテラル `lit/*.rb`（マジックコメント、`\u` 混在、BOM、`eval`） | 15 ファイル | 文字列系は一致、正規表現リテラルのみ不一致 |
| 変換器網羅 `conv_cov.rb`（`search_convpath` 全対） | 103 × 102 | CRuby 8,742 / monoruby 6,006 |
| CJK 全セル `cjkall.rb`（#1544 / #1500 の測定） | 288,792 | 34,274 差異（既知） |
| ruby/spec | core/string 3,905 例、core/encoding 632、core/symbol 270、core/regexp 258、core/matchdata 185、core/integer/chr 26、library/stringio 675 | string / symbol / matchdata / chr **0 失敗**、encoding 2F 1E（= #1617 と変換器の検査順）、regexp 1E（`Regexp.union` の UTF-16）、**stringio 121F 90E**（未実装メソッドが主） |

## 2. 現状の構造

### 2.1 どこに何があるか

| 層 | 場所 | 行 | 責務 |
|---|---|---|---|
| 値表現 | `value/rvalue/string.rs` | 5,798 | `RStringInner`（inline / heap / 共有）、`Encoding` 列挙、`CodeRange` の遅延計算、`CharByteIter`、`precise_len` 群（EUC-JP / SJIS / Emacs-Mule / EUC-KR / CP949 / Big5 / GBK / GB18030 / EUC-TW / stateless / CESU-8）、`compatible_encoding`（`rb_enc_compatible` の移植）、ISO-2022-JP ↔ stateless の書き換え |
| 文字列メソッド | `builtins/string.rs` | 19,408 | `String` の組み込みほぼ全部 |
| 変換 | `builtins/encoding.rs` | 16,671 | `Encoding` クラス、`Encoding::Converter`、`String#encode` / `scrub` / `unicode_normalize`、JIS 系の cell-direct 変換、`stream_convert` と誤り情報 |
| キャリア絵文字 | `builtins/encoding_carrier.rs` | 4,069 | DoCoMo / KDDI / SoftBank の表 |
| 正規表現 | `value/rvalue/regexp.rs` `builtins/regexp.rs` | 3,346 + 3,377 | Onigmo（`onigmo-regex` crate）。`onigmo_encoding_for` が `Encoding` → `OnigmoEncoding` を選ぶ（**EUC-JP / SJIS / ISO-8859-x / KOI8 / Windows-125x のみ**） |
| pack / unpack | `value/rvalue/string/pack.rs` | 2,270 | |
| IO | `builtins/io.rs` `file.rs` `value/rvalue/io.rs` | 9,886 + 4,782 + 2,092 | external / internal、`set_encoding`、BOM、改行装飾 |
| パーサ | `parser/prism_backend.rs` | 6,466 | マジックコメント → リテラルのエンコーディング（正規表現は未対応、#1622） |
| Marshal / JSON / Symbol | `marshal.rs` / `json` 組み込み / `symbol.rs` | | |

### 2.2 既存の設計文書との関係

- [`encoding_char_iteration_design.md`](encoding_char_iteration_design.md)（*proposed*）が
  掲げた P0〜P2（符号化ごとの文字幅、`char_length` / `CharByteIter` / `scrub` の統一）は
  **実装済み**（[`runtime_optimization/string.md`](runtime_optimization/string.md) §2 が
  述べる通り。今回の測定でも `length` / `chars` / `valid_encoding?` は全 37 入力で一致）。
  未着手なのは **P3「`to_str()` の呼び出し側の棚卸し」**で、§3 の A・C・E・F は
  まさにその残滓（非 UTF-8 の文字列を UTF-8 `str` に通してから処理している）。
- `string.md` §7 の「非 UTF-8 上の正規表現は `regex` クレートへのバイト写像」は**古い**。
  現在は Onigmo で、エンコーディングを渡す経路がある。§3-B は「渡していない
  エンコーディングがある」という問題。

## 3. 挙動差の分類（601 差異 + 個別測定）

件数は `strmatrix.rb` の行数。重複する行は主因に寄せた。

| # | 根本原因 | 行 | 代表例（CRuby → monoruby） | 状態 |
|---|---|---|---|---|
| A | **UTF-16/32 が不可逆**: 不正な単位を U+FFFD に書き換える（`chars` / `reverse` / `chomp` / `replace` / `encode(同一)` / `Marshal` / `-@` / `String.new` / `inspect` / `dump.undump` / `setbyte` / `byteslice`）。`strip` が末尾の `\0` を落として `"abc"` を `[97,0,98,0,99]` にする。`ljust` / `center` の詰め物が U+FFFD。`succ` が誤った側のバイトを進める。`start_with?` 等が UTF-8 引数との互換性検査を飛ばして黙って false。`split` / `unicode_normalize` / `hex` / `crypt` が UTF-8 前提で失敗。`dump` の結果が UTF-8（CRuby は BINARY） | 158 | `"ab日".encode("UTF-16BE").strip` → `"愀扥�"`；`"a\x62".force_encoding("UTF-16LE").encode("UTF-16LE")` → `"a�"` | 未 issue |
| B | **CJK コードページで正規表現がバイト単位**（`=~` のオフセット、`[/./]` が 1 バイト、`gsub(/./)` が 9 回、`scan` が文字を割る、`start_with?(/a/)` が「invalid byte sequence in UTF-8」）。`ord` / `codepoints` / `each_codepoint` が先頭バイト（EUC-JP / SJIS も含む）。`Integer#chr(enc)` が JIS X 0208 の 2 バイト以外すべて RangeError。`bytesplice` が文字境界を見ない。`<< int` の例外文言 | 146 | `"한".encode("EUC-KR").ord` → 199（CRuby 51153）；`0x8EB1.chr("EUC-JP")` → RangeError | 未 issue |
| C | **ダミーエンコーディングを拒否しない**（ISO-2022-JP / UTF-7 / UTF-16 / UTF-32）: CRuby は `upcase` 等に `CompatibilityError: incompatible encoding with this operation`。monoruby は処理してしまい、ISO-2022-JP ではエスケープ列まで大文字化して壊す。`encode("UTF-7")` は変換器不在より先に入力を検査して別の例外 | 57 | `"あ".encode("ISO-2022-JP").upcase` → `"\e\x24\x62\x66…"` | 未 issue |
| D | **大小文字変換が符号化を見ない**: 非 Unicode 符号化に `:turkic` / `:fold` で「invalid byte sequence in UTF-8」、`:ascii` で不正 UTF-8 が「input string invalid」（CRuby は ASCII 部分だけ変換）、不正 UTF-16 を変換してしまう（CRuby は `ArgumentError: input string invalid`）、ISO-8859-1 / Windows-1252 の `casecmp?` / `upcase` が表を持たない | 40 | `"caf\xE9".force_encoding("ISO-8859-1").casecmp?("CAF\xC9"…)` → false | 未 issue |
| E | **数値解析が UTF-8 前提**: `to_i` / `to_r`（`to_f` / `to_c` は正しい）が不正バイトや非 UTF-8 で ArgumentError（CRuby は先頭の数字だけ読む）。`hex` が UTF-16 で `CompatibilityError: ASCII incompatible encoding` を出さない | 58 | `"ab\xFF".to_i` → ArgumentError（CRuby 0） | 未 issue |
| F | **`format` / `%c` / 幅**: `%-10s` の幅を非 ASCII で無視、`%c` が非 ASCII 互換・コードページで CompatibilityError | 46 | `format("%-10s|", "日本語")` → 詰め物なし | 未 issue |
| G | **JSON**: 不正 UTF-8 を U+FFFD に潰して生成（CRuby は `JSON::GeneratorError`）、非 UTF-8 ソースを変換せず | 54 | | 未 issue |
| H | `encode(xml:)` が `invalid:` を無視 | 27 | | #1615 |
| I | **coderange の伝播**: UTF-8 の Valid な親から切り出した ASCII 部分文字列に `ascii_only?` が false（`"abc日本"[1]`、`scan`、`match`、`partition`、`byteslice`）。互換性判定が 7bit 判定に依存するので、その先で誤った CompatibilityError になる。`slice!` / `insert` が US-ASCII / UTF8-MAC の結果を UTF-8 にする、`tr` が BINARY を UTF-8 にする、`<< 0xff` が壊れた US-ASCII を BINARY にする | 45 | `"abc日本xyz"[1].ascii_only?` → false | 未 issue |
| J | **例外文言**: 「invalid byte sequence in **UTF-8**」と受信者の符号化を言わない、`Integer#chr` の `invalid codepoint 0x.. in ENC`、`unpack("U")` の `(expected 3 bytes, given 2 bytes)`、`encode(ISO-2022-JP)` の経路名が末尾の hop を欠く（#1584 と同型） | 30 | | 一部 #1584 |
| K | 個別: `chop` が切断 UTF-8 の尾を 1 バイトしか落とさない、`to_sym` が壊れた文字列を EncodingError にする（CRuby は壊れたシンボルを許す）、`sub` が置換の起きない相手にも互換性検査、`delete("あ")` を BINARY に対して拒否、`encode(fallback:)` が BINARY ソースで無視、`Array#join` の空区切りが UTF-16 と非互換扱い、`scrub` 後の coderange が古い、`Regexp.new(utf16文字列)` を拒否、`Encoding.compatible?(US_ASCII, BINARY)` が nil | 40 | | 未 issue |
| — | **クラッシュ**: `send("")` / `:"".to_proc.call(obj)` が Rust panic で abort | 1 | | **#1621** |

`misc.rb` / `misc2.rb` で判明した規則差（行列に載らないもの）:

- `Encoding` オブジェクト: frozen でない、`Encoding.new` / `allocate` / `dup` / `clone`
  が成功する（CRuby は TypeError）、`Marshal.dump` の形式が `o:` で `_dump` 経路でない、
  `name.frozen?` が false、`aliases` / `name_list` の要素が frozen でない、
  `UNICODE_VERSION` が無い、`find` が `latin1` / `utf8` / `UTF8` / `latin2` / `koi8r` /
  `iso2022jp` を余計に受ける。`constants` / `aliases` / `name_list` の中身は一致。
- 識別子: `instance_variable_set("@あ".encode("EUC-JP"))` が TypeError（CRuby は EUC-JP の
  シンボルとして別物に保つ）、BINARY 名の ivar / `send` が UTF-8 名と同一視される、
  `"@\xff".b` の ivar 名を拒否。`define_method` / `Struct` / `Hash` のキーは一致。
- 互換性判定・結合: 文字列同士の `Encoding.compatible?` 35 対、`+` / `<<` / `concat` の
  規則、Hash キー・`hash` / `eql?` / `==` / `<=>` の符号化横断は**すべて一致**。
  `String#inspect` / `dump` の 45 変種も一致。`valid_encoding?` の 90 例も一致。

## 4. 関連 issue の棚卸し

### 4.1 今回の措置

| issue | 内容 | 措置 |
|---|---|---|
| #1590 | `Converter#finish` が ISO-2022-JP の ASCII 復帰を書かない | #1616 で修正済み → **closed** |
| #1593 | wrapper（CESU-8 / UTF8-MAC）の誤り文言が中間符号化を名乗る | #1616 の `report_src` で修正済み → **closed** |
| #1612 | ISO-2022-JP が 343 文字を受け入れる | #1616 で修正済み（PR 本文に閉じ指示が無かった）→ **closed** |
| #1621 | `send("")` の Rust panic | **新規** |
| #1622 | 非 UTF-8 ソースの正規表現リテラル | **新規** |

### 4.2 open の対応表（22 件）

| 領域 | issue | 計画での位置 |
|---|---|---|
| 変換器の逐次動作 | #1617（`src` から不正列を取り除かない）、#1618（失敗 hop の名前）、#1619（境界での消費数）、#1589（`partial_input: true` の応答）、#1592（US-ASCII 宛の途中終端） | Phase 6-a |
| 変換器の選択肢・文言 | #1615（`xml:` と `invalid:`）、#1614（`code converter not found` の対）、#1611（報告順）、#1584（二段 hop の文言）、#1604（`scrub` のブロック + 置換） | Phase 6-a |
| 変換表 | #1544（中国語・韓国語の符号器が WHATWG、14,753 文字）、#1500（`valid_encoding?` が拒む列を `encode` が読む）、#1559（GB12345）、#1424（Emacs-Mule 検証、JIS X 0212）、#1591（Windows-1258 の余計な変換器）、#1575 / #1577（PR #1620 で進行中） | Phase 6-b |
| 名前・登録 | #1530（18 符号化の欠落と `Utf8` / `EucJp` の判別子）、#1520（`name_list`） | Phase 6-b |
| その他 | #1492（`tr` の CRuby 側の癖、追わない）、#1587（rubyspec-stats） | 対象外 |

## 5. エンコーディング API・取り扱いの整理（あるべき責務）

CRuby の `rb_enc_*` の分担に合わせて、monoruby の各層が持つべき契約を書いておく。
§3 の差異はどれも、この契約のどこかを UTF-8 前提の近道が迂回していることに帰着する。

1. **`Encoding` オブジェクト**: 全インスタンスは frozen の singleton。`new` / `allocate`
   / `dup` / `clone` は TypeError。`Marshal` は `_dump` / `_load`（`Iu:` 形式）。
   `find` が受けるのは `name_list` にある名前だけ（大小文字無視）。`UNICODE_VERSION`。
2. **文字列の三値**: バイト列 `content`、符号化 `ty`、coderange `cr`。
   `cr` は **7bit / valid / broken** の意味を CRuby と同じに保つ（7bit = 全バイト <
   0x80）。部分文字列への伝播は「Valid の親 → 子は Unknown」でよく、Valid を継がせては
   いけない（§3-I）。
3. **文字境界層**（実装済み）: `precise_len` / `CharByteIter` が唯一の文字幅の出所。
   **契約**: 「文字」を扱う操作は必ずここを通る。`to_str()`（UTF-8 `str` 化）は
   *UTF-8 互換で valid* な文字列にしか使わず、非 UTF-8 の**バイト保存**操作
   （`strip` / `chomp` / `chop` / `succ` / `reverse` / `center` / `split(空白)` / `dump` /
   `Marshal` / 同一符号化への `encode` / `String.new` / `replace` / `-@`）には使わない。
   これが設計文書の P3。
4. **符号位置 ↔ バイト**（`rb_enc_codepoint_len` / `rb_enc_mbcput` 相当）: 非 Unicode の
   多バイト符号化では「符号位置」= バイト列をビッグエンディアンで詰めた整数
   （EUC-JP の あ = 0xA4A2 = 42146）。`ord` / `codepoints` / `each_codepoint` /
   `Integer#chr(enc)` / `<< int` / `%c` / `bytesplice` の境界検査がこれを共有する。
   UTF-16/32 は単位を復号した Unicode 符号位置。
5. **互換性判定**（実装済み、一致）: `compatible_encoding` を唯一の入口にし、`sub` 等
   は**置換が起きたときだけ**呼ぶ。ダミー符号化は文字操作の入口で
   `incompatible encoding with this operation` を返す（`rb_enc_check` の前段）。
6. **大小文字**: Unicode 符号化（UTF-8 / UTF-16/32 / CESU-8 / UTF8-MAC）は完全な表、
   単バイト符号化は各自の表（Onigmo が持つ `onigenc_*` の case fold を使えるか要確認）、
   多バイト非 Unicode（EUC-JP / SJIS / CJK コードページ）は **ASCII 範囲のみ**。
   `:turkic` / `:fold` は Unicode 系のみ有効。不正入力は `ArgumentError: input string
   invalid`。
7. **正規表現**: `Encoding` → `OnigmoEncoding` の写像を全符号化に張る。crate は
   EUC_KR / EUC_TW / EUC_CN / Big5 / GB18030 / UTF16BE/LE / UTF32BE/LE を既に公開して
   おり、GBK / CP949 / CP51932 / eucJP-ms / Big5-HKSCS 等は Onigmo 本体にある（wrapper
   に列挙を足す）。パターン文字列の符号化はそのまま Onigmo に渡し、リテラルはソース
   符号化で `\xHH` を検証する（#1622）。
8. **変換器**: `Encoding::Converter` は `search_convpath` → hop ごとの逐次実行。
   検査順は **変換器の存在 → 不正列 / 不完全 → 未定義**。誤り情報は失敗した hop の対を
   名乗る（#1618）。`src` からは不正列を取り除く（#1617）。
9. **数値・書式**: `to_i` 系はバイトを ASCII として読む（非 ASCII 互換符号化は
   `CompatibilityError: ASCII incompatible encoding`）。`format` の幅は**文字**数、
   `%c` は書式文字列の符号化で符号位置を書く、結果の符号化は `rb_enc_copy` /
   `rb_enc_associate` の規則（引数の符号化を取り込み、非互換なら
   CompatibilityError）。
10. **JSON**: 生成は入力を UTF-8 に変換し（非 UTF-8 ソースは `encode("UTF-8")`）、
    不正なら `JSON::GeneratorError`。解析は UTF-8 に変換してから。
11. **IO**: `mode: "rb"` は BINARY、`"rt"` / `universal_newline:` は読みでも改行変換、
    `undef:` / `invalid:` / `replace:` は open の選択肢として変換器に渡す、
    `set_encoding` の未知名は警告して無視、`BOM|UTF-16` は binmode 必須、`gets(sep)` は
    区切りの互換性を検査、`printf` は書き込み側の変換を通す。
12. **識別子**: シンボルは（バイト列, 符号化）で同一性を持つ。ASCII のみなら US-ASCII
    に正規化。ivar / cvar / const / メソッド名も同じ。壊れたシンボルは許す。
13. **パーサ**: マジックコメントはすべてのリテラル種に伝播（文字列系は済み、正規表現は
    #1622）。`\xHH` はソース符号化で検証。

## 6. 実装計画

各項目は「測定 → 実装 → base 再ビルドとの突き合わせで 0 regression」の型で 1 PR
（`bin/test` と §7 の行列）。S / M / L は 1 PR の粒度（S: 半日、M: 1〜2 日、L: それ以上）。

### Phase 0 — 落ちる・壊れるものと計測基盤（すべて S、互いに独立）

| 項目 | 対応 | 動く数 |
|---|---|---|
| 0-1 | #1621 `send("")` の panic | クラッシュ 1 |
| 0-2 | §3-I coderange 伝播: UTF-8 Valid の親から子へ Valid を継がせない（`propagated_cr`）。`slice!` / `insert` / `tr` / `<< int` の結果符号化 | 45 行 |
| 0-3 | `Array#join` の空区切りは常に互換 | `chars.join` 6 行 + spec |
| 0-4 | `Encoding` オブジェクトモデル: frozen、`new` / `allocate` / `dup` / `clone` 拒否、`_dump` / `_load`、`UNICODE_VERSION`、`name` / `aliases` / `name_list` の frozen、`find` の余計な別名 6 つ | misc2 8 行 |
| 0-5 | `Encoding.compatible?` の `Encoding` 同士の 3 規則（US-ASCII は相手を返す、非 ASCII 互換は nil） | 3 |
| 0-6 | JSON: 不正 UTF-8 で `GeneratorError`、非 UTF-8 は UTF-8 へ変換 | 54 行 |
| 0-7 | **計測基盤の固定**: §7 のスクリプトを `bin/encoding-matrix` と `monoruby/tests/encoding_matrix/` に移し、期待値は pin された CRuby から `ruby_oracle.tsv` と同じ方式で記録する。以後の各 PR はこの差分で「動いた数 / 戻った数」を示す | — |

### Phase 1 — 文字境界層の完成（設計文書 P3）: 非 UTF-8 のバイト保存（M〜L）

1-1 `to_str()` 呼び出し側の棚卸し（`builtins/string.rs` の約 40 箇所）。「文字が要る」
    「バイトが要る」「UTF-8 の str が要る（valid な UTF-8 互換のみ）」に三分し、前二者を
    `CharByteIter` / バイト API に移す。着手順は §3-A の破壊が大きい順:
    同一符号化への `encode` / `Marshal` / `String.new` / `replace` / `-@` / `dup`
    （恒等操作は無条件にバイト保存）→ `inspect` / `dump`（`\xHH` で書く）→ `chars` /
    `reverse` / `chomp` / `chop` / `strip` / `succ` / `center` 系 / `split(空白)` →
    `setbyte` / `byteslice` / `slice!`。
1-2 ダミー符号化の門: 文字操作の入口で `incompatible encoding with this operation`
    （§3-C の 57 行）。`encode` は変換器の存在を先に見る。
1-3 UTF-16/32 で `hex` / `to_i` 系は `ASCII incompatible encoding`、`crypt` は
    `wide char encoding`、`unicode_normalize` は内部で UTF-8 へ往復。
1-4 `String#dump` の結果は非 ASCII 互換なら BINARY（CRuby の癖だが 1 行）。

見込み: §3-A 158 行の大半、C 57 行、K の一部。

### Phase 2 — 符号位置 ↔ バイト（M）

2-1 `enc_codepoint(bytes) -> u32` / `enc_mbcput(code) -> bytes` を `string.rs` に置き、
    `precise_len` と同じ表で全多バイト符号化（EUC-JP の JIS X 0201 / 0212 面、SJIS、
    EUC-KR / CP949 / Big5 系 / GBK / GB18030 / EUC-TW / stateless / Emacs-Mule /
    UTF-16/32）を覆う。
2-2 それに乗せる: `ord` / `codepoints` / `each_codepoint` / `Integer#chr(enc)` /
    `<< int` / `%c` / `codepoints.pack`。例外文言は `invalid codepoint 0x%X in ENC`。
2-3 `bytesplice` の `offset N does not land on character boundary`。

見込み: §3-B のうち正規表現以外（約 70 行）、chr の misc2 20 行。

### Phase 3 — 正規表現の多エンコーディング（M）

3-1 `onigmo_encoding_for` に EUC_KR / EUC_TW / EUC_CN(GB2312) / Big5 / GB18030 /
    UTF16LE/BE / UTF32LE/BE を追加（crate 側に既にある）。GBK / CP949 / CP51932 /
    eucJP-ms / Big5-HKSCS / Big5-UAO / CP950 / CP951 は `onigmo-regex` の列挙を足す
    （Onigmo 本体にある）。
3-2 `Regexp.new` / `Regexp.union` にワイド符号化のパターン文字列（core/regexp の 1 E）。
3-3 #1622: リテラルをソース符号化で下ろし、`\xHH` を検証する（`prism_backend.rs`）。
3-4 非互換時の文言 `incompatible encoding regexp match (X regexp with Y string)` と
    「invalid byte sequence in Y」の使い分け。

見込み: §3-B の正規表現 76 行、#1622。

### Phase 4 — 大小文字・照合（M）

4-1 `upcase` / `downcase` / `capitalize` / `swapcase` / `casecmp` / `casecmp?` を
    符号化で分岐: Unicode 系 → 現行の表、単バイト → 各表（Onigmo の case fold を流用
    できれば S、無ければ ISO-8859-1..16 / Windows-125x / KOI8 / mac* の表を持つ M）、
    多バイト非 Unicode → ASCII のみ。
4-2 `:turkic` / `:fold` は Unicode 系のみ。`:ascii` は不正入力でも ASCII 部分を変換。
4-3 不正入力は `ArgumentError: input string invalid`（Unicode 系）。

見込み: §3-D 40 行。

### Phase 5 — 数値・書式（S〜M）

5-1 `to_i` / `to_r`（と `oct` / `Integer()` の文字列経路）をバイト解析にし、非 ASCII
    互換は `CompatibilityError`。
5-2 `format` / `%`: 幅と精度を文字数で、`%c` を書式の符号化で、結果符号化を
    `rb_enc_associate` の規則で。`ljust` / `rjust` / `center` の詰め物をワイド符号化で
    正しく符号化。

見込み: §3-E 58 行、F 46 行。

### Phase 6 — 変換器

6-a 逐次動作と文言（各 S〜M、issue 単位）: #1617（`src` の消費、`mac_source_stream` /
    `cesu_source_stream` / UTF-16 復号器の 3 箇所）→ #1589 / #1592（`partial_input`）→
    #1618（hop ごとに問う）→ #1615 → #1614 → #1611 → #1604 → #1619 → #1584。
    加えて: 変換器不在の検査を入力検証より先に（`encode("UTF-7")`、core/encoding の
    `incomplete_input_spec`）、`fallback:` を BINARY ソースでも呼ぶ、ISO-2022-JP-2 の
    余計な変換器（#1591 と同型）、`Converter#insert_output`。
6-b 変換表（L、issue 単位）: #1544（CRuby の表に差し替え、14,753 文字）→ #1500 →
    変換器のない 17 符号化（Big5-UAO / CP50220 / CP50221 / CP850 / GB12345 / IBM037 /
    IBM737 / IBM775 / IBM857 / IBM860〜865 / IBM869 / ISO-2022-JP-KDDI: 2,890 対）→
    #1559 → #1424 → #1530 / #1520。CP50220/1 は ISO-2022-JP の線に乗せれば cell-direct
    で書ける。

### Phase 7 — IO（S、14 差異を個別に）

`File.read(mode: "rb")` の符号化、`set_encoding` の未知名は警告して無視、open の
`undef:` / `invalid:` / `replace:`、`"rt"` と `universal_newline:` の読み側、`gets(sep)`
の互換性検査、`File.readlines(encoding:)`、`printf` の書き込み変換、`BOM|UTF-16` の
binmode 要求、`StringIO#getc` の `pos`、frozen `StringIO` への `write` は IOError。
StringIO の未実装メソッド（`syswrite` / `each_codepoint` / `getch` / `each_byte` /
`each_char` / `ungetbyte` / `fsync`、arity）は符号化と別の stdlib 補完として 1 PR。

### Phase 8 — 識別子とシンボル（M）

シンボルの同一性を（バイト列, 符号化）に。`instance_variable_set` 等が非 UTF-8 /
BINARY 名を受ける。`send(String)` の内部化。壊れた文字列の `to_sym` を許し、
`Symbol#inspect` で `\xHH` を書く。

### 順序の理由

- Phase 0 は独立で小さく、以後の測定を安定させる（0-2 は互換性判定に効くので早い）。
- Phase 1 が最大の破壊（データ消失）を止め、かつ 2〜5 が依存する「非 UTF-8 を
  `str` に通さない」規律を作る。
- Phase 2 の符号位置は 3 の Onigmo と 5 の `%c` の前提。
- Phase 6-a は既に issue 化された個別修正で、6-b の表差し替えは大物なので後ろ。
- Phase 7 / 8 は他と独立で、割り込み可能。

### 追わないもの

- `encode(universal_newline: true)` が不正入力の結果に valid の coderange を付ける
  （CRuby の装飾器の癖）。`xml:` + `crlf_newline:` の `\r\r\n`。
- #1492 `String#tr` の 7bit 切り詰め。
- `warning: historical binary regexp match /.../n` の警告（出さない）。

## 7. 検証手順と資産

| スクリプト | 規模 | 期待 |
|---|---|---|
| `strmatrix.rb`（入力ごとに別プロセスで走らせ、クラッシュを `crashes.txt` に採る） | 6,289 | 差異数が単調に減る。base との比較で「戻り」0 |
| `misc.rb` / `misc2.rb` | 95 / 96 | 同上（要素単位） |
| `ioenc.rb`（`LANG=` / `C.UTF-8` / `ja_JP.UTF-8`） | 98 × 3 | |
| `lit/*.rb` | 15 | |
| `conv_cov.rb` | 10,506 対 | 8,742 に近づく、余計な 154 が消える |
| `cjkall.rb` / `sjp_full.rb` / `isow3.rb` / `hop2.rb` | 288,792 / 44,180 / 13,009 / 325 | 既知の値 |
| ruby/spec `core/string` `core/encoding` `core/symbol` `core/regexp` `core/matchdata` `core/integer/chr_spec.rb` `library/stringio` | | string / symbol / matchdata / chr は 0 を保つ |

いずれも「CRuby 側の出力をファイルに固定し、monoruby 側と diff」で、`ruby_oracle.tsv`
と同じ思想。Phase 0-7 でリポジトリに入れる。
