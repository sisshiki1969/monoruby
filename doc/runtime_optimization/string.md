# String の実装と最適化

String はテンプレートエンジンとパーサの最内ループに出てくる型で、erubi の
バッファ `<<`、Hash の String キー、`"lit".freeze` がそれぞれ計測で
ボトルネックになった
（[`../yjit_bench_slow_investigation_2026-09.md`](../yjit_bench_slow_investigation_2026-09.md)
§5.2、§5.6）。

この文書は、String のセル内レイアウト、部分文字列の共有と copy-on-write、
エンコーディングと code range のキャッシュ、リテラルの扱い、VM と JIT の
高速経路をまとめ、最後に CRuby との実装差異を並べる。共通の前提は
[README.md](README.md)、Hash の String キーについては [hash.md](hash.md) を参照。

---

## 1. 表現

### 1.1 セルと 32 バイトの inline 閾値

```rust
#[repr(C)]
pub struct RStringInner {
    content: StringContent,   // 先頭固定: JIT が直接アドレッシングする
    ty: Encoding,             // repr(u8)
    cr: Cell<CodeRange>,      // repr(u8)、遅延で埋まる
}
```

バッファは fork した `SmallVec<[u8; 32]>`（`STRING_INLINE_CAP = 32`、
`lib.rs`）で、Array と同じく `OFFSET_CAPA` / `OFFSET_INLINE` /
`OFFSET_HEAP_PTR` / `OFFSET_HEAP_LEN` を JIT に公開するために fork している。
**32 バイトまでは 64 バイトの RValue セルの中**（malloc 無し）、超えると
ヒープにあふれる。inline のあいだは `capa` スロットが長さを兼ね、あふれると
`capa` は容量で長さはポインタの隣に置かれる。JIT の全 emitter は
`cmp capa, STRING_INLINE_CAP` でこの二重の意味を分岐する。`Encoding` が
`u8` ペイロードに収まるサイズに抑えられているのは RValue を 64 バイトに
保つためである。

### 1.2 共有文字列と copy-on-write

CRuby の `STR_SHARED` に相当する仕組みがあり、**あふれた SmallVec の
フィールドに重ねる**形で置かれている:

| offset | SmallVec（あふれ時） | `SharedContent` |
|---|---|---|
| 0 | capacity | `tag`（= `STRING_SHARED_TAG` = `isize::MAX`） |
| 8 | heap ptr | `ptr`（root のバッファ内を指す） |
| 16 | heap len | `len` |
| 24 | （inline の残り） | `root: Value` |

タグが `usize::MAX` でなく `isize::MAX` なのは、JIT のインライン `bytesize` /
`getbyte` が `capa > INLINE_CAP` を**符号付き**で比較するため（`cmovgt` /
`csel gt`）で、正の値にしておけば共有文字列が自動的にヒープ経路に乗る。
その帰結として **読み取り専用のインライン命令は共有検査を一切必要としない**。
検査が要るのは書き込み側だけである。

- `union StringContent { owned, shared }` の判別は先頭の `usize` を読むだけ。
- `Clone`: **sharer の複製は O(1)**（4 語のコピーで同じ root のもう 1 つの
  sharer）。所有バッファの複製はバイトコピー。
- `owned_mut()` が**唯一の変更の入口**で、書き込みはまず `uniquify()`
  （見ているバイト列を新しい所有バッファに写す）を通る。これが CoW の
  「write」側。JIT 向けには同じものが `detach()` / `runtime::str_detach` と
  して出ている。
- `string_substring(parent, start, end)` が共有するのは **`len > 32` かつ
  親が既に共有かヒープあふれのとき**だけ。inline に収まる複製は最大 32
  バイトで、root の確保 + GC エッジより安い。
- `ensure_shared_root(parent)` の 3 ケース: (1) 親が既に sharer → その root
  を返す（**root の連鎖は決して作らない**）、(2) 親が frozen → 親自身が
  root（chilled は該当しない）、(3) 可変でヒープあふれ → バッファを
  `mem::take` して**隠れた frozen root String** を新設し、親を最初の sharer に
  する。確保をまたぐ瞬間だけ親は空文字列として妥当な状態にあり、
  `parent.write_barrier(root)` が String が持ちうる唯一の外向き参照を記録する。
- `string_snapshot(receiver)`: 正規表現系メソッドが使う frozen で安定な
  スナップショット。ブロックがレシーバを書き換えても match のビューは
  有効なまま。frozen ならそれ自身、共有／あふれなら frozen root（レシーバが
  窓なら frozen なサブビュー）、短い inline なら frozen な複製。
- `check_string_not_modified`（CRuby の `str_mod_check`）は**バイト長だけ**
  で判定する。monoruby は同じ長さの in-place 編集でも CoW detach で再確保
  するので、ポインタ比較だと過剰に発火する。

共有ビューを作る側: `String#[]` / `slice` / `chomp` / `strip` 系 / `scan` /
`byteslice` / `lines` / `each_line`、`MatchData#[]` / `pre_match` /
`post_match` / `$~` の haystack、`Regexp` 側の各種（[regexp.md](regexp.md)）。

GC: sharer は root を mark し（`rvalue.rs` の STRING アーム）、
`young_child_exists` は「若い root を持つ sharer」を報告する — 報告しないと
minor GC が root を足元から解放する。String は OLD へ昇格可能。

`shared_string_tests`（`string.rs`）が overlay のオフセットを `smallvec::OFFSET_*`
と突き合わせ、短いスライスが所有のままであること、sharer のサブ文字列が
同じ root を使うこと、frozen 親が自分自身を root にすること、sharer の
clone が安価で独立していることを固定する。

### 1.3 順序・等価・ハッシュ・C 連携

- `PartialEq` は `as_bytes()` の比較（memcmp）。`Hash` はバイト内容のハッシュで、
  **String にダイジェストはキャッシュされない**（§7）。
- `Ord` は手書きの prefix 比較で、ループ内の長さ差分岐を避ける。
- `nul_terminated_buf_ptr`: CoW を先に detach し（C 側が書くかもしれない）、
  必要なら `reserve(1)` して**予備容量に** NUL を書く。`as_bytes` / `len` /
  `hash` / 等価は変わらない。Fiddle / FFI ブリッジが使う。
- 容量成長は SmallVec の倍々で、CRuby の `rb_str_capa` のような調整は無い。
  事前サイズ指定は `concatenate_string_inner`（§4.2）と
  `with_encoding_capacity` の 2 箇所。

---

## 2. エンコーディングと code range

- `CodeRange { Unknown=0, SevenBit=1, Valid=2, Broken=3 }` は `repr(u8)` で
  判別値を固定している（JIT が直接書く）。
- `Encoding` は `repr(u8)`: `Ascii8(0), Utf8(1), UsAscii(2), Utf16Le(3),
  Utf16Be(4), Utf32Le(5), Utf32Be(6), Iso8859(u8), EucJp, Sjis(u8),
  Iso2022Jp, Other(u8), NamedByte(u8)`。先頭 7 つはペイロード無しで、
  JIT の `<<` 高速経路の `<= 6` 判定はこれに対応する。`Other` は UTF-7 /
  CP50220/1 / BOM 付き UTF-16/32（ASCII 非互換、コーデック無し、名前だけ
  保持）、`NamedByte` は Big5 / GBK / GB18030 / EUC-KR / Windows-125x /
  IBM* / KOI8 / TIS-620 / Emacs-Mule など約 38 の ASCII 互換コードページで、
  格納と反復は ASCII-8BIT として振る舞う。
- `cr` は**遅延**: `code_range()` が最初の問い合わせで分類して `Cell` に
  入れ、`set_encoding` が無効化する。`Encoding::classify` は空なら SevenBit、
  ASCII 互換で全バイト < 0x80 なら SevenBit（短絡）、あとはエンコーディング
  ごと（UTF-8 は `str::from_utf8`、UTF-16/32 はバイト数の偶奇、EUC-JP / SJIS
  は `*_char_width`、ISO-2022-JP は `encoding_rs`）。
- コンストラクタは「走査する／しない」の対で用意されている:
  `from_str` / `from_str_scanned`、`from_string` / `_scanned`、
  `from_encoding` / `_scanned`、`from_vec_scanned`（UTF-8 か binary かを同じ
  一巡で自動判定）、`from_ascii_bytes`（呼び出し側が SevenBit を保証）、
  `from_vec_cr` / `from_buf_cr`（`StringBuf` に直接組んだバッファをそのまま
  採用）、`with_encoding_capacity`（SevenBit で開始）。方針は「長生きする
  テンプレートは先に走査（複製が `cr` をただで継ぐ）、使い捨ては後回し」。
- **追記時の O(1) 畳み込み**（`extend`）が最大の性能修正である。素朴に `cr`
  を Unknown に戻すと、10 万回の `s << "abcde"` ループが **CRuby 5.6 ms に
  対して約 5.7 s**（O(N²)）になった。`(SevenBit, SevenBit) → SevenBit`、
  `({SevenBit, Valid}, {SevenBit, Valid}) → Valid`、それ以外 Unknown、の
  畳み込みで O(N) に戻る。`extend_from_slice_checked` /
  `extend_from_slice_merge_cr`（`pack` の `buffer:`）も同じで、
  `extend_from_slice_no_validate`（`append_as_bytes`）は意図的に畳まない。
- 部分文字列への伝播 `propagated_cr`: SevenBit は無条件、Valid は
  単バイト系なら無条件、UTF-8 は両端が文字境界のとき、UTF-16/32 は 2/4
  バイト整合のとき。UsAscii / EucJp / Sjis / Iso2022Jp と Broken / Unknown の
  親は Unknown に落とす。
- `set_byte` は ASCII バイトを置くときだけ SevenBit キャッシュを保つ
  （JIT の `setbyte` が同じ規則を再現する）。
- `char_length` はバイト指向のエンコーディングと、SevenBit がキャッシュ
  された UTF-8 / EUC-JP / SJIS で O(1)。UTF-8 の Valid は非継続バイトを数え、
  Broken は `iter_char_bytes` を歩く。
- `regex_view` / `from_mapped_utf8` / `needs_byte_mapping`: UTF-8 専用の
  `regex` クレートをバイト指向エンコーディングにかけるための
  バイト ↔ `U+00XX` 代理写像（[regexp.md](regexp.md)）。
- `compatible_encoding` / `Encoding::compatible` は `rb_enc_compatible` の
  移植で、キャッシュ済みの `cr` を消費して再走査しない。

[`../encoding_char_iteration_design.md`](../encoding_char_iteration_design.md)
はエンコーディングごとの文字境界層の提案（*proposed*）で、§2 に現状の
`RStringInner` の穴（EUC-JP / SJIS のバイト単位反復、非 UTF-8 の `to_str`
退避）の監査がある。ただしその文書は `content: Vec<u8>` 時代の記述で、
SmallVec / 共有 union への改修より前のものである。

---

## 3. リテラル

### 3.1 fstring プール

`Store::frozen_str_pool: HashMap<(Vec<u8>, Encoding), Value>` は `(bytes,
encoding)` をキーにしたプログラム全体のプールで（`require` をまたいで
生き、`Store::mark` で GC root）、`Store::intern_frozen_str` が
`Value::string_from_source_bytes`（`cr` 走査済み）+ `set_frozen()` で作って
記憶する。目的は CRuby と同じく pragma 下で `"abc".equal?("abc")` を true に
すること。

### 3.2 `# frozen_string_literal: true`

`emit_string` / `emit_bytes` は pragma 下で `emit_frozen_interned` →
`BytecodeInst::FrozenLiteral`（**そのままロード、複製しない**）。pragma が
無ければテンプレート値 + `BytecodeInst::Literal` で、評価ごとに `deep_copy`。
`emit_literal` は `is_always_frozen` なクラスのリテラルも `FrozenLiteral` に
回す。`source_encoding()` は `# encoding:` マジックコメントを解決し既定は
UTF-8。

### 3.3 chilled リテラル（CRuby 3.4 の移行経路）

pragma の無いファイルのテンプレートは chilled と印を付ける
（`chill_literal_template`）。ただし monoruby 自身の `install_root()` 配下の
ランタイム Ruby は除く（`Warning[:deprecated]` がインタプリタ内部を指さない
ように）。フラグは `CHILLED_LITERAL_BIT = 0b1000_0000`（意図的に下位バイトの
中 — 以前は GC 年齢フィールドと衝突していた、issue #975）。`Symbol#to_s` も
chilled な String を返す。`--debug-frozen-string-literal` はリテラルの出自を
`record_string_origin` / `string_origin` で記録し、エラー表示が参照する。

### 3.4 非 frozen リテラルの評価: テンプレート + O(1) CoW 複製

`Value::value_deep_copy` は最初に `share_string_buffer(&mut val)` を呼ぶ。
**テンプレート自身が（1 回だけ）隠れ frozen root の sharer に変わり**、以後
の評価ごとの `deep_copy` は O(len) のバイトコピーではなく O(1) のビュー複製
になり、キャッシュ済みの `cr` も継ぐ。ERB / heredoc サイズのテンプレートが
動機。chilled（デバッグ時は出自も）は各複製に伝播する。

VM の `vm_literal` は `Value::value_deep_copy` への call。JIT の
`TraceIr::Literal` → `DeepCopyLit` → `emit_deep_copy_lit` で完全にインライン
確保されるのは **Array リテラルだけ**で、String は `deepcopy_literal` の
call になる。`FrozenLiteral` は純粋なレジスタロード。

### 3.5 `"lit".freeze` の畳み込み（CRuby の `opt_str_freeze`）

`bytecodegen/method_call.rs` が、safe-nav でなく・**pragma 下でなく**
（pragma 下ではリテラルが既にインターン済みオブジェクトで `Object#freeze` は
安い no-op）・メソッドが `freeze`・引数無し・レシーバが `NodeKind::String`
のときだけ `BytecodeInst::StringFreeze(reg, interned)` を出す。実行時の
`string_freeze_literal` は `String#freeze` が未再定義ならインターン済み
リテラルをそのまま返し（複製も呼び出しも無し）、再定義されていれば
`dup` + `set_chilled_literal` + 再定義された `freeze` を dispatch する。
BOP 表に `(STRING_CLASS, "freeze")` があり、JIT は `basic_op_assumable` の
もとで BOP 依存を記録して定数ロードにし、再定義は本体を evict、コンパイル時
に再定義済みなら無条件 deopt。

計測: `'abc'.freeze` **67.6 → 9.8 ns**（YJIT 26）、`buf << 'lit'.freeze`
**73 → 15.9 ns**（YJIT 21）。

**`-"lit"`（`opt_str_uminus`）の畳み込みは無い。** `String#-@` は Ruby で
`frozen? ? self : dup.freeze` と定義されていて（`builtins/string.rb`）、
CRuby の `rb_fstring` のような**重複排除テーブルは無い**。`String#+@` は
chilled フラグに Ruby レベルの述語が無いため Rust のまま。

---

## 4. VM の経路

### 4.1 `+` / `<<`

- `+`（`add`）はサブクラスのレシーバでも**素の String** を返す（`dup` せず
  新規に組む）。`#to_str` が Ruby に再入するので引数を先に変換する。
- `<<`（`shl` / `shl_inner`）は `s << s` のエイリアスを inner のスナップ
  ショットで処理し（sharer なら O(1)）、Integer をコードポイントとして
  受ける CRuby の `rb_str_concat` の ASCII 特例（US-ASCII は高位バイトで
  ASCII-8BIT に自動拡張）、`codepoint_bytes` によるエンコーディングごとの
  符号化（`rb_enc_codelen` / `rb_enc_mbcput` 相当、`Invalid` と `OutOfRange` で
  onigmo の 2 つのメッセージを再現）、`#to_str` 変換中のレシーバの
  `vm.temp_push` による root を行う。`extern "C" fn string_shl` が JIT の
  呼ぶフレームレスの入口。

### 4.2 補間 — `ConcatStr`

バイトコード側（`bytecodegen/expression.rs`）: 補間がリテラル断片で
始まらないときは空の `seed` リテラルを押し、実行時の結合が常にソース
エンコーディングから交渉するようにする（pragma 下でも結果が新規の可変
String になる）。**リテラル断片はインターン済み frozen テンプレートとして
出す**（評価ごとの deep copy ではない）。CRuby も dstr の断片を frozen で
埋め込んでおり、テンプレートのキャッシュ済み `cr` が結合の逐次交渉に
流れ込むので、大きな断片の分類は一生に 1 回で済む。

実行時（`runtime::concatenate_string_inner`）の設計:

- 一貫して生バイト（Rust の `String` を経由すると不正列が U+FFFD に書き
  換わる）。
- エンコーディングは最初の String オペランドから seed し、以降は
  `Encoding::compatible` で交渉。
- **エンコーディングと `cr` を逐次で追跡する。** 以前は交渉のたびに
  成長中のバッファを一時 String で包んでオペランドごとにコピーしていて
  O(N·total) だったのが O(total) になった。
- 結果は String オペランドから事前サイズを決めた `StringBuf` に**直接**
  組む。短い結果はヒープに触れず、長い結果は確保 1 回で、
  `RStringInner::from_buf_cr` がそのまま採用する。
- **Fixnum オペランドはバッファに直接整形する**（`format_i64` +
  `append_piece`）。`to_s` でヒープ String を作らない。`vm.to_s_is_refined`
  で refinement が勝つようゲート。
- ユーザーの `to_s` が非 String を返せば `#<Class:0xADDR>` に落とす。

JIT の `TraceIr::ConcatStr` → `AsmInst::ConcatStr` → `LInst::ConcatStr` →
`emit_concat_str` は同じヘルパへの call。

### 4.3 `==` / `eql?` / `!=` / `<=>` / `hash`

- `==`（`===` / `eql?` は同じ builtin と同じインライン生成器）:
  `string_eq_bool` はバイト比較**に加えて**エンコーディング互換検査を行う
  （CRuby 準拠: 交渉できないエンコーディングの同一バイト列は不一致）。右辺
  が String でなく `to_str` に**応答する**なら（`store.no_to_str` で判定、
  `to_str` 自体は呼ばない）`rhs == self` を dispatch する。
- `!=` は基本演算（`define_basic_op`）で、CRuby に `String#!=` は無いので
  上記の逆 dispatch を含む厳密な否定になっている（右辺が `to_str` と独自
  `==` の両方を持つ場合に素のバイト比較だと乖離する）。
- `<=>` はバイト比較に**エンコーディング序数のタイブレーク**（CRuby の
  `rb_enc_index` 順）。非 ASCII 内容でだけ効く。
- `hash` は `Value::calculate_hash` → プロセスごとの `RandomState` で
  seed した SipHash（`HASH_STATE`、CVE-2011-4815 対策）を `from_hash_digest`
  で Fixnum 範囲に畳む。**String ごとのハッシュキャッシュは無い**。

### 4.4 `[]`、`to_sym`、`%`、その他

- `String#[]` の Fixnum / Range 形は**コピー無しの共有部分文字列**。Range は
  `rb_range_beg_len` の順（負の正規化 → `exclude_end` の畳み込み）で、
  以前 `"abc"[0...0]` が折り返していたバグの記録がある。ASCII のみの
  レシーバは SevenBit キャッシュで `char_length` が O(1)。
- `to_sym` / `intern` は CRuby のシンボルエンコーディング正規化を実装する:
  ASCII 互換エンコーディングの ASCII のみ内容は US-ASCII に畳む
  （`"a".force_encoding("KOI8-R").to_sym == :a`）、US-ASCII / UTF-8 名は
  文字列で、他は `(bytes, encoding)` でインターン。`IdentifierTable`
  （`id_table.rs`）は UTF-8 名の `rev_table`、バイナリ名の
  `rev_table_bytes`、前方の `table: Vec<IdentName>`、バイトインターンした
  シンボルの出自エンコーディングを持つ `enc_map` からなり、
  `LazyLock<RwLock<_>>` のグローバル。`IdentId::compare` は id の一致で
  read lock を取る前に短絡する。
- `%`（`rem`）は Hash 1 個なら名前参照の源として `format_by_args` へ
  （`%{name}` と `%<name>spec` の両方）、Array は splat、それ以外は
  `to_ary` を試してから包む。`Kernel#format` と共有。JIT 特殊化は無い。
- `*` は overflow ガード後 `RStringInner::repeat` で、`cr` を再走査せず
  解析的に保つ。
- `split` は `$~` の haystack を設定してコピー無しのスナップショットを使い、
  非 UTF-8 で空セパレータなら `iter_char_bytes` の経路。
- `lines` / `each_line` は**ブロックが走る前に**全行を実体化する
  （`build_lines`）ので、ブロックがレシーバを変えても全ビューが同じ
  frozen root を指す。
- `scan` / `gsub` / `sub` は先に `string_snapshot` を取り、各 match 結果は
  スナップショットの共有ビュー、`$~` も同じバッファを共有する。
- `Array#pack` の `buffer:` は `extend_from_slice_merge_cr`、`Array#join` は
  `from_vec_cr` を使う。
- `string_alloc_func` は空の **ASCII-8BIT** 文字列を作り、`String.new` は
  汎用の `Class#new` を通るのでサブクラスの `initialize` 上書きが効く。

---

## 5. JIT の経路

### 5.1 登録されているインライン生成器

| メソッド | 生成器 | 出るもの |
|---|---|---|
| `==` / `===` / `eql?` | `string_eq_gen` | 無し — 定数畳み込み |
| `!=`（基本演算） | `string_ne_gen` | 無し — 定数畳み込み |
| `<<` | `string_shl_gen` | `emit_string_shl` |
| `bytesize` | `string_bytesize` | 純 LIR `StringLenFixnum` |
| `getbyte` | `string_getbyte` | `emit_string_getbyte` |
| `setbyte` | `string_setbyte` | `emit_string_setbyte` |

これ以外の String メソッドはインライン化されていない。特に `length` /
`empty?` / `==` のバイト比較 emitter は無く、`String#length` と
`String#empty?` は普通の builtin である。

### 5.2 `==` / `!=`: コンパイル時の定数畳み込み

右辺のクラスがコンパイル時に分かっていて、それが `String` でも `to_str` を
持つクラスでもなければ（`store.no_to_str(rhs_class, class_version)`）、比較
全体をコンパイル時定数の `false` / `true` に畳む — 至る所にある `str == nil`
がこれ。クラスは二項演算のインラインキャッシュ由来の推測なので
`guard_class` を出し、後からの（再）定義はインライン dispatch が既に出して
いる class-version ガードが捕まえる。結果は `state.def_C` で登録するので、
値としての利用は抽象状態から読み、分岐での利用は静的に解決される（素の
`CondBr` は真偽で、融合した `BinCmpBr` は `binary_cmp_br` で）。`no_to_str`
はクラス × class_version でメモ化され、JIT コンパイル中に global method
cache の `RefCell` が借用中でも呼べるよう、キャッシュしない祖先走査で
解決する。

### 5.3 `bytesize`: 型付き LIR マクロ命令

```
movq d, [b + RVALUE_OFFSET_ARY_CAPA]
cmpq d, STRING_INLINE_CAP
cmovgtq d, [b + RVALUE_OFFSET_HEAP_LEN]
salq d,1 ; orq d,1
```

共有文字列の `capa` は `isize::MAX` と読めるのでヒープ長のロードに回り、
それは `SharedContent::len` である — 「共有タグは正でなければならない」
要件の出所。aarch64 は `csel gt`。

### 5.4 `getbyte` / `setbyte`

`emit_string_getbyte`: 添字の untag、`cmp capa, 32` + `cmovgt` の対で
`(len, ptr)` の inline / heap 選択、`cmovs` で負添字の補正、符号無し境界
検査（まだ負の添字も同時に捕まえる）、`movzxb` + Fixnum タグ。**範囲外は
インラインで nil** を返すので `while b = s.getbyte(i)` の終端で deopt しない。
共有検査は overlay により不要。

`emit_string_setbyte`: frozen（`0b010`）／chilled（`0b100`）のレシーバと
範囲外添字で deopt。**共有レシーバは deopt せず detach して再試行する。**
`s = lit.dup; s.setbyte(..)` では共有ミスが慢性化し（dup した文字列ごとに
1 回）、side-exit のエスカレーションが無条件になった今、deopt のたびに
呼び出し元の連鎖全体を歩いて変換してしまう（ruby-xor の退行）。out-of-line
経路は `runtime::str_detach`（素の malloc + memcpy、Value 確保無し、GC 無し）
を呼んで `reload` に戻る。**cr キャッシュはインラインで整合を保つ**:
SevenBit + バイト < 0x80 は SevenBit のまま、それ以外は `STRING_CR_OFFSET` に
`Unknown` を書く（`RStringInner::set_byte` と同じ規則）。

### 5.5 `<<` — 最大のもの

`string_shl_gen` は証明済みのレシーバクラスを要求し、引数クラスから
`StringShlHint`（`Integer → Fixnum`、`String → Str`、それ以外 `Both`）を
導いて使わない経路を出さない。レシーバクラスの制限は要らない（String
サブクラスも同じ builtin に解決する）。`emit_string_shl` の doc コメントが
正式な説明で、要点は:

**Fixnum バイト経路**: 引数が Fixnum、レシーバが frozen / chilled でない、
バイトが 0..=255、エンコーディングタグが `STRING_TY_MAX_INLINE_SHL`
（= `UsAscii` = 2）以下 — Ascii8 / UTF-8 / US-ASCII では 7 ビットの
コードポイントがそのままバイトで、UTF-16/32 は 2/4 バイト、US-ASCII より
先は高速経路で組めない多バイト列がある — 高位バイトはさらに Ascii8
（タグ 0）に絞る。共有レシーバは fallback（この経路は detach しない）。
inline / heap の格納と容量検査（満杯は fallback — 成長で deopt しては
**いけない**）。cr の畳み込み: ASCII バイトはキャッシュを保ち、高位バイトは
Unknown を書く。

**String 引数経路**: 引数が `ObjTy::STRING` のヒープオブジェクト、レシーバが
frozen / chilled でない。タグが等しいときは両方**ペイロード無し**
（`<= 6`）、**不一致**のときは両方 ASCII 互換（`<= 2`）で断片が SevenBit
キャッシュ済み、レシーバが SevenBit か Valid のキャッシュ済み — これは
`Encoding.compatible?` がレシーバのエンコーディングに解決するケース
（コメントは「erubi のバッファの形」と呼ぶ）。断片の `cr` が単に未キャッシュ
なら fallback するが、ヘルパが分類してキャッシュするので、同じ断片は
**2 回目の追記から**インラインになる。**共有レシーバは out-of-line の
`str_detach` + 再試行**で fallback ではない（「共有は `dup` ごとの出来事で、
高速経路を永久に離れる理由ではない」）。**引数**は共有でも overlay 経由で
正しく読める。両側の inline / heap 選択、容量検査、バイトコピーループ
（`s << s` は重ならない: 元 `[0,len)`、先 `[len,2len)`）、生きているスロット
（inline なら `capa`、あふれなら `heap_len`）への長さ書き戻し。
**cr の畳み込みはレジスタ内で** `RStringInner::extend` と同じ: `sub 1` で
SevenBit→0、Valid→1、Unknown はラップ、Broken→2 とし、`ja` 1 つで「整形
でない」を捕まえ、2 つの `or` で SevenBit か Valid を決める。

fallback は生成コードの中で `string_shl(vm, globals, recv, arg)` を
**deopt 無しで**末尾呼び出しし、builtin の完全な意味論を運ぶ。

レイアウト定数 `STRING_CR_OFFSET` / `STRING_TY_OFFSET` /
`STRING_TY_MAX_INLINE_SHL` / `Encoding::tag()` は
`inline_shl_encoding_tags_are_pinned` テストが固定する。振る舞いは
`tests/string_bytes.rs` が全ケースでバイト列・エンコーディング・**キャッシュ
された code range**（`valid_encoding?` / `ascii_only?` 越しに観測）を CRuby
オラクルと突き合わせる。トップレベルの main スクリプトは JIT されないので、
各ホットな追記ループは 20 回の呼び出し閾値を越える `def` の中に置く必要が
ある、とファイル先頭が注意している。

### 5.6 Hash の String キー

`Hash#[]` のインライン probe は、キークラスが `String` ちょうどなら
`string_digest_c`（バイト列の wyhash、挿入時と同じ）と `string_key_eq_c`
（同一性 → STRING×STRING バイト比較）を葉ヘルパにして機械語で探索する。
再定義された `String#hash` はバケッティングに一切使われない。frozen String は
inline（≤ 3 ペア）Hash のキーとして許され、リテラルキーも `h["k"] = v` の
格納キー（`Value::frozen_hash_key` が複製して freeze）も frozen なので
`{"content-type" => "text/plain"}` は boxed map を作らない（134 → 57 ns）。
詳細は [hash.md](hash.md) §1.1、§1.3、§4.2。

---

## 6. Ruby で書かれているメソッド

`builtins/string.rb`:

- `include Comparable` は `between?` / `clamp` を得るため**だけ**。順序演算子
  は native のままで、クラス自身のメソッドがモジュールより勝つので
  `<` / `<=` / `>` / `>=` は遅い `<=>` 経由に落ちない。
- `to_s` / `to_str`、`insert`、`prepend`、`chop` / `chop!`、`delete_suffix` /
  `!`、`partition` / `rpartition`、`codepoints` / `each_codepoint`、`clear`
  （`bytesplice` 経由）、`upto`。
- `concat` / `append` は `rb_str_concat_multi` の実装で、引数が 2 つ以上なら
  先にバッファに集めてからつなぐので、レシーバをエイリアスする引数は
  入口の値で寄与する（`str.concat str, str` は 4 倍でなく 3 倍）。
- `each_byte` は**明示的に性能上の選択**で、`bytes.each(&block)` でなく素の
  `while` ループ。前者は bytesize 個の Array と Proc を呼び出しごとに作って
  いた。ここでは JIT が `bytesize` / `getbyte` / `yield` をインライン化し、
  「バイトあたり約 3 倍速い」。
- `upto` は CRuby の 2 つの特例（全桁数字の端点は整数として、1 文字 ASCII の
  端点はバイトで反復）を再現する。
- `-@` / `dedup` は `frozen? ? self : dup.freeze`。**重複排除テーブル無し**。
  `+@` は chilled に Ruby レベルの述語が無いため Rust に残っている。

---

## 7. CRuby との実装差異

| 項目 | CRuby | monoruby |
|---|---|---|
| 埋め込み文字列 | `RSTRING_EMBED`、64 ビットで約 24 バイト | **32 バイト**。fork した `SmallVec<[u8; 32]>` が 64 バイトの RValue セル内 |
| 共有文字列 | `STR_SHARED` + RString の `shared` フィールド | あふれた SmallVec のフィールドに重ねた `SharedContent`、`capacity == isize::MAX` でタグ。**読み取り専用の JIT 命令が共有検査を要らないよう**に設計 |
| 共有の方針 | 積極的（`rb_str_new_shared`、`RSTRING_EMBED_LEN_MAX` 超の `rb_str_substr`） | `len > 32` **かつ**親が既に共有かヒープあふれのときだけ |
| root | 共有の親自身が共有でありうる（連鎖、`rb_str_shared_root` で解決） | 隠れた **frozen** root。sharer は連鎖しない。frozen な親は自分自身が root |
| CoW | `rb_str_modify` → `str_make_independent` | `owned_mut()` / `uniquify()` の単一入口 |
| `rb_fstring` の重複排除 | pragma のリテラル**と**実行時の `-"str"` / `String#-@` / Hash キーに適用 | コンパイル時のリテラルプール `Store::frozen_str_pool`（`(bytes, encoding)` キー）**のみ**。`String#-@` は素の `dup.freeze`（**実行時の重複排除無し**）。Hash の String キーは格納ごとに `dup` + `freeze` でインターンしない |
| chilled リテラル | CRuby 3.4 の `STR_CHILLED` | ヘッダフラグのビット 7 `CHILLED_LITERAL_BIT`、`Symbol#to_s` 用の別の `is_chilled` ビット、`--debug-frozen-string-literal` の出自テーブル |
| coderange | `ENC_CODERANGE_*` を RBasic のフラグにキャッシュ | JIT 既知オフセットの `Cell<CodeRange>`（`repr(u8)`）。**遅延**計算で、追記・部分文字列・repeat では再計算でなく O(1) で畳む |
| `String#hash` | seeded SipHash。fstring 側でキャッシュ | seeded SipHash（`DefaultHasher` + プロセスごとの `RandomState`）。**String ごとのキャッシュ無し**（[`../yjit_bench_slow_investigation_2026-09.md`](../yjit_bench_slow_investigation_2026-09.md) §5.2 が未解決コストとして挙げる） |
| Hash キーのバケッティング | `any_hash` はサブクラスでも内容をハッシュ、`rb_any_cmp` は `String` ちょうどのときだけバイト比較 | 同じ分割: `string_digest` は無条件に内容、`string_key_eq` / `is_plain_rstring_inner` がバイト比較をクラス一致でゲート |
| `opt_str_freeze` | あり | あり — `StringFreeze` 命令（§3.5） |
| `opt_str_uminus` | あり | **無し** |
| エンコーディング | 約 100 種の本物のコーデック | native コーデックは **UTF-8, US-ASCII, ASCII-8BIT, UTF-16LE/BE, UTF-32LE/BE, ISO-8859-1..16, EUC-JP, Shift_JIS / CP932 / Windows-31J, ISO-2022-JP（`encoding_rs`）**。他は名前だけ保持（`Other` = ASCII 非互換のダミー、`NamedByte` = 約 38 の ASCII 互換コードページで、格納と文字反復は ASCII-8BIT として振る舞い `#name` / `#inspect` / ASCII 互換性だけが違う） |
| EUC-JP / Shift_JIS の文字反復 | 完全な `mbclen` | `classify` と `char_length` は native に復号するが、`CharByteIter` は**バイト単位**で進む。[`../encoding_char_iteration_design.md`](../encoding_char_iteration_design.md) が埋めようとしている穴 |
| 非 UTF-8 上の正規表現 | onigmo の多エンコーディング | バイト → `U+00XX` 代理写像で UTF-8 専用の `regex` クレートに掛ける（`regex_view` / `from_mapped_utf8`）。EUC-JP / SJIS はバイト単位の近似 |
| 宣言エンコーディング下の不正バイト | 許容、coderange BROKEN | 同じ（`content` は不透明なバイトバッファ、`ty` は情報のみ） |
| `str_mod_check` | ポインタ + 長さ | 長さのみ（同じ長さの in-place 編集でも再確保するため） |

---

## 8. テストと関連文書

| 場所 | 内容 |
|---|---|
| `tests/string_bytes.rs` | `<<` / `setbyte` のバイト・エンコーディング・code range を CRuby と突き合わせ |
| `tests/hash_string_keys.rs` | String キーの Hash 経路 |
| `tests/literal.rs` | リテラルの評価 |
| `string.rs` の `shared_string_tests` / `inline_shl_encoding_tags_are_pinned` | overlay のレイアウト、CoW の分離、タグの固定 |
| [hash.md](hash.md) | String キーの probe、`frozen_hash_key` |
| [`../inline.md`](../inline.md) | インライン生成器の契約（trial inlining、`is_simple()`） |
| [`../lir.md`](../lir.md) | `StringLenFixnum` の LIR |
| [`../gc.md`](../gc.md) | 共有 root の write barrier、`young_child_exists` |
| [`../bop_redefinition.md`](../bop_redefinition.md) | `(STRING_CLASS, "freeze")` / `"!="` の BOP |
| [`../chain_deopt.md`](../chain_deopt.md) / [`../deopt_log.md`](../deopt_log.md) | `setbyte` / `<<` が deopt でなく detach + 再試行を選ぶ背景 |
| [`../c_extention.md`](../c_extention.md) | `nul_terminated_buf_ptr` と FFI |
