# Hash の実装と最適化

Ruby プログラムは Hash を「オブジェクトのフィールド袋」として使うので、
`[]` / `[]=` / `key?` / `fetch` はアプリケーションのもっとも内側のループに
現れる。yjit-bench の erubi では実行時間の 29 %、rack で 18 %、
activerecord で 13 % が Hash 参照だった
（2026-09 の yjit-bench 調査による）。

この文書は、現在の Hash がどういう表現とどういう探索経路を持っているかを
まとめ、入れた最適化とその効果、機械語の probe の設計、次の候補を
優先順位付きで並べる。個々の API の意味論ではなく「1 回の参照に何が
起きるか」に焦点を当てる。CRuby の `ar_table` / `st_table` と実装が
どう違うかは §6 にまとめた。

`runtime_optimization/` の他の章（[Array](array.md) / [String](string.md) /
[Regexp](regexp.md)）と共通の前提は [README.md](./) にある。

---

## 1. 3 つの表現

Hash の表現は `RValue` ヘッダの型別メタデータバイト（`Metadata::ty_flags`）に
入っている。48 バイトのペイロードを表現の判別に使わないので、小さい Hash は
ペイロード全部をデータに使える。

| ビット | 意味 |
|---|---|
| 0-2 | 表現: 0..=3 = その数のペアを inline 保持、7 = boxed |
| 3 | `ruby2_keywords` フラグ |
| 4-5 | inline hash の反復深度（飽和カウンタ。boxed は `BoxedHash::iter_lev`） |
| 6 | inline hash が `compare_by_identity` |

ゼロバイト（`Header::new` の既定値）がそのまま空の inline hash として妥当で、
`dup` / `clone`（`Header::newborn`）はこのバイトを保存するので、表現はヘッダと
一緒に移動する（`sanitize_dup_flags` が表現と identity ビットだけを残し、
`ruby2_keywords` と反復深度は落とす）。

表現は「3 つ」と言っているが、identity 比較の有無を掛けると形は 4 つある:
inline（`eql?` 比較）、inline + `IDENT_BIT`、boxed の `HashContent::Map`、
boxed の `HashContent::IdentMap`。boxed 側は `Box<RubyMap<Option<Value>, Value>>`
と `Box<RubyMap<Option<IdentKey>, Value>>` という**別の型のマップ**で、
`#[repr(C, usize)]` の判別子がオフセット 0 にあるので機械語からも見分けられる。

### 1.1 inline 表現（≤ 3 ペア）

`HashBody::inline` に `(key, value)` を 3 組、セルの中に直接持つ。**ヒープ確保
ゼロ**、探索は 3 要素の線形走査。`is_inline_key`（`rvalue/hash.rs`）が許すキーは:

- **packed immediate**（Fixnum / Symbol / nil / true / false / flonum）—
  同一性がそのまま内容なので、比較はビット比較で足り、プローブ時に再計算した
  ダイジェストが挿入時のものと食い違うことがない。
- **frozen String** — String の `eql?` はエンコーディング規則込みのバイト比較で
  （再定義された `String#eql?` / `#hash` はどちらの表現でも参照しない）、frozen
  なら探索中に内容が変わらない。

リテラルのキーも `h["k"] = v` の格納キー（新しいキーのときに
`Value::frozen_hash_key` がコピーして freeze する）も frozen なので、**String キーの小さな Hash リテラルは
boxed map を作らない**。

`compare_by_identity` な inline hash は id 走査だけなので、任意のキー（可変な
ヒープオブジェクトを含む）を保持できる（`IDENT_BIT`）。

4 ペア目、inline に置けないヒープキー、デフォルト値／デフォルト proc の設定、
反復中の `delete` / `shift`（tombstone を置く場所が inline には無い）のいずれかが
来ると `promote` が boxed へ移す。移送は native の再挿入で Ruby コードは
走らず、inline の反復深度ビットは `iter_lev` に引き継がれる。逆方向は
`clear` だけで、デフォルトが無ければ boxed の格納を手放して inline に戻る
（identity ビットは保つ）。

### 1.2 boxed 表現

`BoxedHash` は `Box<RubyMap<Option<Value>, Value>>`（`compare_by_identity` なら
`Option<IdentKey>` キーの別インスタンス）とデフォルト値／デフォルト proc、
反復深度、tombstone 数を持つ。

`rubymap` は順序保持マップ（hashbrown のインデックステーブル + エントリ Vec）
で、Ruby の挿入順序保持セマンティクスをそのまま表現する。反復中の `delete` は
エントリを **tombstone**（キーを `None`）にして位置を保ち、`Option<Value>` の
niche によりキーワードがゼロなら死んだエントリ、という判定が機械語からも
できる。反復が終わった後の最初の変更操作が `compact_if_dirty` で詰める。

**Rust 側の走査は 2 種類ある（#2155）。** ステップの間に Ruby コード（ブロック、
キーの `#hash` / `#eql?`、値の `#==` / `#inspect`）が走りうる走査は
`Hashmap::walk` を使う。CRuby の `rb_hash_foreach` と同じく、反復ガードの下で
**位置**で進み、毎ステップ格納を読み直して tombstone を飛ばし、現在のペアを
temp スタックに root する。`select!` / `delete_if` などは到達したペアをその場で
`delete_at`（tombstone 化）し、反復中の `clear` は格納を手放さず全ペアを
tombstone にする（`clear_during_iteration`）ので、走査中に Hash が変えられても
解放済みの領域を読むことはない。新しいキーの追加は従来どおり RuntimeError。
`Hashmap::iter` は借用したままの走査で、ステップの間に Ruby が走らない
（か、root 済みのスナップショットを回す）場所だけで使う。

### 1.3 探索経路

```
JIT: Hash#[]  ──inline gen (hash_index)──►  hashindex_pure(vm, globals, recv, key)
                                                  │
VM:  Hash#[]  ──builtin index───────────────────►  Hashmap::index
                                                  │  （ミス時のみ default / default_proc）
                                                  ▼
                                             HashRef::get
                                        ┌─────────┴─────────┐
                                    inline 走査        boxed プローブ
                                                  ┌────────┴────────┐
                                          packed_digest      string_digest
                                          （vm 不要）        + string_key_eq（vm 不要）
                                                  └────────┬────────┘
                                                  IndexMapCore::get_index_of_prehashed*
```

要点は **prehashed probe**: packed キーと String キーは Ruby コードを一切
起動せずにダイジェストと `eql?` を決められるので、`vm` / `globals` を触らない
専用経路を通る。汎用経路（`RubyMap::hash`）とバケットが一致することは
`packed_digest` / `string_digest` のドキュメントコメントが不変条件として
書いている。

### 1.4 機械語が直接歩く部分

`Hash#size` / `#__key_at` / `#__value_at` / `#__entry_count` / `#__live_at` /
`#__get_or_key` / `#default` / `#default=` / `#compare_by_identity?` は JIT が
表現を直接歩く機械語として出る。焼き込むオフセットは手計算せず
`HASH_INLINE_PAIRS_OFFSET` などのレイアウト定数（`offset_of!` 由来）と
`rubymap::EntriesLayout` のプローブから取る。`Bucket<K, V>` は `repr(Rust)` で
キー型の niche によってフィールドが並び替わる（`Option<Value>` では
value+0 / hash+8 / key+16）ため、`entries_layout()` は本物の `Vec` を作って
実測し、`rubymap` 側の `raw_probe` / `layout_matches_the_safe_api` テストが
その値で歩いた結果が安全な API と一致することを固定している。

3 ペア以下の Hash リテラルは、**キーが全部 packed immediate で互いに異なる**
場合に限り JIT がセルを bump 確保してヘッダとペアを直接書き込む
（`new_hash_inline` → `emit_alloc_cell`）。frozen String キーは inline 表現には
置けるが、この bump 確保の対象ではなく、`gen_hash` の実行時呼び出しを経て
inline 表現に落ち着く。

`gen_hash`（`codegen/runtime.rs`、VM と JIT の両方が呼ぶ）は、Rust のスタック上の
`HashmapInner` にペアを入れ、最後に Hash にする。Rust のスタックは GC が見ないので、
これが成り立つのは Ruby のコードが動かない間だけである（リテラル自身のキーと値は
フレームのスロットにあるが、unfrozen な String キーの凍結コピーはこのマップしか
持っていない）。そこでキーごとに `HashmapInner::aset_native_key` で入れる。packed な
値とクラスが `String` そのものの String は、`#hash` も `#eql?` も呼ばずに入る。
それ以外のキーは何も入れずに `false` を返し、`gen_hash_rooted` がそこまでのマップを
Hash にして temp stack に置いたうえで、残りのペアを `Hashmap::aset_pairs` で入れる
（#2153）。前者の経路は構築中のマップ専用なので、tombstone と走査中の検査を省いた
`store` の `FRESH` 版を使う。

`Hash#each` / `each_key` / `each_value` はこれらのプリミティブの上に Ruby で
書かれている（`builtins/hash.rb`）ので、ホットな呼び出しサイトではメソッドと
ブロックの両方がインライン展開される（§1.6）。

### 1.5 ハッシュ値とダイジェスト

Hash がキーについて知っているのは**ハッシュ値**（`#hash` が返す整数）と `eql?`
だけで、マップはどのキーも「ハッシュ値をもう 1 回 fold した**ダイジェスト**」の
下に置く。

| 段 | 実装 | 場所 |
|---|---|---|
| ハッシュ値（Ruby から見える `#hash` の値そのもの） | `Value::hash_value`。組み込みの型は seeded な wyhash 系 multiply-fold（`RubyHasher`、施策 5）で native に計算、それ以外は `#hash` を dispatch | `value.rs` |
| ダイジェスト（バケッティング） | `rubymap::digest_of_hash_value(hv) = fold(hv, P2)`。全キー共通で、マップごとのハッシャは通らない（`RubyHash::ruby_digest`） | `rubymap/src/hasher.rs` |

以前は 2 つが別の関数だった（`#hash` の値は std の SipHash-1-3、バケッティングは
ミキサーにキーのビットやバイト列を直接流す）。そのため「`#hash` が別のオブジェクトの
ハッシュ値を返すキー」— delegator や String / Integer のラッパー — が、相手と
同じバケットに入らず、`eql?` が呼ばれる前に外れていた（#2204）。CRuby の
`any_hash` は両方に同じ関数を使うので、そこに揃えた。Fixnum / Float キーが
内側で SipHash を 1 回回していた二重ハッシュもこれで消えている。

`RubyHasher` は長さを最初に混ぜ、16 バイト単位のループの後に重なりを許した
4 / 8 バイトの末尾読みをして最後にもう 1 回 fold する。シードはプロセスで 1 回
`RandomState` から引く（`OnceLock<u64>`）ので、全マップと全 JIT 呼び出しサイトが
同じシードを共有する。ハッシュ品質は `hasher.rs` のテストが固定している:
長さごとの分離、1 ビットの変化が**上位 7 ビット**（hashbrown の control byte）
まで拡散すること、等間隔の整数キーが散ること。

`Value::hash_value` の内訳: immediate は tagged bits を 1 回 fold
（`packed_hash_value`。`0.0` だけは heap の `-0.0` と `eql?` なので Float の
値ハッシュ）、Bignum / Float は値、String は生バイト列（クラスによらず、
エンコーディング非依存、再定義された `String#hash` は見ない）、Array / Hash は
組み込みの `hash` が再定義されていない限り native の構造ハッシュ（葉だけの
Array — `[id, :name]` のような複合キー — は再帰ガード無し）、`Kernel#hash` を
継ぐオブジェクトは identity、それ以外は `#hash` を dispatch して `#to_int` で
整数化する。`Integer#hash` / `Float#hash` / `String#hash` / `Array#hash` /
`Hash#hash` / `Kernel#hash` はこの同じ関数の値を返す。Struct / Set / Time /
Regexp などの `#hash` は部品を自分で混ぜるメソッドで（`seeded_hasher()` =
SipHash）、Hash からは dispatch される側。

#### キーを尋ねる probe と尋ねない probe

`#hash` / `eql?` は Ruby のコードでありうるので、Hash を書き換えるかも
しれない（#2199: 格納の途中で `eql?` が `clear` すると、probe が解放済みの
領域に残って落ちていた）。probe は 2 通りある（`rvalue/hash.rs`）:

- **尋ねない**（呼び出しゼロ）: immediate（`packed_digest`）、クラスが `String`
  ちょうどの String（`string_digest` / `string_key_eq`）、Bignum・0 以外の
  Float・`Kernel#hash` と `Kernel#eql?` の両方を継ぐオブジェクト
  （`unasked_digest` / `unasked_key_eq`、後者の判定は
  `Store::is_identity_key_class` が class version でキャッシュ）。prehashed な
  probe 1 回で済む。
- **尋ねる**（`is_observed_key`）: String のサブクラス、Array、Hash、Struct、
  `hash` か `eql?` を自前で持つオブジェクト、`-0.0`。`HashRef::pos_with_hash`
  が probe を分解する。先に `#hash` を呼び（テーブルには何も触れていない）、
  そのダイジェストの下にあるペアを比較なしで集め（`RubyMap::each_filed_under`）、
  temp stack に root してから probe 順に `eql?` を尋ね、受理されたキーの位置を
  その時点のテーブルから読み直す。格納（`store_observed`）と削除は、こうして
  得た「以後 Ruby が走っていない位置」に対して位置指定で行う。CRuby の st.c は
  同じ問題を `rebuilds_num` を見たやり直しで解いている。

異なるプロセスで `#hash` が変わること（CVE-2011-4815 対策）は
`tests/hash_seed.rs` が本物のバイナリを 2 回起動して確認している。

### 1.6 Ruby で書かれた走査と、そのためのプリミティブ

`builtins/hash.rb` に `each` 系が Ruby で書かれている理由は、JIT が
インライン展開するのが `FuncKind::ISeq` の呼び出し先だけだからである。
Rust の builtin が要素ごとにブロックを起動すると 1 要素ごとにフルの
ブロック呼び出しを払うが、Ruby の `each` なら `h.each { .. }` のホットな
サイトでメソッド本体とブロックの両方が `yield` の位置に展開される。
ブロック引数を `&block` で受けると `BlockArg` バイトコードになって
メソッド全体が特殊化不能になり、フレームもヒープに移る
（`Iseq::has_block_arg`）ので、必ず `yield` で書く。

そのために Rust 側が出しているプリミティブ（大半に機械語インライナがある）:

| プリミティブ | 役割 |
|---|---|
| `__entry_count` | エントリ配列の生の長さ（tombstone 込み） |
| `__live_at(i)` | 位置 i が生きているか |
| `__key_at(i)` / `__value_at(i)` | O(1) の位置参照。範囲外・tombstone は nil（エラー経路が無いので `while` ループに境界機構が要らない） |
| `__set_value_at(i, v)` | 位置指定の値上書き。ダイジェストもプローブも走らない |
| `__iter_begin` / `__iter_end(g)` | 反復参照の取得・返却（inline の深度は飽和するので戻り値を必ず返す） |
| `__dup_table` | エントリと identity モードだけを複製。デフォルトは持たず、常に素の `Hash`（CRuby の `hash_dup_with_compare_by_id`） |
| `__new_hash_with_capacity(n)` | 容量指定の空 Hash |
| `__get_or_key(k)` | `key?(k) ? self[k] : k` を 1 プローブで（CRuby の `rb_hash_lookup2(map, k, k)`）。デフォルトは見ない |
| `__pairs` | `[[k, v], ...]` のスナップショット |
| `__block_splits_pair?` | 呼び出し元ブロックの形（`|k, v|` か `|pair|` か）を Proc を作らずフレームから読む |

この上に乗っている走査の要点:

- `each` / `each_pair` / `each_key` / `each_value` は
  `guard = __iter_begin; ... while i < __entry_count ... if __live_at(i) ...
  ensure __iter_end(guard)` の形。
- `transform_values` は `__dup_table` + `__set_value_at` で、**キーは変わらない
  ので一度も再ハッシュもプローブもしない**。切り離したコピーを歩くので
  ガードも不要で、全位置が生きている。`transform_values!` は同じ位置上書きを
  `self` に対してガード付きで行う。
- `transform_keys` はブロック無し `transform_keys(map)` にペアごとのブロック
  判定が乗らないよう 3 本のループに分け、`__get_or_key` を使う。
- `to_h` にブロックが付いたときは `each` を経由せず位置を直接歩き、
  `[k, v]` 配列を作らない。結果は `__new_hash_with_capacity` で事前確保。
- `map` / `collect` は `method(:each).owner == ::Hash` を確認したうえで
  `__block_splits_pair?` をループの外で 1 回だけ判定する（`|*vs|` の rest
  引数は specialized-yield の対象外になるため）。
- `dig` を Rust に移した実験は計測で退行したので Ruby に戻してある
  （`hash.rb` のコメントに記録）。

---

## 2. 入れた最適化

効果は同じ計測機で交互ラウンドの中央値の最小値で比較している（単発は ±10 %
揺れる）。「純ルックアップ」は同じループからルックアップだけ抜いた時間を
差し引いた値。

| # | 施策 | 変更箇所 | 効果 |
|---|---|---|---|
| 1 | ≤3 ペアの packed キー Hash を inline 表現に（ヒープ確保ゼロ、JIT がセルを直接書く） | `rvalue/hash.rs`、JIT の literal 経路 | Symbol キーリテラル生成が boxed の 1/2 以下 |
| 2 | 定数 Hash リテラルをテンプレート化し、評価ごとに複製（#1232） | `bytecodegen/expression.rs::from_literal_pairs` | 評価ごとの挿入ループが消える |
| 3 | 汎用 `[]=`（VM と JIT の多相残余）に Hash の直接経路（#1245） | `codegen/runtime.rs::set_index` | rack −8 %、graphql −13 %、activerecord −8 %（他の 3 施策込み） |
| 4 | frozen String キーを inline 表現に許可（#1246） | `rvalue/hash.rs::is_inline_key` | `{"content-type" => "text/plain"}` 生成 134 → 57 ns（YJIT 60）、`h["k"]` 50 → 23 ns、`h["k"] = v` 40 → 22 ns。rack −7 %、activerecord −13.5 % |
| 5 | バケット用ハッシュを SipHash-1-3 から seeded ミキサーへ | `rubymap/src/hasher.rs` | 18 エントリの純ルックアップで 20〜34 % 減。erubi −12.4 %、graphql −4.1 % |
| 6 | `#hash` の値そのものを同じミキサーから作る（#2204、§1.5） | `value.rs::hash_value` | Integer / Float キーの内側の SipHash が消え、`h[int]` 335 → 160 命令、`[1, :a, "s"].hash` 1152 → 311 命令 |
| 7 | `Hash#[]` の probe を機械語で出す（§3） | `AsmInst::HashProbe` | 18 エントリ String キーの hit 約 12 ns、miss 約 8 ns。erubi −7〜−10 % |

施策 5 の背景: `RubyMap` は std の `RandomState`（SipHash-1-3）を既定の
ハッシャに継いでいて、`perf` で見るとルックアップ 1 回の **1/3** がそこだった。
CRuby の `st_hash` は seeded な非暗号ミキサーなので、同じトレードを取った。
プロセス単位の乱数シード（`RandomState` から 1 度引くので新規依存なし）+
wyhash 系の multiply-fold。シードを秘匿することが hash-flooding を抑える根拠で、
混ぜ方自体は乗算 2 回。20 万エントリで 4096 刻みの整数キー（弱いミキサーが
破綻する典型ケース）でも悪化しない。

### 純ルックアップのコスト（1 回、ns）

| | monoruby | CRuby+YJIT |
|---|---:|---:|
| String 6 エントリ hit（先頭 / 末尾） | 10.7–11.1 / 14.4–14.6 | 33.1 / 34.6 |
| String 6 エントリ miss | 10.9 | 27.9 |
| String 18 エントリ（erubi 形）hit | 11.7–12.3 | 26.4–40.5 |
| String 18 エントリ miss | 8.1–8.3 | 18.8 |
| String 100 エントリ hit / miss | 11.6–12.1 / 8.0–10.7 | 25.3 / 19.0 |
| Symbol 18 / 100 / 1000 エントリ hit | 7.5–8.6 | 10.6 |
| Symbol 18 / 100 / 1000 エントリ miss | 8.8–11.2 | 11.2 |

String の hit は digest 呼び出し（wyhash）と eq 呼び出し（memcmp）が残るので
Symbol ほどは縮まないが、miss は in-line で nil を返せる。Integer キーは probe の
対象外で（§4.1）、`hashindex_pure` → `Hashmap::index` → `HashRef::get` →
`IndexMapCore` の 4 段を通る。

---

## 3. 機械語の probe

受信側が `Hash` ちょうどで boxed 表現だと分かっている `Hash#[]` の呼び出し
サイトでは、ダイジェスト → インデックステーブル引き → エントリ比較を機械語で
出す（`AsmInst::HashProbe`）。必要なレイアウト知識は `rubymap::EntriesLayout`
から取る。

### 3.1 対象

`IndexMapCore` には 2 つの動作領域がある。**≤ 8 エントリ**（`AR_MAX`、CRuby の
`RHASH_AR_TABLE_MAX_SIZE` と同じ）は `linear`、`indices` を作らず `entries` を
線形走査して `entry.hash == hash` の一致だけキー比較する。それ以上は hashbrown
の probe（上位 7 ビットで control byte を 16 個ずつ比較 → 添字 → エントリ比較）。
`Bucket { hash, key, value }` はハッシュ値を格納しているので、線形走査は
「64 ビット比較 × N、当たったときだけキー比較」で済む。

| キー | ダイジェスト | 領域 |
|---|---|---|
| packed のうちビットがそのままミキサーに入るもの（Symbol / nil / true / false） | 葉ヘルパ `packed_digest_c` | 線形・索引の両方 |
| クラスがちょうど `String` の文字列 | 葉ヘルパ `string_digest_c` + `string_key_eq_c` | 線形・索引の両方 |
| それ以外（Integer を含む） | probe を出さず `hashindex_pure` を直接呼ぶ | — |

### 3.2 設計判断

- **ダイジェストは葉ヘルパ、probe は機械語。** fold は 64×64→128 の乗算だが、
  x86-64 のアセンブラ（monoasm）に 1 オペランド `mul` が**無い**（`imul` は
  2 オペランドの下位 64 ビットのみ）。aarch64 には `umulh` がある。monoasm に
  足したらヘルパをその命令列に置き換えるだけで、他は変わらない。
- **形の不一致は deopt ではなく builtin 呼び出し。** inline 表現・identity
  マップ・索引領域を扱えない構成はすべて `hashindex_pure` への合流で、exit
  しない。この deopt は再コンパイルを起こさないので、deopt にすると大きな
  Hash のサイトが毎回 deopt することになる。
- **線形領域の miss は in-line で `nil`。** 走査を尽くしたら
  `Option<Box<HashDefault>>`（null = default 値も proc も無し）を 1 ロード見て、
  無ければ nil。builtin に落とすとダイジェストと走査をもう一度やり直す。
- **索引領域は hashbrown の `find_inner` を SIMD 無しで写す。** monoasm には
  SIMD が無いので、16 バイトの control group を 64 ビット語 2 つとして読み、
  hashbrown 自身の generic backend と同じ SWAR ゼロバイト判定
  （`(x - 0x01..) & !x & 0x80..`、x = word ^ tag×0x01..）で候補ビットを出す。
  真の一致より後ろのバイトに偽陽性が出うるが、候補は必ずエントリの格納
  ダイジェスト（64 ビット全部）で検証するので無害。候補の添字は control
  バイト列の**下**にあるバケット配列（bucket i は `ctrl - 8(i+1)`）から
  読む。2 語とも見終わってから EMPTY（`0xFF` — bit 7 と 6 が立つので
  `w & (w << 1) & 0x80..`）の有無で打ち切り、無ければ三角数列で次の group
  へ。**EMPTY を見た瞬間に打ち切ってはいけない**：削除で EMPTY に戻った
  スロットより後ろに、それ以前に挿入された要素が同じ group 内に居られる
  （hashbrown は group 内の一致を全部見てから EMPTY を判定する）。
  `rubymap` の `raw_probe` テストがこのアルゴリズムを生の offset だけで
  なぞり、`find_inner` と同じ答えになることを固定している。
- **レイアウトは実測と `offset_of!` で取る。** `Bucket` は niche を持つキー型で
  並び替わる（`Option<Value>` では value+0 / hash+8 / key+16）ので、
  `EntriesLayout` の `hash_offset` / `linear_offset` は実測で取る。
  `indices_ctrl_offset` / `indices_mask_offset` / `group_width` は vendored
  hashbrown の `RawTable` / `HashTable` に生やした `ctrl_offset()` /
  `bucket_mask_offset()` / `GROUP_WIDTH` から取る。
- **String キーの eq。** `string_key_eq_c` は `string_key_eq` そのもの
  （identity → STRING×STRING バイト比較）。同一オブジェクト（frozen リテラル）
  なら eq 呼び出しを省く。**格納ダイジェストが一致したのに eq が偽**なら
  （64 ビット衝突か tombstone か異種キー）`miss` → builtin。こうすると eq
  呼び出しの前後で probe のループ状態を退避する必要が無く、rdx/rcx と entry
  ポインタだけ push すれば済む。
- **サブクラスは class guard が弾く。** `guard_class(STRING_CLASS)` はクラス
  一致なので、`eql?` を dispatch すべき String サブクラス（#1258）は probe に
  入らず deopt する。
- **identity マップは probe しない。** `HashContent` の判別子（0 = Map,
  1 = IdentMap）を見て IdentMap なら builtin へ。String の内容ダイジェストは
  identity ダイジェストと違うので、probe すると自分自身すら見つからない。
- **キークラスが混在するサイト。** `h[k]` の k のクラスは抽象状態から取り、
  guard の失敗は plain deopt（再コンパイル無し）。同じサイトに String と Symbol
  が交互に来ると、後から来たクラスは毎回 deopt して interpreter で実行される。
  receiver 側の `BecamePolymorphic` 再コンパイルは VM が立てる POLY ビットで
  「次は generic に」と判断できるが、引数クラスにはその仕組みが無い。

### 3.3 どこから発火し、いつ機械語を諦めるか

- `Hash#[]` のインライン生成 `hash_index` は `fire_index_inline` から呼ばれ、
  受信側クラスが**ちょうど `Hash`** のときだけ probe を出す。理由は builtin
  の `Hash#[]` がミス時に `default` **メソッド**を dispatch する（サブクラスや
  特異メソッドで上書きできる）のに対し、`hashindex_pure` は格納されたデフォルトを
  直接読むからで、サブクラスは通常の class-version ガード付き呼び出しに残す。
  `Hash#[]` は `BASIC_OP_DEFS` に入っているので class-version ガードは省き、
  `record_bop_dep` で再定義を捕まえる。`Hash#[]=` は意図的に BOP に**入って
  いない**ので、`hash_index_assign` は通常経路のまま（`index_hash_assign_redefinition`
  テストが固定）。
- 受信側クラスを抽象状態が確定できない多相サイトでは `index_dispatch` が
  **2 腕のディスパッチ**を出す: `BrClassNe` で Hash なら probe、そうでなければ
  `runtime::get_index`。外れたクラスは deopt ではなく C 呼び出しになる。
- **ヘルパは Ruby を実行しない（#2156）。** JIT コードから直接呼ぶヘルパは
  フレームを持たない。そこで Ruby コードが走ると、(1) メソッド・定数・基本
  演算の再定義や ivar の型変化がヘルパより後ろのコード（ガード済みの前提）に
  伝わらず、(2) chain deopt がそのフレームを変換できない。そこで
  `hashindex_pure` が答えるのは、探索が Ruby を走らせないと分かっている場合
  だけである: キーが packed 値か、クラスがちょうど `String` の文字列か、
  受信側が identity Hash のとき。ミス時は default proc が無ければ格納された
  デフォルトを返す。それ以外（`#hash` / `#eql?` を dispatch するキー、default
  proc の実行）では 0 を返し、JIT コードは deopt して、インタプリタがフレーム
  付きで builtin を呼ぶ。deopt を繰り返すサイトは
  `RecompileReason::ImpureHelper` で印が付き、再コンパイル後は通常のメソッド
  呼び出しになる。`Hash#[]=`（`hashindex_assign_pure`）、`Hash#__get_or_key`
  （`hashgetorkey_pure`）、`Hash#include?`（`hashinclude_pure`、#2147）も
  同じ規則に従う。
- `hash_entries_layout()` が `None`（2 つのマップ型でレイアウトが一致しない
  構成）か、hashbrown の group 幅が 16 でない構成では、索引領域は静的に
  `miss` へ飛ばす。いずれも性能上の後退で、正しさには関わらない。

---

## 4. 今後のアイデア（効果の見込み順）

### 4.1 Fixnum キーを probe に載せる（小）

Integer キーの `#hash` の値はミキサー 1 回で作れる（§1.5）ので、Fixnum も
`packed_digest_c` 相当の葉ヘルパ 1 回で §3 の線形・索引 probe に載せられる。
未着手。

### 4.2 呼び出しサイトのキーセット・インラインキャッシュ（中・要検証）

erubi の 322 個の spec Hash は **すべて同じ 18 個のキーを同じ順序で持つ**
（同じ JSON 形状から作られる）。`spec["name"]` のような「リテラルキー ×
同形状の Hash」という組み合わせは、テンプレートエンジンや JSON 処理では
支配的なパターンである。呼び出しサイトに「このキーセットならインデックスは
これ」を憶えさせられれば、probe そのものを飛ばせる。CRuby のオブジェクト
shape に相当する仕組みを Hash に持ち込むことになるので、キーセットの同一性を
安く判定する仕掛け（挿入順の版番号など）の設計が要る。

### 4.3 ディスパッチ段数を減らす（小〜中）

probe の対象外のキーと builtin への合流は `hashindex_pure` → `Hashmap::index` →
`HashRef::get` → `IndexMapCore` の 4 段で、それぞれが `vm` / `globals` を
引き回している。`Hashmap::index` はミス時の default 処理のためだけに 1 段あり、
`HashRef::get` は表現の分岐をしている。表現の分岐を呼び出し側に持ち上げて
boxed 専用の入口を用意すれば数 ns 縮む見込み。

### 4.4 frozen String にダイジェストをキャッシュ（小）

ハッシュ計算がルックアップの 1 割を切った今は上積みが小さい。長いキー
（パスや URL）を多用するコードでは効くので、`RStringInner` に 4 バイトの
空きができたときの候補として残す。frozen なら無効化が不要という性質は
変わらない。

### 4.5 inline 表現の拡張（要検討）

48 バイトのペイロードにペア 3 組でちょうど埋まっている。4 組以上にするには
`RValue` を大きくするかキー・値を別配列にするかで、どちらも Hash 以外の
すべてのオブジェクトに影響する。erubi の Hash は 18 ペアなので、この方向で
救えるワークロードは限られる。

### 4.6 RValue の中身のアロケータ（別件だが Hash に効く）

boxed Hash の実体（インデックステーブルとエントリ Vec）は glibc malloc
経由で、activerecord では malloc/free 系だけで 15 % を占める。`mimalloc`
フィーチャの既定化 A/B は Hash に限らない話だが、Hash の生成・成長が多い
ワークロードにはここが効く。

---

## 5. `==` / `eql?` / `hash` の意味論

- `Hash#==` / `#eql?` はまず同一性で短絡する（`h = {x: Float::NAN}; h == h` が
  true になる）。空でない Hash 同士では identity モードが違えば偽。再帰構造は
  `exec_recursive_paired` で扱う。
- `Hash#hash` はペアごとに `kpart ^ vpart` を作って順序非依存に足し合わせ、
  サイズを混ぜてから `from_hash_digest` で Fixnum 範囲に畳む。キーと値の
  `#hash` は本物の dispatch で、`rb_exec_recursive_outer` 相当の再帰保護が付く。
- 内部の `HashRef::eql`（Hash 自身がキーになったときのキー比較）は別物で、
  モードが違えば偽を返す。
- `Hash#rehash` は Ruby で `to_a; clear; 再挿入` と書かれていて、C レベルの
  テーブル再構築ではない。
- 値や鍵を**探す**メソッド（`assoc` / `rassoc` / `key` / `value?`、
  `Enumerable#include?` / `find_index`、Array の線形探索）は `eql?` でなく
  `rb_equal`、つまり「同一オブジェクトなら `==` を呼ばずに真」で比べる
  （#2206）。`Kernel#__rb_equal` が Ruby 実装用のその入口（1 要素ごとに
  ネイティブ呼び出しになるので、`Hash#value?` は Ruby をやめて `key` と
  同じ `walk` の Rust 実装にした）。レシーバは
  CRuby と同じ側で、`assoc` / `rassoc` は**引数**、`key` / `value?` と
  `Enumerable` の探索は**格納されている側**。
- `Hash#assoc` は、ペアが ar テーブルに収まる数（8）を超えていて identity
  テーブルでもなければ、まず鍵をハッシュ値で引く（引数の `#hash` を 1 回、
  同じ値で綴じられたペアを `==` で比較）。見つかった場合に返すのは
  `[引数, 格納値]`（`rb_assoc_new(key, value)`）。外れたときだけ線形走査する。
- `Set#include?` / `member?` / `===` / `disjoint?` / `intersect?` は、Set が
  空でも要素の `#hash` を呼ぶ（#2207、CRuby 4.0 の Set はハッシュテーブル
  そのものなので）。空の Hash のルックアップが何も呼ばないのとは逆
  （`ar_lookup`、#2127）。identity Set はどちらも呼ばない。

---

## 6. CRuby との実装差異

| 項目 | CRuby | monoruby |
|---|---|---|
| 表現の段階 | `ar_table`（≤ 8、線形、ハッシュ値格納）→ `st_table` | **≤ 3 ペアは RValue のペイロードに直置き（ヒープ確保ゼロ）** → boxed の線形領域（≤ 8、`AR_MAX`、CRuby と同じ閾値）→ hashbrown の索引領域。線形→索引はエントリ `Vec` を共有したまま index table を作るだけで、`ar`→`st` のような作り直しは無い |
| 挿入順序 | `st_table` のエントリ配列 | `rubymap`（IndexMap 系）: エントリ `Vec` + 位置の index table。観測される順序は同じ |
| 反復中の変更 | 追加は例外、`delete` は許可（`RHASH_ITER_LEV`） | 同じ規則（既存キーの更新は許可）。機構は tombstone + `dead` カウンタ + `compact_if_dirty`。inline hash は tombstone を置く前に boxed へ昇格する。深度は inline が 2 ビット飽和、boxed が `Cell<u32>` |
| 走査の実装 | `rb_hash_foreach`（C のコールバックループ） | `each` は Ruby の位置ループ（§1.6）。JIT がブロックごと展開するため |
| String キーの複製 | `rb_hash_key_str` が fstring テーブルに**インターン**（Hash 間で共有） | 同じ。`Value::frozen_hash_key` が内容の fstring を引き（`Store::fstring`）、無ければ frozen コピーを作って弱いテーブルに登録する（string.md §3.1）。特異クラスやインスタンス変数を持つキーは素の frozen コピー。コピーするのは CRuby（`hash_aset_str`）と同じく**新しいキーのときだけ**で、既存キーへの格納は格納済みのキーを残して何も確保しない（`HashRefMut::aset`） |
| String キーの比較・ハッシュ | 内容のみ、`String#hash` は dispatch しない。クラスが `String` ちょうどのときだけバイト比較で短絡（`rb_any_cmp` / `any_hash`） | 同じ規則（`Value::eql`、`string_key_eq`）。ただしバイト比較は `RStringInner::eq` で**エンコーディングを見ない**。CRuby の `rb_str_hash` は非 ASCII 文字列でエンコーディング index を混ぜる |
| `compare_by_identity` | `st_table` の型を `identhash` に切り替えて再ハッシュ | inline は `IDENT_BIT` を立てるだけ（packed キーは `eql?` と同一性が一致するので inline のまま）。boxed は**別型**の `RubyMap<Option<IdentKey>, Value>` に作り直す。空マップ限定で双方向に切り替える `set_compare_by_identity_empty` があり、`Hash#replace` / `Set` が使う |
| デフォルト値／proc | `ifnone` スロット | `Option<Box<HashDefault>>`。null なら「どちらも無し」を 1 ロードで判定でき、JIT のミス経路が in-line で nil を返せる。デフォルトを持つと boxed 強制（`Hash.new(7)` は確保する）。`clear` はデフォルトを保つ（CRuby と同じ） |
| ミス時の `default` | `rb_funcall(id_default)` | builtin の `Hash#[]` は同じく dispatch。`hashindex_pure` / `runtime::get_index` は格納値を直接読むので、対象を `Hash` ちょうどに限定 |
| Ruby から見える `#hash` | `rb_hash_start` 系（SipHash-1-3、seeded）。`any_hash` がバケッティングにも同じ値を使う | Hash が native に計算する型は seeded ミキサー（`Value::hash_value`）で、バケッティングはその値の fold（§1.5）。Struct / Set / Time などのメソッドは `HASH_STATE`（SipHash-1-3、seeded） |
| String ハッシュのキャッシュ | 無し | 無し（§4.4 に候補として残す） |
| GC | `rb_gc_mark` | `HashRef::mark` がデフォルトと生きたエントリを mark。`young_child_exists` が世代別の remember 判定、mutator と JIT の `emit_hash_default_assign` が write barrier を出す（[`../gc.md`](../gc.md)） |

---

## 7. テストと計測の所在

| 場所 | 何を固定しているか |
|---|---|
| `monoruby/tests/hash_probe_jit.rs` | 機械語 probe: hit / miss / default 値 / default proc / nil・true・false キー / tombstone / 索引・inline・identity への委譲（**call であって exit ではない**）/ クラスガード / 線形↔索引の成長 / String キー（線形・索引・frozen 同一）/ サブクラスキー / String → Symbol のクラス変化 |
| `monoruby/tests/hash_string_keys.rs` | boxed と inline の String キー経路: 往復、混在、サブクラス、エンコーディング（バイト比較）、tombstone、`compare_by_identity`、`string_subclass_key_dispatches_eql` |
| `monoruby/tests/hash_compare_by_identity.rs` | identity hash は String キーを複製しない、通常 hash は複製して freeze する |
| `monoruby/tests/hash_seed.rs` | `#hash` がプロセス間で変わり、プロセス内で安定 |
| `rubymap/src/lib.rs`（`raw_probe`, `layout_matches_the_safe_api`, `niche_carrying_keys_reorder_the_bucket`） | JIT が使う生オフセットでの探索が `find_inner` と同じ答えになること |
| `rubymap/src/hasher.rs` のテスト | ミキサーの品質（長さ分離、上位 7 ビットへの拡散、等間隔キー） |
| `builtins/hash.rs` の `jit_layout_matches_entry_at*` | monoruby 側のレイアウト定数が `entry_at` と一致 |
| `benchmark/jit_hash.yaml` | 1000 エントリ Hash の反復・変換ベンチ |

---

## 8. 計測手順（再現用）

```sh
# ビルド
cargo build --release
cargo build --release --features perf --target-dir target-perf   # perf 用シンボル

# 純ルックアップのマイクロ（baseline を引く）
target/release/monoruby bench_hash.rb

# 内訳
perf record -F 999 -e cpu-clock -g --call-graph=fp -o h.data -- \
  target-perf/release/monoruby h_str.rb
perf report -i h.data --stdio --no-children --sort symbol -g none

# ベンチ（交互ラウンド。単発は信用しない）
cd ../yjit-bench && export LANG=C.UTF-8
MAX_TIME=25 RESULT_JSON_PATH=out.json \
  /path/to/monoruby -Iharness-warmup benchmarks/erubi/benchmark.rb
```

マイクロは必ず「同じループからルックアップを抜いたもの」を baseline として
測り、その差を見る。monoruby と CRuby ではループ自体のコストが 10 倍違う
（2.5 ns 対 23 ns）ので、生の時間を並べると比較にならない。
