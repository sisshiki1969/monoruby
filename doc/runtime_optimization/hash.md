# Hash の実装と最適化

Ruby プログラムは Hash を「オブジェクトのフィールド袋」として使うので、
`[]` / `[]=` / `key?` / `fetch` はアプリケーションのもっとも内側のループに
現れる。yjit-bench の erubi では実行時間の 29 %、rack で 18 %、
activerecord で 13 % が Hash 参照だった
（[`yjit_bench_slow_investigation_2026-09.md`](../yjit_bench_slow_investigation_2026-09.md) §5.2）。

この文書は、現在の Hash がどういう表現とどういう探索経路を持っているかを
まとめ、そこに入れた最適化を計測とともに記録し、残っているコストと次の
候補を優先順位付きで並べる。個々の API の意味論ではなく「1 回の参照に何が
起きるか」に焦点を当てる。CRuby の `ar_table` / `st_table` と実装が
どう違うかは §6 にまとめた。

`runtime_optimization/` の他の章（[Array](array.md) / [String](string.md) /
[Regexp](regexp.md)）と共通の前提は [README.md](README.md) にある。

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

リテラルのキーも `h["k"] = v` の格納キー（`Value::frozen_hash_key` が
コピーして freeze する）も frozen なので、**String キーの小さな Hash リテラルは
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

### 1.3 探索経路

```
JIT: Hash#[]  ──inline gen (hash_index)──►  hashindex(vm, globals, recv, key)
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

`Hash#each` / `each_key` / `each_value` はこれらのプリミティブの上に Ruby で
書かれている（`builtins/hash.rb`）ので、ホットな呼び出しサイトではメソッドと
ブロックの両方がインライン展開される（§1.6）。

### 1.5 2 つのハッシュ関数

Hash には**用途の違う 2 つのハッシュ関数**が共存していて、これが §4.1 の前提に
なる。

| 用途 | 実装 | 場所 |
|---|---|---|
| バケッティング（マップ内部のダイジェスト） | seeded な wyhash 系 multiply-fold（`RubyHasher`、施策 5） | `rubymap/src/hasher.rs` |
| Ruby から見える `Object#hash` の**値** | std の `RandomState` = SipHash-1-3（`HASH_STATE`、`seeded_hasher()`） | `value.rs` |

`RubyHasher` は長さを最初に混ぜ、16 バイト単位のループの後に重なりを許した
4 / 8 バイトの末尾読みをして最後にもう 1 回 fold する。シードはプロセスで 1 回
`RandomState` から引く（`OnceLock<u64>`）ので、全マップと全 JIT 呼び出しサイトが
同じシードを共有する。ハッシュ品質は `hasher.rs` のテストが固定している:
長さごとの分離、1 ビットの変化が**上位 7 ビット**（hashbrown の control byte）
まで拡散すること、等間隔の整数キーが散ること。

Ruby 側の `#hash` は `Value::ruby_hash` / `ruby_hash_packed` が作る。nil /
true / false / Symbol はビットそのもの、String は `RStringInner::hash`（生
バイト列、エンコーディング非依存、再定義された `String#hash` は見ない）、
Array / Hash は組み込みの `hash` が再定義されていない限り native の構造
ハッシュ、それ以外は `#hash` を dispatch して `#to_int` で整数化する。
**Fixnum / Float だけは内側で SipHash を丸ごと 1 回回して** `Integer#hash` /
`Float#hash` の値を作り、その結果をマップ側のハッシャに流す。これは
`Array#hash` が要素を混ぜるときに Ruby レベルの `#hash` と一致させるためで、
バケッティングには要らない二重ハッシュである（§4.1）。

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
| 5 | バケット用ハッシュを SipHash-1-3 から seeded ミキサーへ（本ブランチ） | `rubymap/src/hasher.rs` | 下表。erubi −12.4 %、graphql −4.1 % |

施策 5 の背景: `RubyMap` は std の `RandomState`（SipHash-1-3）を既定の
ハッシャに継いでいて、`perf` で見るとルックアップ 1 回の **1/3** がそこだった
（String キーで `string_digest` 18.7 % + `SipHasher13::write` 13.9 %、Symbol キーで
`packed_digest` 24.6 % + 11.7 %）。CRuby の `st_hash` は seeded な非暗号ミキサー
なので、同じトレードを取った。プロセス単位の乱数シード（`RandomState` から
1 度引くので新規依存なし）+ wyhash 系の multiply-fold。シードを秘匿することが
hash-flooding を抑える根拠で、混ぜ方自体は乗算 2 回。Ruby から見える
`Object#hash` は従来どおり `HASH_STATE` 側なので変わらない。

### 純ルックアップのコスト（1 回、ns。18 エントリの Hash）

| キー | 施策 4 まで | 施策 5 後 | CRuby 4.0.6 + YJIT |
|---|---:|---:|---:|
| String 4 バイト | 42.8 | **33.7** | 17.5 |
| String 19 バイト | 49.1 | **37.7** | 18.5 |
| String ミス | 42.0 | **30.7** | 16.7 |
| Symbol | 39.0 | **25.7** | 16.1 |
| Integer | 52.1 | **39.5** | 16.4 |

ハッシュ品質のサニティチェック: 20 万エントリで 4096 刻みの整数キー（弱い
ミキサーが破綻する典型ケース）は 94.2 → 88.8 ns と悪化しない。

---

## 3. 残っているコスト

施策 5 後の `h["name"]` / `h[:name]` ループの `perf` セルフ時間:

| | String キー | Symbol キー |
|---|---:|---:|
| IndexMap / hashbrown の probe | 31.6 % | 32.6 % |
| `HashRef::get`（2 インスタンス化の合計） | 25.0 % | 19.7 % |
| `Hashmap::index` | 10.4 % | 13.6 % |
| ダイジェスト（`RubyHasher`） | 7.8 % | 8.9 % |
| `hashindex` builtin | 7.2 % | 6.3 % |
| `memcmp`（キー比較） | 7.7 % | — |

ハッシュは 33 % → 8 % まで落ち、いまの支配項は **probe と 4 段のディスパッチ**
（`hashindex` → `Hashmap::index` → `HashRef::get` → `IndexMapCore`）である。

---

## 4. 今後のアイデア（効果の見込み順）

### 4.1 Integer / Float キーの二重ハッシュを外す（小・確度高）

`Value::ruby_hash_packed` の Fixnum / Float アームは、`Integer#hash` /
`Float#hash` が返す**値**を作るために内側で `seeded_hasher()`（std の
`DefaultHasher` = SipHash）を丸ごと 1 回回し、その結果を外側のマップハッシャに
流している。これは `Array#hash` / `Hash#hash` が要素を混ぜるときに Ruby レベルの
`#hash` 結果と一致させるための要請で、**バケッティングには要らない**。上表で
Integer キーだけ Symbol キーより 14 ns 遅い（39.5 vs 25.7）のがそのコスト。

構造ハッシュ用（Ruby から見える `#hash` 値）とバケッティング用を分ければ、
Integer キーの参照が Symbol キーと同じところまで来るはず。

### 4.2 probe を JIT でインライン展開する（大・最大の残り。**段階 1〜3 実装済み**、2026-09-04/05）

いまは `Hash#[]` のインライン生成が `hashindex` への直接呼び出しまでしか
やらない（メソッドフレームは省くが、そこから先は Rust）。受信側が
`Hash` ちょうどで boxed 表現だと分かっている呼び出しサイトなら、
ダイジェスト → インデックステーブル引き → エントリ比較を機械語で出せる。
上の内訳の probe 32 % + ディスパッチ 42 % のかなりの部分が対象で、YJIT との
残差（33.7 vs 17.5 ns）を埋める本命。`gen_hash_entry_at` が既に
`rubymap::EntriesLayout` からエントリ配列を歩いているので、必要なレイアウト
知識は揃っている。

#### 段階分け

`IndexMapCore` には 2 つの動作領域がある。**≤ 8 エントリ**（`AR_MAX`、CRuby の
`RHASH_AR_TABLE_MAX_SIZE` と同じ）は `linear`、`indices` を作らず `entries` を
線形走査して `entry.hash == hash` の一致だけキー比較する。それ以上は hashbrown
の probe（上位 7 ビットで control byte を 16 個ずつ SIMD 比較 → 添字 →
エントリ比較）。`Bucket { hash, key, value }` はハッシュ値を格納しているので、
線形走査は「64 ビット比較 × N、当たったときだけキー比較」で済む。

| 段階 | キー | 領域 | 状態 |
|---|---|---|---|
| 1 | packed のうちビットがそのままミキサーに入るもの（Symbol / nil / true / false） | 線形（≤ 8） | **実装済み** `AsmInst::HashProbe`（旧 `HashProbePacked`） |
| 2 | 同上 | 索引（> 8）— hashbrown の group probe | **実装済み**（段階 3 と同時） |
| 3 | String — バイト列ダイジェスト + memcmp + `plain_string` 判定 | 両方 | **実装済み**（erubi の 18 エントリ・String キーはここ） |

Integer キーは 4.1 の二重ハッシュ（SipHash を内側で 1 回回す）が解けるまで
対象外。葉ヘルパで正しく計算はできるが、ヘルパ自体が現状のルックアップの
大半のコストになる。

#### 段階 1 の設計判断（2026-09-04）

- **ダイジェストは葉ヘルパ `packed_digest_c`、probe は機械語。** fold は
  64×64→128 の乗算だが、この JIT が使う x86-64 アセンブラ（monoasm）に
  1 オペランド `mul` が**無い**（`div` / `idiv` はあり、`imul` は 2 オペランドの
  下位 64 ビットのみ）。aarch64 には `umulh` がある。monoasm に足したら
  ヘルパをその命令列に置き換えるだけで、他は変わらない。
- **形の不一致は deopt ではなく builtin 呼び出し。** inline 表現・identity
  マップ・索引領域はすべて `hashindex` への合流で、exit しない。最初は deopt に
  していたが、この deopt は再コンパイルを起こさないので、大きな Symbol キー
  Hash のサイトが**毎回 deopt**することになる（4.5 の ic ゲートで踏んだのと
  同型）。builtin 呼び出しなら現状と同コストで退行しない。
- **線形領域の miss は in-line で `nil`。** 走査を尽くしたら
  `Option<Box<HashDefault>>`（null = default 値も proc も無し）を 1 ロード見て、
  無ければ nil。builtin に落とすとダイジェストと走査をもう一度やり直すので、
  最初の実装では miss が 16 → 30 ns に**退行**していた。
- **`EntriesLayout` に `hash_offset` / `linear_offset` を追加**。`Bucket` は
  niche を持つキー型で並び替わる（`Option<Value>` では value+0 / hash+8 /
  key+16）ので、既存どおり実測で取る。

#### 段階 1 の効果（6 エントリ・Symbol キー、純ルックアップ、交互 4 ラウンド）

| | 従来 | 段階 1 |
|---|---:|---:|
| hit（先頭） | 16.3–18.1 | **6.3–9.7** |
| hit（末尾） | 15.3–19.3 | **8.2–11.0** |
| miss | 16.5–20.4 | **7.4–10.6** |
| 18 エントリ Symbol（索引領域、fallback） | 18.7–26.0 | 15.5–21.2（退行なし） |

hit 経路の call は `packed_digest_c` の 1 つだけになり、`hashindex` の call は
miss 側にしか残らない（`emit-asm` で確認）。`tests/hash_probe_jit.rs` が
hit / miss / default 値 / default proc / nil・true・false キー / tombstone /
索引領域・inline 表現・identity マップへの委譲 / クラスガード / 線形↔索引の
境界を跨ぐ成長 / 生きた状態の読み取りを CRuby と突き合わせる。

#### 段階 2・3 の設計判断（2026-09-05）

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
- **レイアウトは `offset_of!` で取る。** `EntriesLayout` に
  `indices_ctrl_offset` / `indices_mask_offset` / `group_width` を追加。
  hashbrown（vendored）の `RawTable` / `HashTable` に `ctrl_offset()` /
  `bucket_mask_offset()` / `GROUP_WIDTH` を生やした。group 幅が 16 で
  ない構成では索引領域を builtin に渡す（静的分岐）。
- **String キーは葉ヘルパ 2 つ + 機械語 probe。** `string_digest_c`（バイト
  列の wyhash、挿入時と同じ）と `string_key_eq_c`（`string_key_eq` そのもの
  ：identity → STRING×STRING バイト比較）。同一オブジェクト（frozen
  リテラル）なら eq 呼び出しを省く。**格納ダイジェストが一致したのに eq が
  偽**なら（64 ビット衝突か tombstone か異種キー）`miss` → builtin。こうする
  と eq 呼び出しの前後で probe のループ状態を退避する必要が無く、rdx/rcx
  と entry ポインタだけ push すれば済む。
- **サブクラスは class guard が弾く。** `guard_class(STRING_CLASS)` はクラス
  一致なので、`eql?` を dispatch すべき String サブクラス（#1258）は
  probe に入らず deopt する。
- **identity マップは probe しない。** `HashContent` の判別子（0 = Map,
  1 = IdentMap）を見て IdentMap なら builtin へ。段階 1 は判別子を見て
  いなかったが、Symbol は `IdentKey` のダイジェスト（`id()`）と packed
  ダイジェストが一致するので偶然正しかった。String は内容ダイジェスト ≠
  identity ダイジェストで、**自分自身すら見つからず nil を返していた**
  （`compare_by_identity` + String キーで検出、テスト追加）。
- **キークラスが混在するサイトは段階 1 と同じ性質。** `h[k]` の k の
  クラスは抽象状態から取り、guard の失敗は plain deopt（再コンパイル無し）。
  同じサイトに String と Symbol が交互に来ると、後から来たクラスは毎回
  deopt して interpreter で実行される（計測では 12 → 24 ns）。段階 1 の
  Symbol guard も同じ。receiver 側の `BecamePolymorphic` 再コンパイルは VM
  が立てる POLY ビットで「次は generic に」と判断できるが、引数クラスには
  その仕組みが無く、再コンパイルしても同じ guard が出るだけなので保留。

#### 補足: どこから発火し、いつ機械語を諦めるか

- `Hash#[]` のインライン生成 `hash_index` は `fire_index_inline` から呼ばれ、
  受信側クラスが**ちょうど `Hash`** のときだけ probe を出す。理由は builtin
  の `Hash#[]` がミス時に `default` **メソッド**を dispatch する（サブクラスや
  特異メソッドで上書きできる）のに対し、`hashindex` は格納されたデフォルトを
  直接読むからで、サブクラスは通常の class-version ガード付き呼び出しに残す。
  `Hash#[]` は `BASIC_OP_DEFS` に入っているので class-version ガードは省き、
  `record_bop_dep` で再定義を捕まえる。`Hash#[]=` は意図的に BOP に**入って
  いない**ので、`hash_index_assign` は通常経路のまま（`index_hash_assign_redefinition`
  テストが固定）。
- 受信側クラスを抽象状態が確定できない多相サイトでは `index_dispatch` が
  **2 腕のディスパッチ**を出す: `BrClassNe` で Hash なら probe、そうでなければ
  `runtime::get_index`。外れたクラスは deopt ではなく C 呼び出しになる。
- キーのクラスはサイトのインラインキャッシュ／抽象状態から取り、
  Symbol / nil / true / false は `packed_digest_c`、String は `string_digest_c`
  + `string_key_eq_c`、それ以外（Integer を含む）は probe を出さず
  `hashindex` の直接呼び出しに落ちる。
- `hash_entries_layout()` が `None`（2 つのマップ型でレイアウトが一致しない
  構成）か、hashbrown の group 幅が 16 でない構成では、索引領域は静的に
  `miss` へ飛ばす。いずれも性能上の後退で、正しさには関わらない。

#### 段階 2・3 の効果（純ルックアップ、交互 2 ラウンド、ns）

| | 段階 1 | 段階 3 | CRuby+YJIT |
|---|---:|---:|---:|
| String 6 エントリ hit（先頭 / 末尾） | 12.4–13.7 / 14.6–16.2 | **10.7–11.1 / 14.4–14.6** | 33.1 / 34.6 |
| String 6 エントリ miss | 13.5–15.6 | **10.9** | 27.9 |
| String 18 エントリ（erubi 形）hit | 14.4–17.1 | **11.7–12.3** | 26.4–40.5 |
| String 18 エントリ miss | 12.6–17.1 | **8.1–8.3** | 18.8 |
| String 100 エントリ hit / miss | 14.7–17.1 / 12.7–14.6 | **11.6–12.1 / 8.0–10.7** | 25.3 / 19.0 |
| Symbol 18 / 100 / 1000 エントリ hit | 10.9–13.3 | **7.5–8.6** | 10.6 |
| Symbol 18 / 100 / 1000 エントリ miss | 11.1–15.0 | **8.8–11.2** | 11.2 |

String の hit は digest 呼び出し（wyhash）と eq 呼び出し（memcmp）が残るので
Symbol ほどは縮まないが、miss は in-line で nil を返せるので 8 ns 台。
yjit-bench の erubi（`benchmark_mono.rb`、warmup 10 + 30 反復、交互 2 ラウンド）
は 215–221 ms → **192–206 ms**（−7〜−10 %）。
`tests/hash_probe_jit.rs` に String キー（線形 / 索引 / frozen 同一 /
default 値・proc）、索引領域の Symbol（100・3000 エントリ、削除後）、
索引領域の tombstone、サブクラスキー、identity マップ、線形↔索引の成長、
String → Symbol のクラス変化を追加。

### 4.3 呼び出しサイトのキーセット・インラインキャッシュ（中・要検証）

erubi の 322 個の spec Hash は **すべて同じ 18 個のキーを同じ順序で持つ**
（同じ JSON 形状から作られる）。`spec["name"]` のような「リテラルキー ×
同形状の Hash」という組み合わせは、テンプレートエンジンや JSON 処理では
支配的なパターンである。呼び出しサイトに「このキーセットならインデックスは
これ」を憶えさせられれば、probe そのものを飛ばせる。CRuby のオブジェクト
shape に相当する仕組みを Hash に持ち込むことになるので、キーセットの同一性を
安く判定する仕掛け（挿入順の版番号など）の設計が要る。

### 4.4 ディスパッチ段数を減らす（小〜中）

`hashindex` → `Hashmap::index` → `HashRef::get` → `IndexMapCore` の 4 段で、
それぞれが `vm` / `globals` を引き回している。`Hashmap::index` はミス時の
default 処理のためだけに 1 段あり、`HashRef::get` は表現の分岐をしている。
表現の分岐を呼び出し側に持ち上げて boxed 専用の入口を用意すれば数 ns 縮む
見込み。4.2 を先にやると自然に消える部分もある。

### 4.5 frozen String にダイジェストをキャッシュ（小）

施策 5 の前は有力だったが、ハッシュが 8 % まで落ちた今は上積みが小さい。
長いキー（パスや URL）を多用するコードでは効くので、`RStringInner` に
4 バイトの空きができたときの候補として残す。frozen なら無効化が不要という
性質は変わらない。

### 4.6 inline 表現の拡張（要検討）

48 バイトのペイロードにペア 3 組でちょうど埋まっている。4 組以上にするには
`RValue` を大きくするかキー・値を別配列にするかで、どちらも Hash 以外の
すべてのオブジェクトに影響する。erubi の Hash は 18 ペアなので、この方向で
救えるワークロードは限られる。

### 4.7 RValue の中身のアロケータ（別件だが Hash に効く）

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

---

## 6. CRuby との実装差異

| 項目 | CRuby | monoruby |
|---|---|---|
| 表現の段階 | `ar_table`（≤ 8、線形、ハッシュ値格納）→ `st_table` | **≤ 3 ペアは RValue のペイロードに直置き（ヒープ確保ゼロ）** → boxed の線形領域（≤ 8、`AR_MAX`、CRuby と同じ閾値）→ hashbrown の索引領域。線形→索引はエントリ `Vec` を共有したまま index table を作るだけで、`ar`→`st` のような作り直しは無い |
| 挿入順序 | `st_table` のエントリ配列 | `rubymap`（IndexMap 系）: エントリ `Vec` + 位置の index table。観測される順序は同じ |
| 反復中の変更 | 追加は例外、`delete` は許可（`RHASH_ITER_LEV`） | 同じ規則（既存キーの更新は許可）。機構は tombstone + `dead` カウンタ + `compact_if_dirty`。inline hash は tombstone を置く前に boxed へ昇格する。深度は inline が 2 ビット飽和、boxed が `Cell<u32>` |
| 走査の実装 | `rb_hash_foreach`（C のコールバックループ） | `each` は Ruby の位置ループ（§1.6）。JIT がブロックごと展開するため |
| String キーの複製 | `rb_hash_key_str` が fstring テーブルに**インターン**（Hash 間で共有） | `Value::frozen_hash_key` がバイト列から新しい frozen コピーを毎回作る。**重複排除テーブルは無い**。特異メソッドは引き継がない |
| String キーの比較・ハッシュ | 内容のみ、`String#hash` は dispatch しない。クラスが `String` ちょうどのときだけバイト比較で短絡（`rb_any_cmp` / `any_hash`） | 同じ規則（`Value::eql`、`string_key_eq`）。ただしバイト比較は `RStringInner::eq` で**エンコーディングを見ない**。CRuby の `rb_str_hash` は非 ASCII 文字列でエンコーディング index を混ぜる |
| `compare_by_identity` | `st_table` の型を `identhash` に切り替えて再ハッシュ | inline は `IDENT_BIT` を立てるだけ（packed キーは `eql?` と同一性が一致するので inline のまま）。boxed は**別型**の `RubyMap<Option<IdentKey>, Value>` に作り直す。空マップ限定で双方向に切り替える `set_compare_by_identity_empty` があり、`Hash#replace` / `Set` が使う |
| デフォルト値／proc | `ifnone` スロット | `Option<Box<HashDefault>>`。null なら「どちらも無し」を 1 ロードで判定でき、JIT のミス経路が in-line で nil を返せる。デフォルトを持つと boxed 強制（`Hash.new(7)` は確保する）。`clear` はデフォルトを保つ（CRuby と同じ） |
| ミス時の `default` | `rb_funcall(id_default)` | builtin の `Hash#[]` は同じく dispatch。`hashindex` / `runtime::get_index` は格納値を直接読むので、対象を `Hash` ちょうどに限定 |
| Ruby から見える `#hash` | `rb_hash_start` 系（SipHash-1-3、seeded） | `HASH_STATE`（std `RandomState` = SipHash-1-3、seeded）。バケッティングは別の seeded ミキサー（§1.5） |
| String ハッシュのキャッシュ | 無し | 無し（§4.5 に候補として残す） |
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
