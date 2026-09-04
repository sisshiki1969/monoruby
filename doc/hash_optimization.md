# Hash の実装と最適化

Ruby プログラムは Hash を「オブジェクトのフィールド袋」として使うので、
`[]` / `[]=` / `key?` / `fetch` はアプリケーションのもっとも内側のループに
現れる。yjit-bench の erubi では実行時間の 29 %、rack で 18 %、
activerecord で 13 % が Hash 参照だった
（[`yjit_bench_slow_investigation_2026-09.md`](yjit_bench_slow_investigation_2026-09.md) §5.2）。

この文書は、現在の Hash がどういう表現とどういう探索経路を持っているかを
まとめ、そこに入れた最適化を計測とともに記録し、残っているコストと次の
候補を優先順位付きで並べる。個々の API の意味論ではなく「1 回の参照に何が
起きるか」に焦点を当てる。

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
一緒に移動する。

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

4 ペア目、ヒープキー、デフォルト値、`compare_by_identity` のいずれかが来ると
`promote` が boxed へ移す。

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
`rubymap::EntriesLayout` のプローブから取る。3 ペア以下の Hash リテラルは
JIT がセルを bump 確保してヘッダとペアを直接書き込む（`emit_alloc_cell`）。

`Hash#each` / `each_key` / `each_value` はこれらのプリミティブの上に Ruby で
書かれている（`builtins/hash.rb`）ので、ホットな呼び出しサイトではメソッドと
ブロックの両方がインライン展開される。

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

### 4.2 probe を JIT でインライン展開する（大・最大の残り）

いまは `Hash#[]` のインライン生成が `hashindex` への直接呼び出しまでしか
やらない（メソッドフレームは省くが、そこから先は Rust）。受信側が
`Hash` ちょうどで boxed 表現だと分かっている呼び出しサイトなら、
ダイジェスト → インデックステーブル引き → エントリ比較を機械語で出せる。
上の内訳の probe 32 % + ディスパッチ 42 % のかなりの部分が対象で、YJIT との
残差（33.7 vs 17.5 ns）を埋める本命。`gen_hash_entry_at` が既に
`rubymap::EntriesLayout` からエントリ配列を歩いているので、必要なレイアウト
知識は揃っている。

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

### 4.5 frozen String にダイジェストをキャッシュ（**却下**、2026-09-04）

施策 5 の前は有力だったが、ハッシュが 8 % まで落ちた今は上積みが小さい。
frozen なら無効化が不要という性質は変わらない。**が、実装して却下した** —
理由は当初書いていた「空きバイト待ち」ではなく、メモの**幅**が前提を壊す
ことにある。

バケットハッシュを作る経路は 2 つあり、いまは「シードミキサーを生バイト列に
適用する」という同一の計算なので値が一致している:

| 経路 | 使う場所 |
|---|---|
| `string_digest(...)` が値を直接返す | prehashed な挿入・探索（fast path） |
| `Value::ruby_hash(state)` がバイト列をハッシャに流し込む | 汎用 `insert`（`promote` が使う）、複合キーの**要素**としての String |

メモ化はこの**値そのもの**を保存しなければならない。**64 ビットのメモなら
両経路に完全に不可視**で、何も変わらない。ところが `RStringInner` は 48 バイトの
`RValue` ペイロードちょうどで、`content`(40) + `ty`(1) + `cr`(1) の後に残るのは
**6 バイト**、8 ではない。したがって入るのは 32 ビットで、値が変わる。
ストリーミング側はそれに追随できない — Array キーの要素になった String は
その Array のハッシャに寄与を流し込むので、事前計算した値を差し込む場所がない。

実際に 32 ビット + 再拡散（`wrapping_mul` で上位ビットを埋める。hashbrown は
語の上位 7 ビットを control byte に使うので、ゼロ拡張では filtering が死ぬ）で
実装したところ、inline → boxed 昇格（`promote` は汎用 `insert`）を経たキーが
別バケットに入り、`tests/hash_string_keys.rs` の 3 本が落ちた。

やるなら、汎用経路で **top-level の String キー**を扱う箇所を全て
`insert_prehashed_with` + `string_digest` に寄せ、ストリーミング経路には
入れ子の String だけを残す（複合キーの内部なので top-level と一致する必要が
ない）。理屈は通るが、効果は String ルックアップの ~7 %（ダイジェストは既に
自己時間の 8 %）、漏れがあると**静かにバケットが食い違う**、さらに全 String
キーのハッシュ幅が 64 → 32 ビットに落ちる。割に合わないと判断した。

`RStringInner` に 8 バイトの空きができた場合は話が別で、そのときは値が
変わらないので上記の一貫性の問題自体が消える。

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

## 5. 計測手順（再現用）

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
