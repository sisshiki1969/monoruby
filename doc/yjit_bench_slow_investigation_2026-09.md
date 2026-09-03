# yjit-bench で monoruby が遅い 4 ベンチ (activerecord / erubi / rack / graphql) の原因調査

調査日 2026-09-03。対象コミットは `750d04f`（master, "Boot rubygems lazily" #1240 直後）。
[ベンチマークポータル](https://sisshiki1969.github.io/monoruby/)の x86-64 最新スナップショット
（`c295e69`, 2026-09-02）で monoruby が CRuby+YJIT の半分前後の速度しか出ていない 4 本に
ついて、どこで時間を使っているかを `perf` / `--features profile` / `--features deopt` /
`--features jit-log` / マイクロベンチで測り、monoruby 固有のボトルネックを切り分けた記録。

計測機は x86-64 / Linux（Xeon 2.1 GHz 4 vCPU, L2 8 MiB, L3 260 MiB）。比較対象は
rbenv でビルドした CRuby 4.0.6（`--yjit`）。ベンチ本体は
[Shopify/yjit-bench](https://github.com/Shopify/yjit-bench) `e73823e`（2026-08-31）を
`harness-warmup` で走らせ、後半半分の反復の中央値を使う（CI の `bench.yml` と同じ条件）。
gem のバージョンは rack 3.2.3 / graphql 2.5.11 / erubi 1.13.1 / activerecord 8.1.1 +
sqlite3 2.7.3。

---

## 1. 結論（先に要点）

4 本の遅さは JIT が吐くコードの質ではなく、**ランタイム側（Rust の builtin・GC・
アロケータ・呼び出しキャッシュの設計）に集中**している。perf のセルフ時間では JIT
コード本体はどのベンチでも 15〜20 % しかなく、残りは Rust 側の Hash / String /
アロケータ / GC / 正規表現 / FFI である。

monoruby 固有で CRuby+YJIT より明確に遅いと確認できた要素（マイクロベンチ §4）:

| 要素 | monoruby | YJIT | 倍率 | 効くベンチ |
|---|---:|---:|---:|---|
| String キーの Hash リテラル `{"content-type" => "text/plain"}` | 155 ns | 60 ns | **2.6x** | rack（レスポンスヘッダ）, activerecord |
| `ary << x` で `x` のクラスが混在（PMC が引数クラスでキーされる） | 84 ns | 66 ns（単相は 3.6 ns → 23x） | — | graphql, activerecord |
| `catch` / `throw` | 580 ns | 218 ns | **2.7x** | activerecord（callbacks） |
| `raise` / `rescue` | 1288 ns | 746 ns | 1.7x | rack, activerecord |
| `StringScanner` による字句解析（Ruby 実装） | 692 µs | 438 µs | 1.6x | graphql |
| `Array#map(&:sym)` | 1556 ns | 926 ns | 1.7x | 全般 |
| String キー Hash（20 キー）`h["k"]` / `h["k"] = v` | 256 / 369 ns | 202 / 250 ns | 1.3x / 1.5x | erubi, rack, activerecord |
| `Hash#dup`（30 キー） | 395 ns | 306 ns | 1.3x | rack（`env.dup`） |
| `'lit'.freeze`（`frozen_string_literal` なし） | 65 ns | 26 ns | 2.5x | （erubi は pragma 付きなので該当せず） |

加えて、どのベンチにも共通に効いている構造的な要因:

- **GC + アロケータで 20〜30 %**（malloc/free 13〜22 % self + GC が inclusive で 8〜14 %）。
  ヒープが 22〜27 ページ（≈ 6 MB）と極端に小さく、erubi は 1 反復あたり 7.3 回、
  activerecord は 3.8 回のマイナー GC が走る。毎回のマイナー GC がロードされた
  **全 ISeq のリテラル・全クラスをルートとして再走査**するので、Rails 級のコード量では
  ルート走査だけで固定費になる（`ISeqInfo::mark` が 1.4〜1.7 % self）。
- **String キー Hash の検索**（`RandomState` = SipHash-1-3 を毎回計算、ハッシュ値の
  キャッシュなし、String キーはインライン表現に入らず必ず boxed map）が erubi で 30 %、
  rack で 18 %、activerecord で 13 %。
- **VM（インタプリタ）側の `[]=` / `<<` はインラインキャッシュを使わず、毎回
  `find_method` → グローバルメソッドキャッシュ**を引く（rack で `Hash#[]=` 780 万回、
  activerecord で 1006 万回、graphql で `Array#<<` 712 万回）。
- activerecord では **deopt の嵐**（60 秒で約 1,600 万回。`LazyAttributeSet#fetch_value`
  の `type.deserialize(value)` サイトだけで 1,000 万回）。1 回の deopt は安い（≈ 15〜20 ns）
  ので支配的ではないが、再コンパイルしても直らない単相ガードを出し続けるポリシー上の穴。
- activerecord の sqlite3 は Fiddle 経由（`fiddle_invoke` + libffi + libsqlite3 で 12 %）。

## 2. 再現結果

| ベンチ | CRuby 4.0.6 --yjit | monoruby | 速度比 (YJIT 基準) | ポータル値 (`c295e69`) | monoruby `--no-jit` | CRuby interp |
|---|---:|---:|---:|---:|---:|---:|
| activerecord | 106 ms | 284 ms | 0.37x | 0.45x | 605 ms | 316 ms |
| erubi | 181 ms | 275 ms | 0.66x | 0.55x | 511 ms | 230 ms |
| graphql | 31 ms | 51 ms | 0.61x | 0.57x | 188 ms (参考) | 63 ms |
| rack | 38 ms | 79 ms | 0.48x | 0.53x | 116 ms (参考) | 65 ms |

- ポータルとの差は計測機の違い。傾向は一致する。
- `--no-jit` は monoruby のインタプリタのみ。**rack / activerecord ではインタプリタが
  CRuby のインタプリタより遅く、JIT を有効にしても CRuby のインタプリタに届かない**
  （rack 79 ms vs 65 ms）。JIT の寄与（rack 116→79、activerecord 605→284）は CRuby の
  YJIT の寄与（65→38、316→106）と同程度なので、差は JIT ではなくランタイムにある。
- graphql / rack の `--no-jit` は他ジョブと並走中に測った参考値。
- 起動（ベンチ計測の外側）: `Bundler.setup` + `require "rack"` が monoruby 0.47 s /
  CRuby 0.17 s、`require "active_record"` が 846 ms / 101 ms。

## 3. 時間の内訳（perf, 定常状態のセルフ時間）

`--features perf` ビルドを `perf record -D 12000 -F 999 -e cpu-clock -g --call-graph=fp`
（起動から 12 秒後にサンプリング開始 = ウォームアップ後のみ）で採り、シンボルを大まかに
分類したもの。分類の正規表現は §7 の `categorize.rb`。

| 区分 | rack | graphql | erubi | activerecord |
|---|---:|---:|---:|---:|
| JIT 生成コード + VM 領域の共有スタブ | 14.7 % | 20.4 % | 16.0 % | 21.2 % |
| Hash（String digest / probe / insert） | 17.5 % | 4.6 % | **29.6 %** | 12.6 % |
| malloc/free + RValue alloc | 17.1 % | 14.8 % | 13.1 % | **22.5 %** |
| GC（mark / sweep, self） | 9.1 % | 6.3 % | 7.1 % | 8.4 % |
| 正規表現（onigmo） | 7.2 % | **33.9 %** | 0 % | 0.2 % |
| String 操作（連結・split・join・memmove） | 8.9 % | 2.5 % | **20.2 %** | 4.1 % |
| メソッド探索・引数処理 | 8.4 % | 4.3 % | 2.7 % | 5.3 % |
| FFI / libsqlite3 | 0 % | 0 % | 0 % | **12.4 %** |
| kernel（page fault, smaps） | 4.3 % | 3.9 % | 2.3 % | 1.9 % |
| その他 | 12.6 % | 9.2 % | 8.8 % | 10.8 % |
| （参考）`execute_gc` の inclusive | 13.0 % | 8.1 % | 11.1 % | 13.6 % |

「JIT 生成コード」のうち perf map で名前が付く JIT 本体は rack 5.1 % / graphql 19.4 % /
erubi 16.0 % / activerecord 17.4 %。rack の残り 8 % は `monoruby-vm` 領域内の無名
アドレス（gdb で逆アセンブルすると、メソッド呼び出しからの復帰シーケンス
`VM+0x4005` と命令ハンドラ群）。完全に JIT 化されたマイクロベンチでも同じ領域に
5〜6 % のセルフ時間が出るので、これはインタプリタ実行ではなく **JIT コードが共有する
VM 領域内スタブ**（呼び出し復帰・エラー検査・GC ポール等）と解釈している。
rack のリクエスト処理に含まれる Ruby メソッドは 1 リクエストあたり 30 個で、
`jit-log` により全て JIT 済み・定常状態での再コンパイルはゼロと確認した。

### 3.1 ベンチ別のホットスポット（上位シンボル、定常状態）

**erubi**（`ErbRenderer#run_erb` の JIT ブロックが 11.7 % self、残りは builtin）

| シンボル | self |
|---|---:|
| `hashbrown::HashTable::find`（`HashRef::get` の probe） | 11.5 % |
| `hash::string_digest`（SipHash-1-3）+ `Sip13Rounds::write` | 9.3 % |
| `HashRef::get` + `get_index_of_prehashed_with` + `Hashmap::index` | 7.1 % |
| `runtime::concatenate_string_inner` + `append_piece`（`#{}` 補間） | 7.4 % |
| `memmove` + `SmallVec::insert_from_slice`（`_buf << str`） | 7.8 % |
| `GCBox::free` + `RurubyAlloc::alloc` + `Allocator::alloc` + `_int_malloc` | 8.4 % |
| `Array#join` / `append_string` / `array_join` | 4.1 % |
| `builtins::object::freeze` + `Object#freeze` ラッパ | 2.7 % |
| `Executor::invoke_tos` + `to_s_is_refined` | 2.4 % |

テンプレートは JSON 由来の 2,600 個の spec Hash（各 20 キー前後）に対する
`spec["name"]` のような **String キーの `Hash#[]`** と、`_buf << '...'.freeze` /
`_buf << (expr).to_s` の繰り返し。上の 3 行が前者に対応する。後者は pragma
`frozen_string_literal: true` 付きで eval されるので monoruby でも速い（§4）。

**rack**（`Rack::Static#call` 以下のミドルウェア連鎖が 51 % inclusive。
`Rack::URLMap#call` 24 %、`Rack::Utils#clean_path_info` 18 %、`Proc#call`（アプリ本体）8 %）

| シンボル | self / inclusive |
|---|---:|
| `Hashmap::index` + `HashRef::get` + `string_digest` + `Sip13` | 8.1 % incl. |
| `String#split`（`clean_path_info`） | 8.1 % incl. |
| `Regexp#match` + `captures_from_pos` + `onig_search`（`Rack::Utils` / `URI` / `Static`） | 8.3 % incl. |
| `Hash#clone`（`env.dup`: `clone_body` + `deep_copy`） | 4.5 % incl. |
| `runtime::set_index`（VM/JIT 汎用 `[]=` → `invoke_method` → グローバルキャッシュ） | 5.1 % incl. |
| `String#gsub`（`URI::RFC2396_Parser#unescape`） | 3.5 % incl. |
| `Executor::save_capture_special_variables`（`$~` の保存） | 3.3 % incl. |
| `execute_gc`（`Root::mark` 5.8 % + sweep） | 13.0 % incl. |
| `RurubyAlloc::alloc` + `Allocator::alloc` + `malloc`/`free` 系 | ≈ 12 % self |
| `kernel::respond_to` | 1.0 % self |
| `Codegen::chain_deopt_into` + `jit_module::handle_error`（`URLMap#call` ブロック内 `return`） | 1.6 % self |

1 リクエストあたり CRuby 側の C 呼び出しは `Hash#[]` 18 回、`Hash#[]=` 5 回、
`Hash#key?` 4 回、`respond_to?` 3 回、`Regexp#match` 2 回など。monoruby ではこれらの
1 回あたりのコストと、レスポンスヘッダの Hash リテラル生成（§5.2）が積み上がる。

**graphql**（`Lexer#advance` 55 % inclusive、うち `StringScanner#_match_len_at_pos` 48 %）

| シンボル | self |
|---|---:|
| `match_at`（onigmo） | 17.5 % |
| `JIT:<StringScanner#_match_len_at_pos>` | 4.6 % |
| `JIT:<Lexer#advance>` | 3.5 % |
| `string_strscan_match` | 2.2 % |
| `onig_search_gpos` + `mbc_enc_len` + `onigenc_mbclen_approximate` + `onig_region_*` | ≈ 5 % |
| `RubyMap::hash`（`_anchored` のキャッシュ引き）+ `GvarTable::lookup` | 3.3 % |
| `RStringInner::regex_view` + `check_utf8` + `ascii_only` | 2.0 % |
| malloc/free 系 + `GCBox::free` | ≈ 12 % |

**activerecord**（`_load_from_sql` 30 %、`instantiate_instance_of` 29 %、
`init_with_attributes` 23 % inclusive）

| シンボル | self |
|---|---:|
| `malloc` + `_int_malloc` + `malloc_consolidate` + `_int_free` + `unlink_chunk` + `cfree` | 16.2 % |
| `fiddle_invoke` + `ffi_call*` + `classify_argument` + `value_to_carg` + `bits_to_value` | 7.6 % |
| `HashRef::get` + `string_digest` + `Sip13` + `get_index_of_prehashed_with` + `Hashmap::index` | 7.7 % |
| `ISeqInfo::mark` + `mark_children` + `RValue::mark` + `Store::mark` + `ClassInfo::mark` | 5.2 % |
| libsqlite3（`sqlite3VdbeExec` 他）+ `pthread_mutex_lock/unlock` | 5.2 % |
| `RurubyAlloc::alloc` + `Allocator::alloc` + `GCBox::free` | 5.1 % |
| `GlobalMethodCache::get` + `find_method` + `check_method_for_class_with_version` | 1.5 % |
| `RStringInner::from_vec_scanned`（DB からの文字列生成） | 0.7 % |

## 4. マイクロベンチによる切り分け

`scratchpad` の `micro*.rb`（§7 に要点）。単位は ns/op、3 ラウンド目の値。**結果を捨てる
式は CRuby がコンパイル時に消してしまう**（`"#{a}"` を捨てると CRuby はオブジェクトを
1 つも作らず、`10M 回で GC 0 回`）ので、結果を使う形で測った値だけを載せる。
「fsl」は `# frozen_string_literal: true` 付き。

| 項目 | monoruby | YJIT | 備考 |
|---|---:|---:|---|
| `{"content-type" => "text/plain"}`（fsl） | 155 | 60 | **String キーは boxed map を毎回構築** |
| `{"content-type" => .., "last-modified" => ..}`（fsl） | 162 | 61 | |
| `{a: 1, b: 2}` | 54 | 60 | Symbol キーはインライン表現 |
| `h["PATH_INFO"]`（fsl, 3 キー） | 35 | 34 | |
| `h["name"]`（20 キー） | 256 | 202 | |
| `h["name"] = i`（20 キー） | 369 | 250 | |
| `h["name"]`（4 キー） | 145 | 143 | 小さい Hash では差なし |
| `env.dup`（30 キー） | 395 | 306 | |
| `[200, i, [i]]` | 47 | 82 | |
| `s = "text/plain"`（fsl） | 9 | 26 | |
| `s = 'abcdef'`（非 fsl, 複製あり） | 57 | 58 | |
| `s = 'abcdef'.freeze`（fsl） | 16 | 25 | |
| `s = 'abcdef'.freeze`（非 fsl） | 65 | 26 | 複製 + `Object#freeze` 呼び出し |
| `buf << 'lit'.freeze`（fsl） | 30 | 44 | erubi の生成コードの形 |
| `buf << v.to_s`（v は String） | 13 | 57 | |
| `buf << i.to_s`（Integer） | 128 | 107 | |
| `"<#{a}>"`（結果使用） | 107 | 130 | |
| `a.map { "<a href=\"##{v}\">#{v}</a>" }.join(", ")`（10 要素） | 1959 | 2336 | |
| `ary << obj`（単相） | 3.6 | 36 | |
| `ary << obj`（引数が 2 クラス交互） | 83.5 | 66 | 単相の 23 倍 |
| `ary << obj`（引数が 4 クラス） | 57 | 15 | |
| `raise ArgumentError` / `rescue` | 1288 | 746 | |
| `catch(:x) { ... throw :x }` | 580 | 218 | |
| `StringScanner` で 2.7 KB を字句解析 | 692 µs | 438 µs | |
| `%w[...8個].map(&:upcase)` | 1556 | 926 | |
| `Time.httpdate(...)` | 3612 | 3060 | 両方 Ruby 実装 |
| `ary.each { return x if ... }`（ブロック内 return） | 41 | 120 | |
| 8 段のミドルウェア連鎖（葉がブロック内 return / 通常） | 206 / 131 | 249 / 137 | 差なし |
| 6 クラスの megamorphic 呼び出し | 18.5 | 42 | deopt 自体は安い |
| `Foo.new(a, b, c)` | 28 | 35 | |
| `obj.respond_to?(:to_path)` | 1.8 | 70 | |
| `1 << 63 <= i`（Bignum 比較） | 86 | 82 | |
| `lambda { ... }.call(env)`（Rack アプリ相当） | 564 | 352 | 直接呼び出しでも 498 vs 318（Hash リテラル分） |

検証して**否定した仮説**（記録として残す）:

- 「rack は `Rack::URLMap#call` のブロック内 `return` が chain deopt を起こし、呼び出し元
  ミドルウェアの後半が VM で走る」— 8 段連鎖のマイクロベンチで葉が `return` する場合と
  しない場合の差は 75 ns、`--features profile` のグローバルキャッシュ計数（VM 実行なら
  `Hash#[]=` が 8 倍になる）も同じだった。呼び出し元は JIT のまま。
- 「文字列補間 `"#{a}"` が 5 倍遅い」— 結果を捨てるベンチの計測ミス。使うと monoruby の方が速い。
- 「erubi は `'lit'.freeze` の複製が原因」— erubi の生成コードは `frozen_string_literal`
  付きで eval され、その条件では monoruby の方が速い。非 pragma のコードにだけ効く。

## 5. 原因の詳細と対策案

### 5.1 GC とアロケータ（全ベンチで 20〜30 %）

- `GC.stat` より: erubi 1 反復（285 ms）で 56 万オブジェクトを生成し 7.3 回 GC、
  activerecord 1 反復（243 ms）で 44 万オブジェクト・3.8 回 GC。ヒープは 22〜27 ページ
  （256 KB ページ、≈ 6 MB）で、`PAGES_PER_GC_TRIGGER = 8`, `GC_HEAP_FRACTION = 16`
  （`alloc.rs`）のため約 3 万オブジェクトごとにマイナー GC が走る。
- マイナー GC のルート走査が `Root::mark` → `Globals::mark` → `Store::mark` で
  **全 ISeq のリテラルと jit_entry の const_map（`store/iseq.rs:448`）、全 ClassInfo** を
  毎回なめる。Rails をロードすると ISeq は数万個あり、`ISeqInfo::mark` 1.4〜1.7 % +
  `ClassInfo::mark` / `Store::mark` の固定費になる（CRuby は ISeq もヒープオブジェクトで
  old 世代になり、マイナー GC では remembered set 経由でしか触らない）。
- RValue の中身（String バッファ、Array/Hash の要素、hashbrown のテーブル）は glibc
  malloc で、activerecord では `malloc` 系だけで 16 %（`Cargo.toml` の `mimalloc`
  フィーチャのコメントにある通り 1 反復 58 万 alloc/free）。
- 空きページを `madvise(MADV_DONTNEED)` で即返す（`alloc.rs::release_page`）ので、
  短命オブジェクトが多いと再利用のたびにページフォルトが起きる（`do_user_addr_fault`
  がマイクロベンチで 5〜6 %、ベンチで 2〜4 %）。
- スレッドローカル `ALLOC` の `RefCell` 借用が 1 アロケーションごとに入る
  （`LocalKey::with` が String 生成マイクロベンチで 6〜11 % self）。

対策:
1. ルートの世代別化: `Store`（ISeq リテラル・クラス）を old 扱いにして、マイナー GC では
   リテラル追加・クラス変更時に remembered set へ入れたものだけを走査する。
2. 初期ヒープ / トリガーを大きくする（`PAGES_PER_GC_TRIGGER` を 32〜64 に。RSS は
   まだ CRuby より小さい）。
3. `release_page` を遅延させる（直近 N 回の GC で再利用されたページは返さない）。
4. `mimalloc` フィーチャの既定化を A/B で検討。JIT のインライン bump 割り当てが
   使えない経路（Rust builtin からの `Value::string` 等）の `ALLOC.with` を薄くする。

### 5.2 String キー Hash（erubi 30 %, rack 18 %, activerecord 13 %）

- ハッシュ関数は `std::collections::hash_map::RandomState`（SipHash-1-3, `hash.rs:128`）。
  `RStringInner` の `Hash` impl はバイト列をそのまま流し（`string.rs:879`）、ハッシュ値は
  String 側にキャッシュされない。probe は rubymap（IndexMap）→ hashbrown の 2 段。
- **String キーはインライン表現の対象外**（`is_inline_key` は packed 値のみ、
  `hash.rs`）。そのため `{"content-type" => "text/plain"}` のようなリテラルは評価のたびに
  `from_literal_pairs` → `RubyMap::with_capacity` + キーの SipHash + hashbrown の
  テーブル確保を行う（155 ns vs CRuby 60 ns。CRuby は事前に作った frozen Hash を
  `duphash` でコピーするだけ）。Symbol キーなら 54 ns で CRuby より速い。
- 1 回の lookup は 20 キーの warm な Hash で 1.27x、小さい Hash では同等。erubi のように
  数千個の Hash に対して cold にアクセスすると probe（`HashTable::find` 11.5 % self）が
  支配的になる。

対策（効果順）:
1. String キーのリテラル Hash を ISeq 側にテンプレートとして持ち、評価時は
   `clone_body`（entries + table の memcpy）だけにする（CRuby の `duphash` 相当）。
2. 短い String 向けにハッシュ関数を差し替える（FxHash / wyhash。`String#hash` の値は
   CRuby もプロセスごとに変わるので互換性の問題はない）。
3. 8 エントリ以下の Hash は CRuby の ar_table 相当（ハッシュ値付き線形探索）にして
   String キーもインライン表現に載せる。
4. `Hash#[]` / `#[]=` の JIT インライン化で `hashindex` 呼び出し自体のオーバーヘッド
   （`Value::unpack`, `is_compare_by_identity`, `frozen_hash_key` の分岐）を減らす。

### 5.3 二項演算サイトのキャッシュが「受信側クラス × 引数クラス」でキーされる（graphql, activerecord）

- `PolyCacheEntry.arg`（`globals/store.rs:1974`）: BinOp / Cmp / Index / StoreIndex サイトは
  第 1 引数のクラスも記録し、JIT は `(recv, arg)` の組でガードする。Integer / Float の
  算術にはこれが必要だが、`Array#<<` や `Hash#[]` のように**引数の型で振る舞いが変わらない
  メソッド**まで引数クラスで多相化してしまう。
- graphql は `children << node` で node クラスが十数種類あり（`polymorphic sites` 表の
  CallId 34611 `<<` Array/Field | Array/InlineFragment | …）、サイトが megamorphic 扱いに
  なって毎回スローパス（`Array#<<` のグローバルキャッシュ引き 712 万回 / 11 秒）。
  activerecord でも `Array#<<` 511 万回、`Hash#[]=` 1006 万回。
- マイクロベンチ: 単相 3.6 ns → 引数 2 クラス交互で 83.5 ns（23 倍）。

対策: 受信側が `Integer` / `Float` 以外に解決したサイトは PMC を recv のみでキーし、JIT
でも recv クラスのガードだけを出す（引数クラスは数値の fast path にだけ使う）。
`Hash#[]` / `Hash#[]=` / `Array#<<` / `Array#[]` は builtin 直呼びに落とす。

### 5.4 VM（インタプリタ）の `[]=` / `<<` / `respond_to?` がキャッシュを使わない（全般）

- `runtime::set_index`（`codegen/runtime.rs:1874`）は Array + Fixnum 添字だけ fast path
  で、それ以外（Hash 含む）は `vm.invoke_method(IdentId::_INDEX_ASSIGN, …)` →
  `find_method` → `check_method_with_refinements` → `GlobalMethodCache::get`。
  `<<` も `bop_entry!(shl_values …)` から同じ経路。JIT でも受信側が確定しないサイトは
  この runtime を呼ぶ（rack で `set_index` が 5.1 % inclusive）。
- `Kernel#respond_to?` builtin は毎回 `check_method` で探索し、未定義なら
  `respond_to_missing?` を Ruby 呼び出しする（rack で `to_path` / `respond_to_missing?`
  が各 260 万回）。

対策: `set_index` / `get_index` / `shl` に Hash / Array 向けの builtin 直呼び fast path
（BOP 再定義チェック付き）を足す。`respond_to?` は builtin 内にクラス × 名前 × バージョン
のキャッシュを持つ。

### 5.5 deopt ポリシーの穴（activerecord）

`--features profile` の 60 秒（240 反復）での deopt 回数:

| サイト | 回数 | 内容 |
|---|---:|---|
| `block in ActiveModel::LazyAttributeSet#fetch_value` [00019] `%3.deserialize(%2)` | 10,036,400 | 属性型ごとに受信側クラスが違う（Integer/String/Text/DateTime/…） |
| `AcceptsMultiparameterTime::InstanceMethods#cast` [00001] `%2 = Hash` | 4,790,098 | 定数参照の const version ガード |
| `ActiveRecord::Delegation::ClassMethods#create` [00006] `%4.new(...)` | 469,561 | Relation / CollectionProxy の 2 クラス |
| `ActiveModel::Type::Integer#out_of_range?` [00004] `%3 <= %1` | 462,562 | `@max = 1 << 63` は常に Bignum |
| `relation_class_for` / `type_casted_binds` / `derive_key` | 各 23 万 | Post / Comment の 2 クラス |

`--features deopt` で 7 反復（62 万 deopt）の原因を分類すると、受信側クラスガード 67 %、
Fixnum ガードに Bignum 3.3 %、`Visitor#visit` の class-set ガード 2.4 %、frame captured
1.8 %、const version 0.3 %、class version 0.1 %。

- `fetch_value` のサイトは deopt ログ上 **「Text を期待して DateTime が来た」14.2 万回と
  「DateTime を期待して Text が来た」14.2 万回**が交互に並ぶ。再コンパイルのたびに
  直近に観測した 1 クラスの単相ガードを出し直しているだけで、1 反復 ≈ 4.7 万回の
  呼び出しが 1 回も JIT で完走していない。原因は `compile/method_call.rs` の
  `RecvMissMode::Learn`（PMC に 2 クラス以上あれば多相パスは試したはずなので plain
  deopt する ratchet）で、PIC（`PIC_WAYS = 4`）の容量を超えるサイト（属性型は 5 種以上）
  では成り立たない。
- `out_of_range?` は `Integer` のガードが Fixnum タグ検査なので Bignum が毎回落ちる
  （コード中のコメントにある通り再コンパイルでは直らない）。`Integer#<=` の Bignum 側を
  deopt ではなく汎用 `cmp` 呼び出しに落とせば済む。
- 1 回の deopt は安い（megamorphic 6 クラスのマイクロベンチで 18.5 ns/呼び出し）ので
  activerecord 全体では数 % と見積もられるが、VM 側で再実行される `super(value)` /
  ブロック引数展開で `to_ary` / `to_a` が String に対して各 458 万回探索されるなど、
  二次的な費用が乗る。

対策: PIC の容量を超えたサイトは受信側クラスに依らない汎用呼び出し（`find_method` +
インラインキャッシュ、deopt なし）を JIT に出す。Integer の Bignum 表現はガードではなく
汎用パスへの分岐にする。

### 5.6 StringScanner が Ruby 実装（graphql）

`stdlib/strscan.rb` の `StringScanner` は Ruby で書かれ、`scan` / `skip` →
`_match_len_at_pos` → `_anchored`（`\A(?:...)` でラップした Regexp を Hash から引く）→
`String#ascii_only?` → `String#__strscan_match`（`builtins/string.rs:3472`）→
`captures_from_pos_no_save` → onigmo `onig_search`（region の確保・解放を伴う）という
段数を 1 トークンごとに踏む。graphql の lexer は 1 トークンあたり `skip` を 2〜3 回呼ぶので、
`match_at` 本体（17.5 %）の周りに同じくらいのオーバーヘッドが付く
（`_match_len_at_pos` 4.6 % + `string_strscan_match` 2.2 % + region / `regex_view` /
`ascii_only` / `RubyMap::hash` ≈ 9 %）。`GvarTable::lookup` 1.6 % は `Lexer#advance` から
の `get_global_var` で、JIT がグローバル変数を毎回テーブル引きしている。

対策: `StringScanner` を Rust builtin にして、region を scanner に持たせ `onig_match`
（アンカー付きの 1 回マッチ）を直接呼ぶ。CRuby の C 実装と同じ形。

### 5.7 例外・catch/throw（rack, activerecord）

- `raise` は `MonorubyErr::new(String)` でメッセージ String を Rust ヒープに複製し、
  `Exception#initialize` へ `handle_keyword` / `coerce_hash_splat_args` を通して引数を渡し、
  `ex_obj` / `_cause` で `IdentId::get_id`（文字列のインターン）を呼ぶ（マイクロベンチの
  perf: `RurubyAlloc::alloc` 15 %, `handle_keyword` 4 %, `IdentId::get_id` 4 %,
  `CallSiteInfo::clone` 1.6 %）。
- `catch/throw` は 2.7 倍遅い。ActiveSupport の callbacks は `catch(:abort)` を使う。

対策: `raise` 時のメッセージ複製とインターンをやめる（IdentId を定数化）、
`Exception#initialize` の kwargs 経路を通さない、`throw` を例外オブジェクトを作らず
`MonorubyErrKind::Throw` だけで巻き戻す。

### 5.8 Fiddle 経由の sqlite3（activerecord 12 %）

`gem/sqlite3` は Fiddle（`builtins/fiddle.rs::fiddle_invoke`）で libsqlite3 を呼ぶ。
1 呼び出しごとに `value_to_carg` / libffi の `classify_argument` / `bits_to_value` と
`SmallVec` の組み立てが走り、`sqlite3_column_*` を列ごとに呼ぶ `read_column` /
`read_row` で積み上がる。CRuby の sqlite3 は C 拡張で直接呼ぶ。

対策: `sqlite3` の hot path（`step` + 全列読み出し）を 1 回の Rust builtin にまとめる。

### 5.9 その他

- `'lit'.freeze`（pragma なし）: `TraceIr::Literal` は非 frozen リテラルを毎回
  `deep_copy_lit` で複製し（`jitgen/compile.rs:408`）、`.freeze` は `Object#freeze` への
  通常呼び出し。CRuby の `opt_str_freeze` 相当（bytecodegen で `"lit".freeze` を frozen
  リテラルに畳む。`emit_frozen_string` はコマンドリテラルと `defined?` にしか使われて
  いない）を足せば 65 → 16 ns。
- `Array#map(&:upcase)`（Symbol#to_proc）が 1.7 倍遅い。`SymbolProcCache` はあるが
  ブロック呼び出し経路が JIT の `yield` より重い。
- `Hash#dup`（rack の `env.dup`）は `HashRef::clone_body` + `RValue::deep_copy` で 1.3 倍。
- `Integer#to_s` を `<<` する形が 1.2 倍。
- 起動（`Bundler.setup`, `require "active_record"`）が 3〜8 倍遅い。ベンチには含まれないが、
  `Kernel#require` / gemspec の `eval` が VM で走る分。

## 6. 優先順位（案）

| 順 | 施策 | 効くベンチ | 見込み |
|---|---|---|---|
| 1 | §5.1 ルート走査の世代別化 + GC トリガー拡大 + ページ返却の遅延 | 全部 | GC inclusive 8〜14 % の大半 + page fault 分 |
| 2 | §5.2 String キー Hash リテラルのテンプレート化、ハッシュ関数差し替え、小 Hash のインライン化 | rack, erubi, activerecord | Hash 系 13〜30 % の 2〜3 割、リテラル 155 → 60 ns |
| 3 | §5.3 非数値 BinOp サイトの引数クラスキーを外す + `<<` / `[]` / `[]=` の直呼び | graphql, activerecord, rack | `<<` 84 → 4 ns |
| 4 | §5.6 StringScanner の Rust 化 | graphql | 字句解析 1.6x → 1.0x |
| 5 | §5.5 deopt ポリシー（PIC 超過時に汎用呼び出し、Bignum 分岐） | activerecord | 数 % + 二次費用 |
| 6 | §5.4 VM 側 `[]=` / `<<` / `respond_to?` の fast path | rack, activerecord | グローバルキャッシュ引き 1000 万回 / 60 s |
| 7 | §5.7 例外・throw の軽量化 | rack, activerecord | raise 1.7x / throw 2.7x → 1.0x |
| 8 | §5.8 sqlite3 の行読み出しを builtin 化 | activerecord | FFI 12 % の半分 |
| 9 | §5.9 `"lit".freeze` の畳み込み | pragma なしのコード全般 | 65 → 16 ns |

## 7. 計測手順（再現用）

```sh
# CRuby 4.0.6
rbenv install 4.0.6 && rbenv global 4.0.6 && gem install bigdecimal

# yjit-bench
git clone --depth 1 https://github.com/Shopify/yjit-bench.git ../yjit-bench
cd ../yjit-bench/benchmarks/{activerecord,erubi,rack,graphql} && bundle install

# 計測（LANG は必須: erubi の gem_specs.json に非 ASCII があり、US-ASCII だと CRuby が落ちる）
export LANG=C.UTF-8
cargo build --release
ruby --yjit -Iharness-warmup benchmarks/rack/benchmark.rb
MONORUBY_REPROBE=1 target/release/monoruby -Iharness-warmup benchmarks/rack/benchmark.rb

# perf（JIT シンボルは /tmp/perf-<pid>.map に出る。-D で起動を除外）
cargo build --release --features perf --target-dir target-perf
perf record -D 12000 -F 999 -e cpu-clock -g --call-graph=fp -o rack.data -- \
  target-perf/release/monoruby -Iharness-warmup benchmarks/rack/benchmark.rb
perf report -i rack.data --no-children --sort sym --stdio -g none

# deopt / 再コンパイル / PMC 統計（終了時に stderr へ）
cargo build --release --features profile --target-dir target-profile
MAX_TIME=60 target-profile/release/monoruby -Iharness-warmup benchmarks/activerecord/benchmark.rb 2> ar.prof

# deopt 1 件ごとのガード種別と原因（出力が巨大なので短時間で）
cargo build --release --features deopt --target-dir target-deopt
MAX_TIME=12 MIN_ITERS=3 target-deopt/release/monoruby -Iharness-warmup benchmarks/activerecord/benchmark.rb 2> ar.deopt

# JIT コンパイルイベント
cargo build --release --features jit-log --target-dir target-jitlog
```

perf のセルフ時間の分類は、シンボル名に対する正規表現で行った（JIT: `^JIT:<|^monoruby-vm|invoker$|^0x`、
Hash: `rvalue4hash|string_digest|Sip13|rubymap|hashbrown.*HashRef|builtins4hash|hashindex`、
alloc: `_int_malloc|_int_free|^malloc$|cfree|malloc_consolidate|unlink_chunk|RurubyAlloc|Allocator.*5alloc|LocalKey.*4with|finish_grow`、
GC: `4mark|mark_children|mark_contents|GCBox4free|gc_check_and_mark|Allocator.*2gc|drop_glue`、
正規表現: `match_at|onig_|forward_search|mbc_enc_len|onigenc|regex|rmatch|captures|strscan`、
String: `6string|concatenate_string|append_piece|memmove|memcmp|memset|invoke_tos|check_utf8|5split|4join|array_join`、
dispatch: `find_method|GlobalMethodCache|check_method|invoke_method|4args|expand_array|handle_arguments|vm_get_constant|respond_to|IdentId6get_id|Value6unpack|8kernel`、
FFI: `fiddle|ffi_call|sqlite|classify_argument|libffi`）。

このコンテナには `perf` が無いので `apt-get install linux-tools-generic` で入れ、カーネル版が
違うため `/usr/lib/linux-tools/<ver>/perf` を直接叩いた（`perf_event_paranoid = 2` でも
`-e cpu-clock` は採れる）。

## 8. 実施した対策（2026-09-03、コスト順）

§6 の案のうち実装コストの低いものから順に入れた。効果は同じ計測機で baseline
（`750d04f`）と新ビルドを交互に 3 ラウンド走らせた中央値の最小値で比較
（`MAX_TIME=30`、単発計測は ±10 % 揺れるので 1 回の差は信用しない）。

| # | 施策 | 変更箇所 | マイクロベンチ | ベンチへの効果 |
|---|---|---|---|---|
| 1 | GC トリガーの下限を 8 → 32 ページ（≈ 13 万 RValue、8 MB） | `alloc.rs::PAGES_PER_GC_TRIGGER` | — | 下表 |
| 2 | 汎用 `[]=`（VM と JIT の多相残余）に Hash の直接経路 | `runtime::set_index` | — | 下表 |
| 3 | `"lit".freeze` を専用命令 `StringFreeze`（opcode 8）に。`String#freeze` を basic-op 表に追加し、未再定義ならインターン済み frozen リテラルを返す（CRuby の `opt_str_freeze`）。JIT は bop 依存を記録して plain literal load、再定義済みなら VM ヘルパが chilled コピーを作って再定義された `freeze` を呼ぶ | bytecodegen `gen_method_call`、`runtime::string_freeze_literal`、VM 両アーキ、TraceIR/JIT | `'abc'.freeze` 67.6 → 9.8 ns（YJIT 26）、`buf << 'lit'.freeze` 73 → 15.9 ns（YJIT 21） | pragma なしのコードのみ |
| 4 | 引数クラスだけが変わる多相二項演算サイト（`children << node`）に、受信側クラスで分岐する 2-arm dispatch（直接呼び出し + 汎用ヘルパ）。それまでは毎回 `invoke_method` → グローバルメソッドキャッシュ | `compile/binary_op.rs::binary_recv_dispatch` | `ary << x`（4 クラス）57 → 4.5 ns、`<<` のグローバルキャッシュ引き 300 万回 → 0 | 下表 |

施策 1〜3 を入れた時点（v2）:

| ベンチ | baseline（3 回の中央値） | v2 | 差 |
|---|---:|---:|---:|
| erubi | 318 / 310 / 311 ms | 317 / 318 / 310 ms | ±0 % |
| rack | 94 / 91 / 90 ms | 84 / 89 / 91 ms | −6.7 % |
| graphql | 61 / 61 / 59 ms | 59 / 53 / 58 ms | −10 % |
| activerecord | 329 / 341 / 340 ms | 318 / 292 / 311 ms | −11 % |

施策 1〜4 を全部入れた最終ビルド（v3、コミット `265aa91`）:

| ベンチ | baseline（3 回の中央値） | v3 | 差 |
|---|---:|---:|---:|
| erubi | 337 / 316 / 310 ms | 308 / 318 / 334 ms | ±0 % |
| rack | 88 / 92 / 98 ms | 84 / 81 / 82 ms | −8 % |
| graphql | 61 / 60 / 62 ms | 54 / 53 / 52 ms | −13 % |
| activerecord | 334 / 328 / 319 ms | 309 / 301 / 293 ms | −8 % |

（v2 → v3 で graphql がさらに 3 % 前後縮んだのが施策 4 の分。activerecord は
ノイズの範囲。）フルテストは 3688 passed / 1 failed で、失敗は下記の既存の
`Float#ceil` 差分のみ。

- erubi は GC 回数が 1/4 になっても速くならなかった（施策 1 単体の単発計測では
  erubi −13 % に見えたが、交互 3 ラウンドでは差なし。ヒープが 6 → 8 MB になって
  キャッシュ局所性が落ちる分と相殺していると見ている）。GC のコストの本体は
  ルート走査（§5.1）なので、次はルートの世代別化が必要。
- 施策 3 の設計メモ: 最初は「レシーバだけを frozen リテラルにして `freeze` 呼び出し
  は残す」形で入れたが、`String#freeze` を再定義したコードが frozen なレシーバを
  受け取る（CRuby は chilled なコピーを渡す）ので、テストで CRuby と食い違った。
  専用命令 + basic-op 検査に置き換えて解消。もう 1 つ、結果 temp を `push` する前に
  命令を emit すると JIT の抽象フレームがその temp を死んだスロットと見なして
  `load()` で panic する（`emit_call` は push してから emit している）。
- 手元の CRuby は 4.0.6 で、`Float#ceil(15)` の結果が 4.0.2 と違う
  （`1.123456789.ceil(15)` → `1.123456789000001`）ため `float::tests::angle` が
  baseline でも失敗する。本変更とは無関係。
