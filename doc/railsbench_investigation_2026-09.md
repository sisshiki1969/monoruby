# ruby-bench 再計測と railsbench のボトルネック解析（2026-09-14）

調査日 2026-09-14。対象コミットは master **`df95d9d3`**。比較対象は rbenv でビルドした
**CRuby 4.0.6**（`--yjit`）。ベンチ本体は [ruby/ruby-bench](https://github.com/ruby/ruby-bench)
を `harness-warmup`（`MAX_TIME=40`）で走らせ、後半半分の反復の中央値を使う。

計測機は x86-64 / Linux 4 vCPU（コンテナ）。`perf` はこのカーネル用の linux-tools が無く
使えないので、関数別の内訳は **callgrind の差分法**（反復数だけを変えた 2 回のプロファイルを
引き算し、起動とウォームアップを相殺する）で採り、生成コードの帰属には
**`--features perf` の JIT シンボルマップ**（CRuby 側は `--yjit-perf`）を使った。手順は §6。

先行調査: [`mail_lee_grape_investigation_2026-09.md`](mail_lee_grape_investigation_2026-09.md)、
[`activerecord_liquid_il_rack_investigation_2026-09.md`](activerecord_liquid_il_rack_investigation_2026-09.md)、
[`sequel_mail_liquid_investigation_2026-09.md`](sequel_mail_liquid_investigation_2026-09.md)。

---

## 1. 結論（先に要点）

### 1.1 いま負けているベンチ（この機械での実測、ms）

| ベンチ | monoruby | CRuby+YJIT | 速度比 | 差（ms） |
|---|---:|---:|---:|---:|
| grape | 715 | 407 | 0.57 | 308 |
| liquid-render | 103 | 63 | 0.61 | 40 |
| liquid-il | 240 | 153 | 0.64 | 87 |
| **railsbench** | **3016** | **1965** | **0.65** | **1051** |
| rack | 48 | 33 | 0.69 | 15 |
| activerecord | 130 | 91 | 0.70 | 39 |
| erubi | 215 | 152 | 0.71 | 63 |
| psych-load | 1414 | 1143 | 0.81 | 271 |
| sequel | 49 | 41 | 0.84 | 8 |
| etanni | 379 | 337 | 0.89 | 42 |

ポータル（x86-64, `df95d9d`, 2026-09-14 06:16 UTC）の 77 本でも同じ並び。**mail は 0.61 →
0.88 に上がって負け組から外れた**（#1329 の `Encoding.find`）。残る負けは 28 本で、
micro 系（send_bmethod 0.38、30k_methods 0.39、30k_ifelse 0.43）を除くと
**アプリ系（railsbench / grape / liquid / rack / activerecord / erubi）に集中**している。

絶対差がいちばん大きいのは **railsbench（1 反復 1051 ms 差）** で、しかもこれまで
一度も解析していない（以前は起動すらしなかった）。以下はその解析。

### 1.2 railsbench の内訳（1 リクエストあたり）

| 指標 | monoruby | CRuby+YJIT | 比 |
|---|---:|---:|---:|
| 実行命令 | **4.92 M** | 4.04 M | 1.22x |
| └ 生成コード（JIT 済み本体 + VM） | **2.25 M** | 1.12 M | **2.01x** |
| └ libc malloc/free | 308 k | 111 k | **2.79x** |
| └ GC + オブジェクト確保 | 223 k | 530 k | **0.42x** |
| └ VM / 呼び出し | 172 k | 402 k | **0.43x** |
| └ Hash | 290 k | 378 k | 0.77x |
| └ ivar / shape | 12 k | 31 k | 0.38x |
| Ruby オブジェクト確保 | **1,548 個** | 869 個 | 1.78x |
| malloc 回数 | 約 3,840 回 | — | — |
| 実時間 | 1.51 ms | 0.98 ms | 1.54x |

読み方が前回までと逆になっている点が重要:

- **呼び出し・ivar・GC では monoruby が勝っている**（0.38〜0.43x）。インライン化、
  インライン ivar スロット、ページ確保型アロケータが効いている。
- 負けているのは **生成コードそのもの（2.0 倍の命令を実行している）** と **malloc**。
- 命令数比 1.22x に対し実時間比 1.54x なので、残りは IPC（キャッシュミス）。生成コードは
  callgrind が数える単位で **17,539 箇所**（YJIT は 1,442 箇所）に散っており、
  JIT シンボルマップの行数も 91,094 行 vs 37,926 行。**コードフットプリントが 2〜3 倍**ある。

### 1.3 生成コードの中身（Ir/リクエスト、シンボルマップで帰属）

> **この表の数字は上限として読むこと**（2026-09-14 追記）。生成コードの領域は
> callgrind の出力ではアドレスでしか識別されず、そのアドレスは実行ごとに変わる。
> 差分法で N の違う 2 回を引き算するとき、片方にしか現れないアドレスは丸ごと
> 「1 リクエストあたりの増分」として計上されてしまう。railsbench の 2 回では
> 生成コード領域 21,743 / 21,768 個のうち共通なのは 19,418 個で、片方にしか
> 無い領域が 1 リクエストあたり 1.34 M Ir 分の幻の増分を作る。**名前で束ねた
> 合計**（Rust 側のシンボル、および「生成コード」全体）は正しいが、以下の
> 個別の Ruby メソッドへの帰属は過大に出ている。C の実測（§2.2）でこれが
> 判明した。

| 単位 | Ir/req |
|---|---:|
| `Array#map`（monoruby の `builtins/array.rb`、Ruby 実装） | **296,729** |
| `OpenSSL::PKCS5.pbkdf2_hmac` のブロック | 57,312 |
| `Hash#each` | 49,757 |
| `OpenSSL::HMAC#initialize` のブロック | 47,313 |
| `Array#each_index` / `Array#each` | 72,474 |
| `ActiveSupport::Callbacks::Filters::Before#call` | 26,233 |
| TZInfo（`ZoneinfoReader#derive_offsets` ほか 5 メソッド） | 約 65,000 |
| `ActionView::RoutingUrlFor#url_for` | 19,486 |
| `OpenSSL::Digest.canonical_name` / `Digest::Instance#update` ほか | 約 32,000 |

そして Rust 側（シンボルあり）の上位:

| 関数 | monoruby | CRuby の対応 | |
|---|---:|---|---:|
| `match_at`（正規表現） | 105,077 | `match_at` | 121,490 |
| `sha2::sha256::compress256` | 92,492 | `rb_Digest_SHA256_Transform` | 108,343 |
| malloc/free 4 関数 | **295,000** | `_int_malloc`+`_int_free` | 66,000 |
| `GlobalMethodCache::get` + `check_method_for_class_with_version` | 55,000 | — | — |
| `chain_deopt_into` | 27,775 | — | — |

**正規表現と SHA-256 の実装そのものは CRuby と互角**（むしろ速い）。差が出ているのは
その周りの Ruby コードと malloc。

---

## 2. どこが「2 倍の命令」なのか

### 2.1 イテレータ単体では monoruby の方が速い

`Array#map` が生成コードの最上位（13 %）なので「Ruby 実装のイテレータが遅いのでは」と
疑ったが、単体マイクロでは逆だった（1 要素あたり ns）:

| | monoruby | CRuby+YJIT |
|---|---:|---:|
| `Array#map` | **8.0** | 14.9 |
| `Array#each` | **4.8** | 6.2 |
| `Array#each_index` | **4.5** | 28.6 |
| `Hash#each` | **18.3** | 29.1 |

つまり 1 反復あたりの生成コードは悪くない。**実行している Ruby コードの量そのものが
多い**のが 2 倍の正体で、その内訳は §2.2・§2.3。

### 2.2 C 拡張の代わりに Ruby を走らせている分（≈ 137 k Ir/req）

Rails は署名 cookie と CSRF トークンのために、リクエストごとに PBKDF2 と HMAC を通る。
digest の核（SHA-256 圧縮関数）は Rust の `sha2` が CRuby の C と互角（92 k vs 108 k）
だが、**その外側のループが monoruby では Ruby**（`stdlib/openssl.rb` の
`PKCS5.pbkdf2_hmac` と `HMAC#initialize`）であり、CRuby では libcrypto の C である。

| 単位 | Ir/req |
|---|---:|
| `block (2 levels) in OpenSSL::PKCS5.pbkdf2_hmac` | 57,312 |
| `block in OpenSSL::HMAC#initialize` | 47,313 |
| `OpenSSL::HMAC#initialize` / `#digest` | 16,989 |
| `OpenSSL::Digest.canonical_name` / `#finish` / `Digest::Instance#update` / `#digest!` | 32,483 |
| 合計 | **≈ 154,000（リクエストの 3.1 %）** |

**実測（2026-09-14 追記）**: この外側ループを Rust に落として（§5 の C）測り直した
ところ、railsbench の命令数は **3,157,820 → 3,086,274 Ir/req（−71,546、−2.27 %）**
だった。上の 154,000 は §1.3 の注記どおり過大で、実際に消えたのはその半分以下である。
名前で束ねた内訳（Ir/req）は:

| 単位 | before | after |
|---|---:|---:|
| 生成コード全体 | 542,608 | 525,317 |
| `Hashmap::insert` | 11,907 | 1,588 |
| `SmallVec<[u8; 32]>::insert_from_slice` | 25,848 | 18,375 |
| `Value::coerce_to_pack_u64`（`pack("C*")` / `pack("N")`） | 4,689 | 0 |
| `string::pack::pack` | 4,665 | 1,546 |
| `__memcpy_avx_unaligned_erms` | 51,096 | 46,883 |
| `Value::unpack` | 31,900 | 28,104 |
| `_int_malloc` / `malloc` / `_int_free` | 238,196 | 233,414 |

マイクロでは効きがはるかに大きい（PBKDF2 2\*\*16 で 2,143 → 85 ms、HMAC-SHA256 で
222 → 54 ms）。railsbench で 2 % に留まるのは、`ActiveSupport::KeyGenerator` が
派生鍵をキャッシュしていて PBKDF2 がリクエストごとには走らないためで、毎回通るのは
署名 cookie の HMAC だけである。

### 2.3 メソッド探索のハッシュ表引き（≈ 55 k Ir/req）

`--features profile` のグローバルメソッドキャッシュ統計（4,000 リクエスト）を
1 リクエストに割り戻すと:

| name / class | 回/req |
|---|---:|
| `hash` / Array | 37 |
| `==` / Object | 22 |
| `default` / ActiveSupport::InheritableOptions | 21 |
| `==` / BOOL | 17 |
| `==` / Symbol | 15 |
| `[]` / Hash | 14 |
| `default` / Rack::Headers | 13 |
| `hash` / Fiber | 13 |
| `hash` / Thread | 12 |
| `method_missing` / InheritableOptions | 12 |

合計で **1 リクエスト約 150 回**。多くは `Hash` の操作が毎回
`has_builtin_identity_hash` / `has_builtin_container_hash` を通り、その中の
`check_method` が `(IdentId, ClassId)` のハッシュ表を引いているもの
（`GlobalMethodCache::get` 29 k + `check_method_for_class_with_version` 26 k Ir/req）。
コメントは「class version でキャッシュ済み」と書いているが、実体は**毎回ハッシュ表 1 引き**
で、ClassInfo 側のメモ（load + compare）にはなっていない。

### 2.4 malloc（2.79 倍）

1 リクエストで Ruby オブジェクト 1,548 個（CRuby 869 個）、malloc 約 3,840 回。
オブジェクト 1 個につき malloc 2.5 回という比率は grape の解析（§ mail/lee/grape の 3.2）と
同じ構造で、**ペイロード（String バッファ、Array バッファ、Hash テーブル）ごとに
malloc が付く**のが原因。

ただし **「短い String / 小さい Array の埋め込み」は monoruby にも既にある**
（2026-09-14 訂正）。`StringContent` は `SmallVec<[u8; STRING_INLINE_CAP]>`
（`STRING_INLINE_CAP = 32`）、`ArrayInner` は `SmallVec<[Value; ARRAY_INLINE_CAPA]>`
（`ARRAY_INLINE_CAPA = 5`）で、どちらもインラインバッファは `RValue` の中
（`RVALUE_OFFSET_INLINE`）にあり、この範囲なら malloc は起きない。つまり残る
malloc はその閾値を超えたペイロードと Hash テーブルの分で、2.5 回/オブジェクト
という比率の内訳は改めて採り直す必要がある。

### 2.5 chain deopt の walk がスタック全体を毎回歩いている（F、約 43 k Ir/req）

`chain_deopt_into` は §1.3 で Rust 側のシンボルとして出ていた唯一の JIT 内部関数で、
こちらは名前で束ねた値なので**当初の 27.8 k Ir/req という数字は正しかった**
（再計測で self 31,769 Ir/req、`runtime::chain_deopt` からの inclusive 43,246 Ir/req）。

何が起きているかを見るため、walk に一時的なカウンタを入れて railsbench を
2,000 リクエスト測った（WARM=2000。計測後にカウンタは外してある）:

| | 合計 | /リクエスト | /walk |
|---|---:|---:|---:|
| walk 回数 | 27,057 | 13.53 | 1.00 |
| 訪問したフレーム数 | 2,066,961 | 1,033.48 | **76.39** |
| 変換したフレーム数 | 402,636 | 201.32 | 14.88 |
| 戻り番地が cont stub だったフレーム（＝変換済み） | 1,293,121 | 646.56 | 47.79 |
| 最初の変換済みフレームより先で訪問したフレーム | 1,572,210 | 786.11 | **58.11** |
| 最初の変換済みフレームより先での変換 | **0** | 0.00 | 0.00 |

Rails のスタックは深いので、1 回の escalation ごとに **76 フレーム**を歩き、
`chain_deopt_table` を引き直している。そのうち **48 フレームは前回の walk が既に
変換済み**で、`check_vm_address` に弾かれて何もしない。

最後の行が効く: **29,057 回の walk を通じて、最初の変換済みフレームより先で変換が
起きたことは一度もない**。これは仕組みから言えることでもある —— walk は必ず
スタックの底まで走るので、あるフレームの戻り番地が cont stub になっている時点で、
その下は前回の walk が処理し終えている（下＝より古いフレームなので、その間に
JIT フレームが新しく積まれることはない）。

したがって **最初の cont stub フレームで walk を打ち切ってよく**、訪問フレームの
**76 %（58.11 / 76.39）** が消える。inclusive 43,246 Ir/req のうち約 33 k Ir/req、
1 リクエストの **約 1.1 %** に当たる。

escalation の発生元（deopt したフレームのメソッド）の内訳:

| メソッド | walk/req |
|---|---:|
| `block in ActionView::Helpers::TagHelper::TagBuilder#tag_options` | 4.53 |
| `ActiveSupport::InheritableOptions#initialize` | 2.00 |
| `block in ActiveSupport::Notifications::Fanout#build_handle` | 1.00 |
| `SQLite3::Statement#each` | 1.00 |
| `ActionView::OutputFlow#initialize` | 1.00 |
| `ActiveSupport::Callbacks#run_callbacks` | 1.00 |
| `Enumerable#__gather_each` | 1.00 |
| `ActiveRecord::ConnectionAdapters::ConnectionHandler#each_connection_pool` | 1.00 |
| `Hash#each` | 0.99 |

walk 回数そのものを減らす（この 13.5 回の deopt がなぜ定常状態で起き続けるのか）は
別の問題で、まだ追っていない。

### 2.6 TZInfo は毎リクエストの zoneinfo 読み直しをしていない（G、棄却）

§1.3 は TZInfo に約 65,000 Ir/req を割り当てていたが、これは §1.3 の注記どおりの
帰属の誤りだった。gem のメソッドにカウンタを差し込んで 300 リクエスト測ると:

| | /req（CRuby・monoruby とも） |
|---|---:|
| `ZoneinfoReader#read` / `#parse` / `#derive_offsets` | **0** |
| `ZoneinfoDataSource#load_timezone_info` | **0** |
| `TZInfo::Timezone.get` / `DataSource#get_timezone_info` | **0** |
| `ActiveSupport::TimeZone.[]` | 0.667 |
| `ActiveSupport::TimeZone#period_for_utc` → `Timezone#period_for_utc` | 0.667 |

ウォームアップ後は zoneinfo ファイルに一切触っていない。残るのは既にロード済みの
ゾーンオブジェクトに対する変換だけで、単体コストは monoruby で
`period_for_utc` 507 ns、`TimeZone.[]` 204 ns（CRuby+YJIT は 659 ns / 114 ns）。
0.667 回/req を掛けて **約 474 ns/req ≒ 1,150 Ir/req、1 リクエストの 0.04 %**。
**G は対策不要**。

---

## 3. JIT のバグ: `define_method` の本体からの `super` が再コンパイルループになる

`--features profile` の再コンパイル統計で、railsbench 4,000 リクエストに対し
`block (3 levels) in <class:Digest>`（`stdlib/openssl.rb` の
`define_method(:initialize) { |data = nil| super(algo, data) }`）が
**357 回**、理由 `MethodNotFound` で再コンパイルされていた。

### 3.1 最小再現

```ruby
class Base
  def initialize(name, data = nil)
    @name = name
    @data = data
  end
end
algo = "SHA256"
Sub = Class.new(Base) do
  define_method(:initialize) { |data = nil| super(algo, data) }
end
20_000.times { Sub.new }
```

| エンジン | 時間 | 再コンパイル |
|---|---:|---:|
| monoruby | 25.8 ms | **1,814 回** |
| monoruby `--no-jit` | **6.9 ms** | — |
| CRuby+YJIT | 4.5 ms | — |

**JIT を切った方が 3.7 倍速い**。`OpenSSL::Digest::SHA256.new` でも同じ
（JIT 42.0 ms / no-jit 28.9 ms / YJIT 18.3 ms、再コンパイル 1,814 回）。

### 3.2 原因

`jit_check_super`（`codegen/jitgen/compile.rs`）は `super` の解決を

```rust
let mother = self.iseq().mother().0;          // 字句上の親
let mother_fid = self.store[mother].func_id();
let func_name = self.store[mother_fid].name().unwrap();
self.store.check_super(recv_class, mother_fid, func_name)
```

で行う。`check_super` は `recv_class` の祖先チェーンを辿って **`mother_fid` が登録されて
いる位置**を探すが、`define_method` で作られた本体（bmethod）では**メソッド境界はブロック
自身**であり、字句上の親はどのクラスにも登録されていない。したがって必ず `None` になり、
`compile_method_call` は `Recompile(MethodNotFound)` を返す。再コンパイルしても同じ解決を
やり直すだけなので、**閾値を超えるたびに永久に再コンパイルし続ける**。

同じファイルの名前付きメソッド側には「recompile を求めると収束しないので `method_missing`
にフォールバックする」というコメントと実装があり、`super`（`callsite.name` が `None`）の
経路だけがその手当てから漏れている。

VM 側は正しく解決できている（`--no-jit` が速いのはそのため）。CLAUDE.md の
`lexical_owner` の説明にある「`define_method` の本体はそれ自体がメソッド境界」という
規則を、`jit_check_super` にも適用すれば直る。

---

## 4. 副産物: 落ちるケースと環境の注意

- **segfault**: `Array` に `map` を上書きする module を `prepend` し、その中で
  `caller_locations` を呼ぶと、monoruby では `Kernel#caller_locations` が Ruby 実装で
  内部的に `map` を呼ぶため無限再帰になる。小さい再現では `StackOverflow (RuntimeError)`
  を正しく上げるが、Rails を載せた状態では **SIGSEGV**（スタックガードが効かない経路がある）。
  なお素の深い再帰（`def f(n) = f(n+1)`）はきちんと例外になる。
  CRuby は `caller_locations` が C なのでそもそも再帰しない。
- **CRuby 側の環境**: このコンテナは `LANG` 未設定で default external が US-ASCII になり、
  erubi / etanni は CRuby 側が `Encoding::InvalidByteSequenceError` で落ちる
  （`LANG=C.UTF-8` を付ければ通る）。monoruby は UTF-8 既定なので影響を受けない。

---

## 5. 対策候補（コスト順）

| # | 施策 | 変更箇所 | 見込み |
|---|---|---|---|
| **A（実施済み）** | `super` サイトの「フレーム依存」判定を、字句上の親ではなく**コンパイル中の本体自身の `is_proc_method` ビット**で行い、再コンパイルではなく VM への plain deopt に落とす | `codegen/jitgen/compile/method_call.rs` | マイクロ **29.6 → 3.7 ms**（YJIT 4.5、`--no-jit` 6.9）。`SHA256.new` 50.3 → 22.2 ms、再コンパイル 1,814 → 0 |
| **B（実施済み）** | `hash` の解決を ClassInfo の class_version 付き `Cell` にメモ化（`match_method` と同型） | `globals/store.rs`, `store/class.rs` | オブジェクトキー `Hash#[]` **64.5 → 57.3 ns**、Array キー 128.5 → 119.3 ns |
| **C（実施済み）** | `OpenSSL::PKCS5.pbkdf2_hmac` と `HMAC` の反復ループを Rust に落とす（digest 核は既に Rust） | `builtins/digest.rs`, `stdlib/openssl.rb` | PBKDF2 2\*\*16 **2,143 → 85 ms**、HMAC-SHA256 **222 → 54 ms**。railsbench **3,157,820 → 3,086,274 Ir/req（−2.27 %）** |
| D | ペイロードの malloc 削減（size-class 別フリーリスト、Hash テーブル）。**短い String / 小さい Array の埋め込みは実装済み**（§2.4 の訂正） | `alloc.rs`, `value/rvalue/*` | railsbench malloc 295 k Ir/req（CRuby の 2.79 倍）。まず 2.5 malloc/オブジェクトの内訳を採り直す |
| E | 生成コードのフットプリント削減（side-exit 領域の共有化、17.5 k 箇所 → 圧縮） | `codegen/` | 命令数比 1.22x に対し実時間比 1.54x の差＝ IPC。i-cache 側の効き |
| **F（測定済み・対策候補あり）** | chain deopt の walk を、最初の「変換済み（戻り番地が cont stub）」フレームで打ち切る | `codegen.rs` | 訪問フレームの 76 % が消える。約 33 k Ir/req ≒ 1 リクエストの 1.1 %（§2.5） |
| ~~G~~（棄却） | TZInfo の zoneinfo 読み直し | — | **読み直していない**。実測 0.04 %/req。§1.3 の帰属誤り（§2.6） |

A・B・C をこのブランチで実施した。callgrind で測った railsbench の命令数は

| | Ir/req | |
|---|---:|---|
| master（A・B 前） | 3,204,229 | |
| ＋ A・B | 3,132,872 | −2.23 % |
| ＋ その後の master（C の測定基準） | 3,157,820 | |
| ＋ C | **3,086,274** | −2.27 % |

A・B の内訳は `GlobalMethodCache::get` 29.1 k → 23.0 k、
`check_method_for_class_with_version` 25.6 k → 20.2 k（どちらも −21 %）、
JIT コンパイラ（`Codegen`）38.1 k → 31.8 k（−17 %）。C の内訳は §2.2。

> 2026-09-14 訂正: ここには当初 A・B を **4,915,558 → 4,849,888 Ir/req（−1.34 %）**
> と書いていたが、同じ `.cg` ファイルを callgrind の `summary:` 行と突き合わせて
> 数え直すと上表になる。当初の数字は 1 リクエストあたりへの割り戻し方を誤っていた。

実時間はこの機械のばらつき（railsbench で ±5 %、実測 1.23〜1.44 ms/req）に埋もれるので、
確かなのはこの命令数と §5 のマイクロの数字。D・E は設計が要る。

---

## 6. 計測手順（再現）

```sh
git clone --depth 1 https://github.com/ruby/ruby-bench.git ../ruby-bench
cd ../ruby-bench/benchmarks/railsbench && bundle install && RAILS_ENV=production bin/rails db:migrate db:seed
cargo install --path monoruby

cd ../ruby-bench
MAX_TIME=40 monoruby    -I harness-warmup benchmarks/railsbench/benchmark.rb
MAX_TIME=40 ruby --yjit -I harness-warmup benchmarks/railsbench/benchmark.rb
```

関数別は callgrind の差分法。`benchmarks/railsbench/small.rb`（ハーネスを使わず
`WARM` 回ウォームアップして `N` 回だけ計測する薄いドライバ）を N だけ変えて 2 回取り、
`fn=` / `cfn=` / `calls=` を素の Python で読んで引き算する:

```sh
WARM=1000 N=100 valgrind --tool=callgrind --callgrind-out-file=s.cg --cache-sim=no monoruby benchmarks/railsbench/small.rb
WARM=1000 N=600 valgrind --tool=callgrind --callgrind-out-file=l.cg --cache-sim=no monoruby benchmarks/railsbench/small.rb
```

1 リクエストあたりの総命令数は、各 `.cg` の末尾にある `summary:` 行（＝全関数の
self コストの総和）を引いて `N` の差で割る。`fn=` 単位の差分を合計しても**一致しない**:
生成コードの領域はアドレスでしか識別されず、そのアドレスは実行ごとに変わるので、
片方にしか現れないキーを落とすか丸ごと足すかで数万〜百万 Ir/req ずれる（§1.3 の注記）。
名前で束ねる（`0x…` を「生成コード」1 つにまとめる）と `summary:` と一致する。

生成コードの帰属には JIT シンボルマップを使う。**valgrind の下では JIT のマップ先が
`0x1_0000_0000` 起点になり、callgrind が `???` に付ける番地と一致する**ので、
`cargo build --release --features perf` したバイナリを callgrind に掛ければ
`/tmp/perf-<pid>.map` をそのまま番地→メソッド名の対応表として使える（CRuby 側は
`ruby --yjit --yjit-perf`。ただしこちらの名前はクラス名を含まない）。

deopt・再コンパイル・グローバルメソッドキャッシュ・GC の統計は
`cargo build --release --features profile,gc-log` のバイナリが終了時に stderr へ出す。
