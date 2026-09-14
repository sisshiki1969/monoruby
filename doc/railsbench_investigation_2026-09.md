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
malloc が付く**のが原因。CRuby は短い String / 小さい Array を埋め込みで持つ。

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
| A | `jit_check_super` を bmethod 境界に対応させる（解決できないときは `method_missing` 相当の汎用呼び出しに落とし、再コンパイルを求めない） | `codegen/jitgen/compile.rs` | `define_method`+`super` のループ解消。マイクロで 25.8 → 7 ms 前後 |
| B | `has_builtin_identity_hash` / `has_builtin_container_hash` を ClassInfo に class_version 付きでメモ化 | `globals/store.rs`, `store/class.rs` | railsbench 55 k Ir/req（1.1 %）、Hash を使う全コードに効く |
| C | `OpenSSL::PKCS5.pbkdf2_hmac` と `HMAC` の反復ループを Rust に落とす（digest 核は既に Rust） | `builtins/digest.rs`, `builtins/cipher.rs`, `stdlib/openssl.rb` | railsbench 約 154 k Ir/req（3.1 %）。Rails の署名 cookie / CSRF に直撃 |
| D | ペイロードの malloc 削減（短い String / 小さい Array の埋め込み、size-class 別フリーリスト） | `alloc.rs`, `value/rvalue/*` | railsbench malloc 295 k Ir/req（CRuby の 2.79 倍）。grape・mail にも同じ構造 |
| E | 生成コードのフットプリント削減（side-exit 領域の共有化、17.5 k 箇所 → 圧縮） | `codegen/` | 命令数比 1.22x に対し実時間比 1.54x の差＝ IPC。i-cache 側の効き |
| F | `chain_deopt_into` が定常状態で 27.8 k Ir/req 走っている理由の確認 | `codegen.rs` | 未調査 |
| G | TZInfo の zoneinfo 読み直し（約 65 k Ir/req）がキャッシュされているかの確認 | 調査のみ | 未確認（プローブが §4 の再帰で潰れた） |

A と B は小さく、C は中規模、D・E は設計が要る。

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

生成コードの帰属には JIT シンボルマップを使う。**valgrind の下では JIT のマップ先が
`0x1_0000_0000` 起点になり、callgrind が `???` に付ける番地と一致する**ので、
`cargo build --release --features perf` したバイナリを callgrind に掛ければ
`/tmp/perf-<pid>.map` をそのまま番地→メソッド名の対応表として使える（CRuby 側は
`ruby --yjit --yjit-perf`。ただしこちらの名前はクラス名を含まない）。

deopt・再コンパイル・グローバルメソッドキャッシュ・GC の統計は
`cargo build --release --features profile,gc-log` のバイナリが終了時に stderr へ出す。
