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
**76 %（58.11 / 76.39）** が消える。消えるのはフレームごとの巡回コストだけで、
変換そのもの（site stub 呼び出し、9,377 Ir/req）は打ち切り位置より手前にあるので残る。
inclusive 43,246 − 9,377 = 33,869 Ir/req の 76 %、**約 26 k Ir/req ＝ 1 リクエストの
約 0.8 %** が見込み。

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

### 2.5.1 walk はそもそも不要 —— 静的に焼ける（検証済み）

§2.5 は「walk を短くする」話だが、**walk 自体が要らない**。コンパイラはコンパイル単位内の
全フレームサイズを把握しているので、変換対象フレームの位置は rbp からの定数変位で出せる。
以下は 2 つの前提の検証結果。

**前提①: ユニット内の全フレームサイズをコンパイラが把握している — 成立（既に実装で使っている）**

`JitContext::specialized_frame_sizes: HashMap<SpecializedId, FrameSizes { total, base }>` に
`pop_frame()` が確定値を記録し、`DynVarOffset::Hint { ids, extra }` を
`resolve_dyn_var_offsets` が `Concrete(usize)` に潰す。その結果が:

```rust
// arch/x86_64/compile/variables.rs — 同一ユニット内の外側ローカルへの書き込み
pub(in crate::codegen::jitgen) fn store_dyn_var_specialized(&mut self, offset: usize, dst: SlotId, src: GP) {
    monoasm!( &mut self.jit,
        movq [rbp + ((offset - (BP_CFP + CFP_LFP) as usize - 8 - conv(dst) as usize))], R(src as _);
    );
}
```

**`movq [rbp + 定数]` 1 命令**で外側フレームのスロットに届いている。汎用版
（`store_dyn_var` → `get_outer`）が `movq rax, [r14]` → `movq rax, [rax]` …
とポインタを辿るのと対照的。つまり①は主張ではなく**既に動いている事実**。

**前提②: 静的なフレーム深さが実行時の CFP 段数と一致する — 成立**

`do_specialized_call`（`arch/x86_64/compile/method_call.rs:344`）:

```rust
self.set_lfp();
self.push_frame();      // 実 CFP フレームを 1 つ押す
monoasm! { &mut self.jit, call entry; }
self.pop_frame();
```

specialized call は必ず実 CFP フレーム 1 つと実 `call` を出す。`trace_contexts()` の
ドキュメントも「every suspended frame of this compilation, outermost first,
**1:1 with `stack_frame`**」と書いている。Loop JIT の根フレームだけは JIT が
プロローグを出さない（既存のインタプリタ / invoker フレームに乗る）が、根は深さ 0 で
`escalate_side_exits() = current_frame_pos() > 0` により escalate しないので、この
経路には現れない。

実測でも裏づけられる。walk 1 回あたりの**ユニット内**変換数のヒストグラム
（railsbench、27,057 walk）は **1 / 2 / 4 の 3 値だけ**:

| ユニット内変換数 | walk 数 |
|---:|---:|
| 1 | 8,000 |
| 2 | 17,057 |
| 4 | 2,000 |

サイトごとにコンパイル時定数、という形をしている。

**唯一の引っかかりだった lfp のヒープ昇格は、静的化の障害にならない**

`move_frame_to_heap` はフレームをヒープに複製し、`cfp.set_lfp(heap_lfp)` で CFP の
LFP スロットを差し替え、スタック側を `set_invalidated()` する。以後 `cfp.lfp()` は
ヒープを指すので、`[rbp + 定数]` でスロットに書くと死んだコピーに書くことになる。

しかし replay stub（`gen_chain_replay_stub`）の 3 引数のうち:

| 引数 | 用途 | 昇格の影響 |
|---|---|---|
| rdi = callee bp | cont pad `[rdi+24]` と戻り番地 `[rdi+8]` の書き込み | なし（必ずスタック側でなければならない） |
| rsi = caller bp | spill 領域の f64 読み出し `[rsi - off]` | なし（spill はマシンスタック。`frame_bytes()` が複製するのは LFP_SELF + 8×reg_num の Ruby ローカル領域だけ） |
| rdx = caller lfp | スロットへの書き込み `[rdx - conv(slot)]` | **あり** |

そして rdx は**静的に算出する必要がない**。LFP は CFP のスロットに入っている
（`BP_CFP = 8`, `CFP_LFP = 8` なので LFP スロットは `bp - 16`）。各フレームの bp が
`rbp + 定数` である以上、

```
movq rdx, [rbp + (K_i - 16)]      ; そのフレームの LFP スロットを読む
```

の **1 ロード**で足りる。昇格済みならヒープのコピーが返るので自動的に正しい。現在の
walk も結局同じ読み出しをしているだけで、違いは `cfp.prev()` を辿ってから読むか、
定数変位で読むかだけである。

（既存の `no_capture_guard` —— `unset_outer_no_capture_guard` のコメント
「a heapified copy of the frame would carry stale LFP slots」—— は同じ危険に対する
ゲートだが、**字句上の**外側チェーンに対して維持されるもので、呼び出しチェーンを扱う
chain deopt にはそのままでは使えない。上の「LFP をスロットから読む」方式なら
ゲート自体が不要になる。）

**今の walk がどれだけ余計に変換しているか**

変換サイトにコンパイル単位 ID を刻んで測った（railsbench 2,000 リクエスト）:

| | /walk | /req |
|---|---:|---:|
| 訪問フレーム | 76.39 | 1,033.48 |
| **deopt したユニット内の変換** | **1.85** | 25.06 |
| **他ユニットの変換** | **13.03** | **176.26** |

変換の **87.6 % がユニット外**。`escalate_side_exits` のコメントの論証
（「Both things escalation buys are confined to a single compilation unit」）に従えば
これらは変換不要で、**1 リクエストあたり 176 フレーム分の有効なコンパイル済み実行を
捨てている**。§8.5 が「strictly more conversion than the speculation will need,
which is sound」と自認している部分の実際の量がこれ。

**キャプチャされたフレームは安全か — 2 つの懸念を潰した**

*(a) `forward_rest` の rbp 相対読み出し*

replay stub の `movq rcx, [rsi]` → `lea rdi, [rcx - rbp_local(src)]` は、変換対象の
さらに 1 つ外側のフレームへの rbp 相対ローカル読み出し。`forward_rest_deferral`
（`context.rs:2849`）のゲートは `is_specialized()` かつ
`forwarding_trampoline_rest(fid)`（＝`def f(...) = g(...)` という形）かつ
`is_simple_call`、`**` splat なし・block 引数なし。つまり読み出し元はトランポリンの
動的呼び出し元 1 段上＝ユニットの根で、読むのは**その呼び出しの引数ウィンドウ**である。

引数ウィンドウは呼び出し直前に書かれる呼び出し元のテンポラリで、呼び出しが返るまで
誰も書かない。名前付きローカルではないので `binding` / dynvar からも届かない。
したがって呼び出し中にフレームがヒープ昇格しても、スタックコピーのウィンドウは正しい
ままで、読み出しは安全。静的化しても `rsi` が `prev_cfp.frame_bp()` から
`rbp + 定数` になるだけで、同じマシン番地・同じ命令列。

実測: `def f(...) = g(...)` を呼び出し元フレーム昇格中に回し、型を途中で変えて
`forward_rest` の materialize を強制するテストを CRuby と照合 —— 21 行一致。
このテストが実際に当該経路を出していることは、`gen_forward_rest_materialize` の
発行を数えて確認した（**12 サイト**）。

*(b) 祖先だけが昇格するケース*

`move_frame_to_heap(L)` は L を複製してから `heap_lfp.outer()` を再帰的に昇格させる。
つまり昇格は**ブロックリテラル / binding が字句上ぶら下がっているフレームから外向きに
しか進まない**。ブロックリテラルの `outer` はそれが書かれたフレームなので、昇格を
引き起こしたフレーム自身が必ず昇格する。そのフレームは呼び出し復帰後に
`pop_frame` → `restore_lfp`（`movq r14, [rbp - 16]`）で生きた LFP を読み直し、
`guard_capture`（`testb [r14 - (LFP_META - META_KIND)], 0b1000_1000`、
`0b1000_0000` = on_heap / `0b0000_1000` = 昇格の tombstone）が meta の汚れを見て
deopt する。`immediate_evict` のコメント「catches ancestor promotions」はこの理由で
成立している。

実測（`store_dyn_var_specialized` が確かに出る形＝aobench 型の `while` +
インラインブロック + Float 外側ローカルで実施し、発行数を数えて確認）:

| 形 | 発行サイト数 | CRuby との一致 |
|---|---:|---|
| キャプチャなし | 1 | — |
| ブロック内で `binding` | **0**（特殊化が抑止される） | 一致 |
| ブロック内で `take { x }`（ブロックリテラルを渡す） | **0**（同上） | 一致 |
| ブロック内で `proc { x }` | **6**（特殊化されたまま） | **一致** |
| メソッド側フレームで `binding` | 0 | 一致 |

`proc { x }` の行が本命で、**最初の反復でキャプチャしたあと特殊化ブロックから
62,499 回書き込んでも、捕まえた Proc は正しい値を読む**。さらにキャプチャした Proc
経由の書き込みをブロック内の書き込みと交互に入れる、`binding.local_variable_set` で
外から書く、2 段ネストにする、といった変種も全て CRuby と一致した。

構造的に非対称になりうる唯一の入口は `materialize_toplevel_binding`（任意のフレームから
main script フレームだけを昇格させる）で、これも直接試して一致した。「A の中で書かれた
ブロックを、A の子ブロックが Proc 化する」という形は Ruby で書けない（ブロック
リテラルの `outer` はそれが書かれたフレームなので、Proc 化すればそのフレームが先に
昇格する）ため、再現は作れなかった。

なおこのアドレッシングは Rails で多用されている（railsbench で **433 サイト**発行）。
つまりゲートは実際に効いている。

**そして静的 chain 変換はこの論点から独立している。** LFP をスロットから読む方式
（`movq rdx, [rbp + (K_i - 16)]`）なので、祖先が昇格していようがいまいが生きたコピーに
書く。現在の `store_dyn_var_specialized` の `[rbp + 定数]` 直接アクセスより厳密に安全。

**実施済み: walk をユニット内に限定した**

まず walk の**範囲**だけを直した（アドレッシングの静的化は次段）。
`JitContext::escalate_side_exits`（bool）を `chain_deopt_frames() -> u32`
（＝`current_frame_pos()`）に置き換え、`AsmIr` が従来どおり各 side exit に焼き込み、
ハンドラが `runtime::chain_deopt(vm, frames)` に渡して walk をそこで止める。
BOP 再定義（`check_bop_redefine`）は `None` を渡して従来どおり底まで走る。

境界の前提は毎回検査する。bounded loop の中の `debug_assert!` が、ユニット内の
フレームの戻り番地は「変換されるか、VM コードとして読める（前回の walk が変換済み）」
のどちらかでなければならないことを確かめる —— そうでなければ静的深さと実行時チェーンが
ずれており、walk が根に届かずに止まっていることになる。全スイートで発火しなかった。

callgrind（同一手法で取り直したペア）:

| | Ir/req |
|---|---:|
| ベースライン | 3,084,280 |
| **ユニット内に限定** | **3,004,061（−2.60 %）** |

| 関数 | before | after |
|---|---:|---:|
| `chain_deopt_into` self | 32,175 | **3,336**（−89.6 %） |
| `CodePtr::add` | 8,319 | **456**（−94.5 %） |
| 生成コード全体 | 526,629 | 506,766 |
| `runtime::vm_get_constant` | 4,137 | 1,146 |
| `Executor::const_lexical_self_key` | 1,493 | 410 |
| `runtime::args::fill_positional_args` | 31,584 | 30,021 |
| `GlobalMethodCache::get` | 23,216 | 22,253 |

walk そのものが消えただけでなく、**インタプリタ側のヘルパも軒並み減っている**
（`vm_get_constant`、`const_lexical_self_key`、`expand_array`、`find_method`、
`fill_positional_args`）。ユニット外のフレームを VM に落とさなくなった分、それらが
コンパイル済みのまま走り続けている効果で、§2.5.1 の「過剰変換 176 フレーム/req」が
実際にコストだったことの裏返しである。

実時間はベースライン中央値 1.751 → 1.658 ms/req。方向は一致するが ±5 % の中。

回帰チェックはすべてベースラインと同一（新規失敗ゼロ）: `cargo test --workspace
--release` 3993 passed / 1 failed（既知の `angle`）、`-C debug-assertions=yes` でも同じ、
`cargo check --target aarch64-unknown-linux-gnu`、ruby/spec core の array / hash /
string / proc / method / enumerable / exception / range / integer / float / binding /
kernel / class / module、`benchmark/*.rb` の出力（`app_aobench` と `trick` は
`srand` 未固定とアニメーションのため元々非決定的。`srand(0)` を入れた aobench は
ベースライン・変更後・CRuby の 3 者が md5 一致）。

**次段（未実施）**: 残る d 回のテーブル引きも、`store_dyn_var_specialized` と同じ
rbp 定数変位に置き換えられる。`chain_deopt_table`（HashMap）、`check_vm_address`、
`cfp.prev()` / `Cfp::return_addr()`、`runtime::chain_deopt` と `CODEGEN.borrow_mut()`
がこの経路から全部消える。d は 1〜4 なので残コストは小さく、優先度は低い。

**`Error` exit の unwind もユニット内で閉じている（確認済み）**

`doc/chain_deopt.md` §8.4 は「a rewritten return-address slot is also on the unwind
path」と書いていて、ユニット外のフレームにも書き換えが要るように読めるが、要らない。

unwind の実体は `entry_raise`（`arch/x86_64/jit_module.rs:80`）:

```
raise:
    ... call handle_error       ; rax: Option<Value>, rdx: Option<BytecodePtr>
    testq rdx, rdx
    jne  goto                   ; このフレームに handler があれば そこから再開
    leave
    ret                         ; 無ければ 通常のエピローグ + ret（rax = 0 がエラー信号）
```

つまり**戻り番地スロットを経由して `ret` する**。書き換えられていないスロットなら、
呼び出し元のコンパイル済み post-call エラーチェックに落ちる —— chain deopt 以前から
ある通常の JIT エラー経路である。§8.4 が言っているのは「*変換済み*フレームを
unwind が通っても stub が `rax == 0` で正しく振る舞う」ことであって、より多くの
フレームを変換せよ、ではない。

そしてこの「未変換の JIT フレームを unwind が通る」経路は既に**通常経路**である:
`escalate_side_exits() = current_frame_pos() > 0` なので、**ユニットの根で起きた raise は
escalate しない**。その時点でサスペンド中の JIT 呼び出し元は未変換のまま unwind が
通り抜ける。walk をユニット内に限るということは、深さ > 0 の Error exit を、深さ 0 の
Error exit が既にやっていることに揃えるだけである。

コードベース自身がユニット外の扱いを明言してもいる
（`context.rs:2722` `method_caller_specialized_ids`）:

> the final `ret` returns from the home to its **dynamic** caller, whose post-call
> frame pop is rbp-derived and therefore correct **no matter how many inlined frames
> were flown over**.

`method_return_specialized`（`lea rbp += Σ; leave; ret`）は、ユニット内の
インラインフレームを**静的オフセットで一気に飛び越えて**ユニット外の動的呼び出し元へ
`ret` する。①②の主張が既に別の形で実装されている 3 つ目の例であり、同時に
「ユニット外のフレームは unwind に対して何も要求しない」ことの直接の証拠でもある。

§8.4 が `method_return_specialized` について言う「the slot belonging to the
*outermost* inlined call」は、深さ 0→1 の呼び出しのスロット（深さ 1 のフレームにある）
で、これは深さ 0 のサイトの replay stub が書き換える。d 回のループは深さ d−1 … 0 を
カバーするので、**含まれている**。

なお同じコメントは、無条件 escalation が `throw` ベンチで
「**`return` 1 回につき chain-deopt walk 1 回**」を招いたため、静的 teardown の適用範囲を
広げて回避した経緯も記録している。walk のコストは既知で、静的なユニット内フレーム算術が
このコードベースでの定石になっている。

実測: raise / rescue をインラインフレームの各段で起こす、`ensure`、`retry`、
ブロックからの `return` / `break`、`throw`/`catch`、`ensure` 付き `return` の
40 行バッテリーが CRuby と一致。

（`method_caller_specialized_ids` は `check_exception_handler(begin, end)` で、飛び越える
フレームに handler があれば静的 teardown を諦める。静的 chain 変換はフレームを
飛び越えず 1 段ずつ変換するので、このゲートは不要。）

---

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
| **F（実施済み）** | escalated side exit の chain deopt を**コンパイル単位内に限定** —— 変換するフレーム数（= `current_frame_pos()`、コンパイル時定数、実測 1〜4）を side exit に焼き込み、walk をそこで止める。BOP 再定義の経路だけ従来どおり底まで走る | `jitgen/context.rs`, `jitgen/asmir.rs`, `codegen.rs`, `codegen/runtime.rs` | railsbench **3,084,280 → 3,004,061 Ir/req（−2.60 %）**。`chain_deopt_into` self 32,175 → 3,336（§2.5.1） |
| ~~G~~（棄却） | TZInfo の zoneinfo 読み直し | — | **読み直していない**。実測 0.04 %/req。§1.3 の帰属誤り（§2.6） |

A・B・C・F をこのブランチで実施した。callgrind で測った railsbench の命令数は

| | Ir/req | |
|---|---:|---|
| master（A・B 前） | 3,204,229 | |
| ＋ A・B | 3,132,872 | −2.23 % |
| ＋ その後の master（C の測定基準） | 3,157,820 | |
| ＋ C | 3,086,274 | −2.27 % |
| ＋ その後の master（F の測定基準） | 3,084,280 | |
| ＋ F | **3,004,061** | −2.60 % |

A・B の内訳は `GlobalMethodCache::get` 29.1 k → 23.0 k、
`check_method_for_class_with_version` 25.6 k → 20.2 k（どちらも −21 %）、
JIT コンパイラ（`Codegen`）38.1 k → 31.8 k（−17 %）。C の内訳は §2.2、F は §2.5.1。

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

---

## 7. 再調査（2026-09-15、master `169ec687`）

A〜F を入れた後の master で、同じ手法（§6）に加えて cache シミュレーション・JIT
シンボルマップ帰属・profile 統計・時系列計測を足して取り直した。結論から:

**命令数ではもう負けていない。負けているのは (1) 1 リクエストあたりのオブジェクト数
（1.73 倍）とその malloc/free、(2) 数か所の「Ruby で書かれた C 拡張」経路
（`gsub(regex, Hash)`——`CGI.escapeHTML`・JSON エンコーダ・`URI`——、`method_missing` 連鎖・キーワード引数）、(3) 学習しない JIT サイト
（`Hash#[]` のキークラス）、(4) 直線的に増え続ける RSS（≈ 1.2 KB/req）、そして命令数が
同じなのに実時間が 1.4 倍になる理由としての (5) 命令キャッシュミス（+33 %/req）である。**

### 7.1 数字

| 指標 | monoruby | CRuby+YJIT | 比 |
|---|---:|---:|---:|
| Ir/req（WARM=1,000 直後、§6 の差分法） | 3,057,452 | 3,038,397 | **1.01x** |
| Ir/req（定常状態、WARM=12,000、§7.2 の注意） | 3,293,410 | ≈ 2.87 M（GC 除き 2,653,417 実測、GC ≈ 218 k 推定） | **≈ 1.1〜1.15x** |
| 実時間 公式ハーネス（MAX_TIME=40、2,000 req/iter の中央値） | 3,012 ms（1.506 ms/req） | 2,149 ms（1.075 ms/req） | **1.40x** |
| 実時間 small.rb（warm 2,000 + 計測 2,000） | 1.41 ms/req | 1.30 ms/req | 1.08x |
| オブジェクト確保（`GC.stat[:total_allocated_objects]`） | **1,590 個/req** | 919 個/req | **1.73x** |
| malloc 呼び出し | 3,573 回/req | 1,071 回/req | 3.34x |
| RSS（ハーネス中央値） | 309 MiB | 137 MiB | 2.25x |
| RSS の伸び（`blocks.rb`、2,000 req ごと） | **+1.2 KB/req、100k req で 282 → 401 MiB、頭打ちなし** | 125 → 133 MiB で安定 | — |

> **CRuby の命令数について 2 つの注意。** (a) `--yjit-perf` を付けた CRuby は
> 3,289,171 Ir/req で、付けないと 3,038,397 —— perf マップの書き出しが **+250,774
> Ir/req（8.3 %）** も乗る。§1.2・§5 の CRuby 側の数字は付けて測っていたので、その分
> 過大だった。本節の CRuby の Ir はすべてマップ無しの値。monoruby 側の `--features
> perf` は 3,041,248 でほぼ無料。(b) 早期（WARM=1,000）と定常状態で CRuby の値は
> 大きく違う（下の §7.2）。

### 7.2 実時間の食い違い: CRuby は 20,000 リクエストかけて速くなる

公式ハーネス（1.40x）と small.rb（1.08x）が食い違うのは、CRuby 側のウォームアップが
長いためである。2,000 リクエストごとに区切って測ると（`blocks.rb`）:

| ブロック（×2,000 req） | monoruby ms/req | monoruby GC 回 | CRuby ms/req | CRuby GC 回 |
|---:|---:|---:|---:|---:|
| 0 | 1.58 | 10 | 1.71 | 27 |
| 1 | 1.45 | 10 | 1.27 | 24 |
| 3 | 1.54 | 11 | 1.30 | 19 |
| 6 | 1.52 | 10 | 1.16 | 14 |
| 9 | 1.50 | 10 | 1.16 | 13 |
| 11 | 1.51 | 10 | **1.03** | 11 |

monoruby は 2,000 リクエストで定常状態に入り以後平坦（GC も 1 回/200 req で一定）。
CRuby はヒープの自動調整で GC が 27 → 11 回/2,000 req まで減り続け、24,000 リクエスト
時点で 1.03 ms/req に達する。**定常状態どうしの比較は 1.50 vs 1.03〜1.07 ms/req ≒
1.4 倍**で、公式ハーネスの数字が正しい。§1.2 の「1.51 vs 0.98」も同じ。

§6 の差分法は WARM=1,000 の直後を測るので、CRuby の GC がまだ多い時点の値になる。
CRuby の GC バケットは早期で 496 k Ir/req（16 %）あり、定常状態では GC 回数が 2.5 分の
1 になるぶん Ir も減る。

**定常状態を差分法で取るときの注意**: WARM=12,000 で N=100 / N=600 の 2 回を引き算すると、
CRuby の GC バケットが **−1,201,462 Ir/req** という負の値になった。12,000 リクエストの
ウォームアップの中で major GC（1 回 ≈ 1 G Ir）が N=100 側にだけ落ちたためで、2 回の
ウォームアップが等価という差分法の前提が GC のタイミングで崩れる。そこで GC を除いた
値と GC の見積もりに分ける:

| CRuby+YJIT、Ir/req | 早期（WARM=1,000） | 定常（WARM=12,000） |
|---|---:|---:|
| GC を除く（実測） | 2,542,055 | 2,653,417（+4 %、うち JIT コンパイラ 91 k → 17 k、VM +97 k） |
| GC（早期は実測、定常は GC 回数 12.5 → 5.5 回/1,000 req で按分） | 496,343 | ≈ 218,000 |
| 合計 | 3,038,397 | **≈ 2.87 M** |

monoruby は GC 回数が最初から一定（1 回/200 req）で GC バケットも 246 k で変わらないが、
合計は 3,057,452 → **3,293,410（+7.7 %）**になる。増分は生成コード +34 k・VM +34 k・
malloc +16 k と分散していて特定の関数に集中しない。実時間は §7.2 の表のとおり平坦なので、
差分法の 2 回のウォームアップが等価でないこと（表の拡張、§7.7 の 24,000 個増えた
シンボル表など）が乗っていると見る。どちらの取り方でも **定常状態の命令数は monoruby が
CRuby の ≈ 1.1〜1.15 倍**（早期の同条件では 1.01 倍）で、実時間の 1.4 倍との差の残りが
§7.6 の命令キャッシュである。

### 7.3 サブシステム別（早期、Ir/req）

| バケット | monoruby | % | CRuby+YJIT | % | 比 |
|---|---:|---:|---:|---:|---:|
| JIT 生成コード | 511,812 | 16.7 | 339,583 | 11.2 | 1.51x |
| libc malloc/free | **367,232** | 12.0 | 118,958 | 3.9 | **3.09x** |
| Hash | 345,947 | 11.3 | 416,736 | 13.7 | 0.83x |
| VM / 呼び出し / runtime | 250,132 | 8.2 | 457,332 | 15.1 | 0.55x |
| GC + オブジェクト確保 | 246,693 | 8.1 | 496,343 | 16.3 | 0.50x |
| String | 237,873 | 7.8 | 220,520 | 7.3 | 1.08x |
| Regexp | 218,212 | 7.1 | 206,341 | 6.8 | 1.06x |
| Value 操作（unpack など） | 124,498 | 4.1 | — | — | — |
| メソッド / 定数探索 | **114,644** | 3.7 | 58,218 | 1.9 | **1.97x** |
| builtins（Rust） / （C） | 104,011 | 3.4 | 138,980 | 4.6 | 0.75x |
| Digest | 101,794 | 3.3 | 144,331 | 4.8 | 0.71x |
| libc mem\* | 69,693 | 2.3 | 33,464 | 1.1 | 2.08x |
| JIT コンパイラ | 22,045 | 0.7 | 91,498 | 3.0 | 0.24x |
| ivar / shape | 11,136 | 0.4 | 30,161 | 1.0 | 0.37x |
| その他 | 331,731 | 10.8 | 285,934 | 9.4 | 1.16x |
| **合計** | **3,057,452** | 100 | **3,038,397** | 100 | 1.01x |

呼び出し・GC・ivar・Hash で勝ち、malloc（3 倍）・メソッド探索（2 倍）・生成コード
（1.5 倍）で負けている、という §1.2 の構図は変わっていない。ただし合計はもう同じ。

### 7.4 ボトルネック（証拠つき、コスト順）

すべて callgrind の差分法（§6）で、呼び出し元は JIT シンボルマップで名前に解決した。

#### 7.4.1 `gsub(regex, HASH)` が Ruby で書かれた C 拡張の経路 —— 175 k Ir/req（5.7 %）

`String#gsub` / `gsub!` の置換が Hash のとき（`replace_all_hash`）が 1 リクエストあたり
175 k Ir。呼び出し元は 3 か所（`gsubprobe.rb`: `String#gsub` / `gsub!` を差し替えて
Hash 置換の呼び出し元を `caller_locations` で集計）:

| 呼び出し元 | 回/req | 平均長 | 実測 Ir/req |
|---|---:|---:|---:|
| `CGI.escapeHTML` —— `monoruby/stdlib/cgi/escape.rb:80` の `string.gsub(/['&\"<>]/, TABLE)`。CRuby は C 拡張 `optimized_escape_html`（5.3 k Ir/req） | 21 | 短い | **34 k**（H の実測差分） |
| `ActiveSupport::JSON::Encoding::JSONGemEncoder#encode` —— `json.gsub!(/>\|<\|&/, ESCAPED_CHARS)`（生成した JSON 全体を走査） | 2 | 261 B | ≈ 140 k（残り） |
| `URI._encode_uri_component` —— `str.gsub!(/[^*\-.0-9A-Z_a-z]/, TBLENCURICOMP_)`（CRuby の `uri/common.rb` も同じ実装） | 1 | 336 B | 〃 |

`replace_all_hash` の内訳（Ir/req）:

| | |
|---|---:|
| `FindCaptures::next`（onigmo の capture 付き反復） | 111,700 |
| `splice_all` | 12,369 |
| `lookup_hash_replacement` —— 1 マッチごとに **`Hash#[]` をメソッド呼び出し** | 12,135 |
| `save_capture_special_variables` —— 1 マッチごとに **MatchData を確保して `$~` を設定** | 8,301 |
| `regex_view` / `string_snapshot` | 12,313 |
| 合計 | **≈ 175,000** |

当初はこの 175 k をすべて `CGI.escapeHTML`（21 回/req で最多の呼び出し元）に帰属させて
いたが、H（escapeHTML をバイト走査に）の実測差分は −34 k で、大半は 2〜3 回/req の
**長い文字列に対する `gsub!`** だった（`>|<|&` の 3 択と否定文字クラスは onigmo の
先頭バイト最適化が効きにくく、1 バイトあたり ≈ 140 Ir）。残り ≈ 140 k の対策は
`gsub(regex, Hash)` 一般: hash が `default_proc` も `default` の再定義も持たない素の
Hash なら `Hash#[]` を直接引き、MatchData と `$~` の設定は最後の 1 回だけにする。

#### 7.4.2 `method_missing` 連鎖（Rails の config アクセス）—— 17 回/req、154 k Ir/req（5.0 %）

`ActionController::Base.logger`（12.7 回/req）、`#csrf_token_storage_strategy`、
`#logger`、`LogSubscriber.flush_all!` などの config 由来アクセサが
`ActiveSupport::OrderedOptions#method_missing` → `Hash#[]` ミス → `default` →
`InheritableOptions#method_missing` → 親、と連鎖する。1 回あたり **9 k Ir**。連鎖の
各段で monoruby 固有のコストが積み上がる:

| 段 | 回/req | Ir/req | 何が高いか |
|---|---:|---:|---|
| `Symbol#to_s`（`+name.to_s`） | 46 | 24,000 | `get_ident_name_clone()` で String を clone → さらに `RStringInner` にコピー、`symbol_encoding()` で read lock をもう 1 回 |
| `String#to_sym` / `try_symbol_or_string` → `IdentId::get_id` | 207 | ≈ 50,000 | `rev_table` が `RandomState`（SipHash）: `get_id` 19.4 k + `sip::Hasher::write` 19.3 k + `hash_one` 11.8 k |
| `Hash#[]` ミス → `IdentId::get_id("default")` を**毎回**実行（`builtins/hash.rs:959`） | 36 | ≈ 9,000 | 文字列リテラルの再インターン。定数 IdentId にすれば 0 |
| `default` / `method_missing` / `respond_to_missing?` のグローバルキャッシュ表引き | 53 | ≈ 6,000 | §2.3 と同じ、サイト別インラインキャッシュがない |
| `invoke_method_missing` 自身: `cs.kw_args.clone()`（IndexMap の clone）+ `args_to_vec` | 17 | ≈ 15,000 | 呼び出しごとの clone と Vec |

対策は段ごとに小さい: `Symbol#to_s` を 1 回の read lock で借用から直接生成（−20 k）、
インターン表を FxHash に（−30 k）、`"default"` を定数 IdentId に（−9 k）、
`invoke_method_missing` の clone を借用に（−10 k）。

#### 7.4.3 引数マーシャリング —— 122 回/req、158 k Ir/req（5.2 %）

JIT が特殊化できない呼び出し（キーワード引数・splat・`method_missing`）は
`jit_generic_set_arguments`（62 回、114 k）と `set_frame_arguments`（60 回、44 k）を通る。
中身はほぼキーワード引数:

| | 回/req | Ir/req |
|---|---:|---:|
| `handle_keyword` | 76 | **90,969** |
| ├ `CallSiteInfo::clone`（`hash_splat_and_kw_rest` の `globals[caller].clone()`、`args.rs:1396`） | 45 | 25,507 |
| ├ kwrest の `RubyMap` 構築 | 42 | 16,233 |
| ├ `free`（その解放） | 116 | 9,905 |
| ├ `kw_names().to_vec()` ほか | 76 | ≈ 5,000 |
| `fill_positional_args` | 123 | 21,236 |
| `coerce_hash_splat_args`（`**opts` ごとに Hash を新規確保） | 83 | 9,174 |

キーワード引数 1 回あたり 1,200 Ir。CRuby はキーワードをスタック上で渡し Hash を作らない。
対策: `CallSiteInfo` を clone せず借用する（−25 k、一番安い）、callee に kwrest が
無ければ `RubyMap` を作らない（−16 k）。

#### 7.4.4 オブジェクトが 1.73 倍 —— malloc/free 367 k、GC 247 k Ir/req

1,590 個/req のうち名前で辿れた主な生成元:

| 生成元 | 個/req | 備考 |
|---|---:|---|
| `runtime::create_array` | **527** | CRuby の Array 生成は ≈ 190/req |
| ├ `ActiveSupport::Callbacks::CallbackChain::DefaultTerminator#call` | 92 | `catch(:abort){}` と `result_lambda.call` の引数パック |
| ├ `ActiveSupport::OrderedOptions#method_missing(method, *args)` | 78 | `*args`（CRuby も確保する） |
| ├ **`Hash#each`（`builtins/hash.rb:38` の `yield [__key_at(i), __value_at(i)]`）** | 69 | CRuby はブロックが 2 引数なら pair Array を作らない（`rb_yield_values(2, k, v)`） |
| ├ `Class#new(...)` の転送 | 39 | 共有本体の `(...)` が rest を実体化 |
| ├ `FileHandler#file_readable?` / `OptimizedUrlHelper#call` / `Array#each` / `silenced?` | 28 / 28 / 26 / 22 | |
| キーワード引数の Hash（`r2k_hash` 38 + kwrest `RubyMap` 20 + `coerce_hash_splat` 83） | ≈ 140 | 7.4.3 |
| `move_frame_to_heap`（`generate_proc` 20 + `generate_lambda` 10） | 35 | CRuby の `rb_imemo_new` 76/req に相当。差ではない |
| `MatchData`（`save_capture_special_variables`、regex 操作ごと） | 33 | `gsub`/`match?`/`=~` 1 回ごと |
| `Symbol#to_s` / `String#b`（`stdlib/openssl.rb` の Ruby 実装） / `dup` | 46 / 41 / 21 | |
| `RValue::new_object_with_ivar_capacity` | 44 | |

malloc 3,573 回/req の呼び出し元は `__rust_alloc` 1,034（`RawVec::finish_grow` 325、
`String::clone` 143、`CallSiteInfo::clone` 92、`SmallVec<[u8;32]>::try_grow` 53、
`invoke_method_missing` 40、`move_frame_to_heap` 28）、`__rust_realloc` 366、
`onig_region_new/resize` 190、`new_object_with_ivar_capacity` 32、`Hash promote` 23。
free 5,682 回/req の 1,776 回は GC の sweep（`Allocator` からの payload 解放）。

monoruby 自身の malloc ラッパ（`__rust_alloc` の `malloc_hard_limit` 22.9 k +
`MALLOC_AMOUNT` 追跡 31 k）で ≈ 54 k Ir/req（1.8 %）を使っている点も小さくない。

#### 7.4.5 学習しない JIT サイト —— deopt 48 回/req、うち 11 回は `Hash#[]` のキークラス

profile ビルド（4,000 req）の deopt 統計を 1 リクエストに割り戻すと:

| サイト | 回/req | 種別 |
|---|---:|---|
| `block in Fanout#listening?` の `silenced?` | 6.0 | POLYMORPHIC（EventObject ほか、PMC 溢れ） |
| `TagBuilder#tag_options` の `%4.[%1]` | 4.1 | **[Hash][String] のクラスガード** |
| `IsolatedExecutionState.[]` の `%2.[%1]` | 4.0 | **[Hash][String]** |
| `ConnectionHandling#connection_specification_name` | 4.0 | POLYMORPHIC（singleton class 4 種で溢れ） |
| `TZInfo::AnnualRules#apply_rule` | 3.1 | POLYMORPHIC |
| `NonConcurrentMapBackend#[]` の `%2.[%1]` | 3.0 | **[Hash][Array]** |
| `LazyAttributeSet#fetch_value` の `deserialize` | 1.5 | POLYMORPHIC |
| `ZoneinfoReader#make_signed_int64` の `<<` | 1.7 | [Integer][Integer]（Bignum 溢れ） |
| `ret` 系（`InheritableOptions#initialize` ほか） | ≈ 6 | 戻り値の推測外れ |

`Hash#[]` の 3 サイトは**毎回** deopt している（4,000 req で 16,296 回 = 呼び出し回数）。
最小再現:

```ruby
def idx(h, k) = h[k]
sym = {a: 1, b: 2}; str = {"a" => 1, "b" => 2}; ary = {[1] => 1, [2] => 2}
20_000.times { |i| idx(sym, i.even? ? :a : :b) }        # Symbol キーで JIT
100_000.times { |i| idx(str, i.even? ? "a" : "b"); idx(ary, i.even? ? [1] : [2]) }
```

→ `Object#idx [:00001] %3 = %1.[%2] [Hash][Array]` が **200,000 回 deopt**（String
100,000 + Array 100,000 のクラスガード失敗）、再コンパイルは 0 回。メソッド呼び出し
サイトの同じ形（受け手が 2 クラス）は 11 回 deopt して `BecamePolymorphic` で
再コンパイルされるので、**Index サイトだけキーのクラスで再コンパイルの判定が
走っていない**。§5 の「9（非数値の BinOp/Index サイトを引数クラスで keying しない）」
は PMC 側だけで、JIT のガードは残っている。

**I（実施済み）**: `hash_index`（`builtins/hash.rs` のインライン生成器）のキークラス
ガードを、`guard_recv_class` と同じカウンタ付き `BecamePolymorphic` 再コンパイル出口
（`JitContext::arg_miss_deopt`）にした。VM の `vm_save_binary_class` は引数クラスの変化
でも POLY バイトを立てて PMC に (Hash, キークラス) を記録するので、再コンパイル時に
PMC が Hash 受け手に 2 種以上のキークラスを持つサイトはプローブを使わず、キーに
依存しない `hashindex` の直接呼び出しに落ちる（受け手多相で POLY が立っただけの
サイトはキー単相のままプローブを使う）。同じ最小再現: deopt **200,000 → 11 回**、
再コンパイル 0 → 1 回、実時間 0.87 → 0.67 s（2,000,000 反復、−23 %）。

#### 7.4.6 グローバルメソッドキャッシュの表引き —— 390 回/req、115 k Ir/req

`GlobalMethodCache::get` 22.3 k + `check_method_for_class_with_version` 19.5 k +
`find_method` 13.9 k + `check_method_with_refinements` 13.9 k + `hash_method` 4.8 k …。
profile 統計の上位（回/req）: `==`/Object 22、`default`/InheritableOptions 21、
`==`/BOOL 17、`==`/Symbol 15、`[]`/Hash 14、`default`/Rack::Headers 13、
`method_missing`/InheritableOptions 12、**`to_ary`/Integer 12**、`==`/Thread 11.5、
`==`/BasicObject 10、`respond_to_missing?`/String 7、`<<`/String 7、`local_to_utc`/Integer 7、
`respond_to?`/Object 6、`to_s`/Integer 6、`to_str`/Integer 6、`to_ary`/String 5。

`==` の 75 回は Hash のキー比較・`Array#include?` から、`to_ary` の 17 回は
多重代入 / splat の暗黙変換から、`to_s`/`to_str` は文字列補間から。CRuby はこれらを
呼び出しサイトのインラインキャッシュ（`rb_check_array_type` などは `basic_definition_p`）
で済ませる。Rust 側の runtime から呼ぶ `==` / `to_ary` / `to_s` / `default` にサイト別
キャッシュを持たせるのが対策。

#### 7.4.7 文字列補間 —— 140 回/req、191 k Ir/req（6.3 %）

`"#{a}#{b}"` は `runtime::concatenate_string` で、1 回 1,365 Ir。内訳は piece ごとの
`append_piece` 141 Ir（うち `SmallVec::insert_from_slice` 85）、`invoke_tos` 87 Ir
（String は短絡済み、実際にディスパッチするのは 3 回/req）、確保 70、encoding 交渉。
呼び出し元は `FileHandler#each_precompressed_filepath` 28（下記）、`TagBuilder#tag_option`
35、`content_tag_string` 14、`TemplatePath.virtual` 19、`set_cookie_header` 13、
`OpenSSL::Cipher#initialize` 16。piece が平均 4 個で ≈ 600 Ir 分が `append_piece` の
SmallVec 成長。合計長を先に計算して一度に確保すれば半分になる。

#### 7.4.8 `File.file?` / `File.exist?` が `canonicalize`（realpath）—— 9 回/req、syscall

`builtins/file.rs:833,846` は `path.canonicalize()` で判定している。Rails の
`ActionDispatch::FileHandler` が毎リクエスト `public/posts/1.html`・`.br`・`.gz` を
`File.file?` / `readable?` で探すので 9 回/req の realpath（各 3〜5 回の syscall）。
Ir では 13 k だが、**実時間では 1 回 5〜10 µs × 9 ≒ 1 リクエストの 3〜6 %**。
`std::fs::metadata` に置き換えるだけ。

#### 7.4.9 小さいが確実なもの

| | 回/req | Ir/req | 何 |
|---|---:|---:|---|
| `String#unpack("H*")` の `format!("{:02x}")` | 64 | 30,700 | バイトごとに `core::fmt`。nibble テーブルで 1/10 |
| `Regexp#match?` の `expect_symbol_or_string()?.to_string()` | 45 | 27,000 | 対象文字列を**シンボルとしてインターン**してから String に戻す（`regexp.rs:1126`）。§7.7 のリークの原因そのもの |
| `MatchData#[]` の `format!("{sym}")` | 14 | 12,600 | 名前付きグループ参照で IdentId を Display 経由で文字列化 |
| `Encoding::classify` | 90 | 22,400 | 1 回 250 Ir の走査。CRuby の `coderange_scan` は 7.7 k |
| `Value::calculate_hash`（Array を Hash キーにする `hash`） | 27 | 62,000 | `exec_recursive` の HashSet 登録 + 要素ごとの `hash` ディスパッチ。CRuby の 2 倍 |
| `Class#new` 共有本体の megamorphic 溢れ | 15.6 | ≈ 6,000 | §5 の 15 で呼び出しサイトにインライン化した残り |

### 7.5 生成コードのメソッド別比較（Ir/req、名前で束ねた値）

| monoruby（502 k 中） | | CRuby+YJIT（281 k 中） | |
|---|---:|---|---:|
| `Hash#each`（Ruby 実装） | 16,201 | `find@lookup_context` | 58,204 |
| method-invoker | 12,996 | `id@attribute_methods` | 31,687 |
| `Array#each`（Ruby 実装） | 12,473 | `_layout@layouts` | 24,278 |
| block-invoker | 11,989 | `options@request` | 20,377 |
| `Rack::Request::Env#get_header` | 9,307 | `start@notifications` | 16,917 |
| `Callbacks::Filters::Before#call` | 8,196 | `block in call@middleware` | 16,873 |
| `ActionDispatch::Http::URL.path_for` | 8,097 | `info@logger` | 16,251 |
| `IsolatedExecutionState.[]` | 6,801 | `presence@core_ext` | 13,419 |
| `Class#new`（Ruby 実装、2 版） | 11,691 | `block in commit!` | 12,890 |
| `_app_views_posts_show_html_erb` | 6,489 | `create_message` | 8,119 |
| `JSONGemEncoder#jsonify` | 5,548 | `process_action` | 7,667 |
| `_app_views_posts_index_html_erb` | 5,106 | `format@journey` | 6,534 |
| monoruby-vm（インタプリタ tier） | 4,456 | `rb_vm_exec` + `vm_exec_core` | 66,515 |

monoruby 側は 1.5 倍の生成コードが **Ruby で書かれたコア（`Hash#each`・`Array#each`・
`Class#new` で 40 k）と invoker（25 k）** に散っている。YJIT は `find`・`id`・`_layout`
に集中していて、これらは monoruby では ≈ 2 k 以下——インライン化と型特殊化は monoruby
の方が効いている。インタプリタ tier は 4.5 k しかなく、deopt の後は短時間で戻っている。

### 7.6 IPC（cache シミュレーション）

この VM ではハードウェアカウンタが使えない（`perf stat` が `<not supported>`）ので
`valgrind --cache-sim=yes --branch-sim=yes` で代替した（1k Ir あたり）:

| | monoruby | CRuby+YJIT |
|---|---:|---:|
| I1 ミス（`I1mr`） | **36.92**（113,003/req） | 28.02（85,148/req） |
| D1 読みミス（`D1mr`） | 6.71 | 8.66 |
| LL 読みミス（`DLmr`） | 0.34 | 0.67 |
| LL 書きミス（`DLmw`） | 0.18（566/req） | 0.06（182/req） |
| 条件分岐予測ミス（`Bcm`） | **14.48**（44,333/req） | 12.79（38,849/req） |
| 間接分岐予測ミス（`Bim`） | 1.82（5,570/req） | 5.13（15,592/req） |
| データ書き込み（`Dw`） | 186.6（571,028/req） | 167.8（509,798/req） |

データ側は monoruby の方が良い（D1・LL の読みミスが少なく、間接分岐は 3 分の 1）。
悪いのは **命令キャッシュ**で、1 リクエストあたり I1 ミスが 113 k 対 85 k（+33 %）、
条件分岐の予測ミスが 44 k 対 39 k（+14 %）、LL への書き戻しが 3 倍（malloc/free と
GC の sweep が触るメモリ）。命令数が同じで実時間が 1.4 倍なのは、まずこの I1 ミス
+28 k/req（L2 から埋めるとして 1 回 10〜20 cycle → 0.3〜0.6 M cycle ≒ 0.1〜0.2 ms）
で説明がつく。§1.2 で見た「生成コードが 17.5 k 領域・シンボルマップ 91 k 行、YJIT の
2.4 倍」の footprint（E）がそのまま効いている。valgrind の cache モデルは汎用の
I1/D1 32 KB・LL 8 MB なので絶対値は目安だが、両者を同じ条件で比べた相対値である。

### 7.7 RSS が増え続ける

`blocks.rb` で 100,000 リクエスト: 282 → 401 MiB、2,000 リクエストあたり +2.4 MiB
（≈ 1.2 KB/req）で直線的、GC 回数は 10/2,000 で一定、ms/req も一定。CRuby は
125 → 133 MiB で止まる。

どこが増えているかは `GC.stat` で切り分けられる（`leak.rb`、ブロックごとに `GC.start`
してから採取、3 回・計 122,000 リクエスト）:

| | ブロック 0 | ブロック 24（48,000 req 後） |
|---|---:|---:|
| RSS | 282 MiB | 336 MiB |
| `heap_live_slots` | 98,491 | 98,499 |
| `heap_allocated_pages` | 70 | 72 |
| `old_objects` | 88,819 | 88,823 |
| `malloc_increase_bytes` | 242.9 MB | **297.8 MB（+1.14 KB/req）** |

**Ruby ヒープは完全に一定で、増えているのは Rust 側の malloc** である。

JIT ではない: `--features jit-log` でウォームアップ 2,000 リクエストの後に印を打つと、
続く 200 リクエストでコンパイルされたのは `Arel.sql` の 1 件だけ（3,146 件のコンパイル
はすべてブートとウォームアップ中）。

正体は **インターン表**である。`Symbol.all_symbols.size` を 2,000 リクエストごとに見ると
monoruby は 37,978 → 41,960 → 45,942 → … と **+2 個/req** で増え続け（CRuby は 33,936 で
一定）、差分を取ると毎リクエストの新顔は 2 つ:

```
:"7856e18e-86af-4763-bc91-8f570ebfd3e2"          # X-Request-Id の UUID
:"_railsbench_session=4mvOj4mB1O7vAkx3GG...（300 バイト超）"   # Set-Cookie の値
```

monoruby の Symbol は GC されず、`Value::try_symbol_or_string` / `expect_symbol_or_string`
（49 か所: `respond_to?`・`send`・`public_send`・`instance_variable_get`・`const_defined?`・
`method_defined?`・`MatchData#[]` …）が **String を渡されると無条件に `IdentId::get_id`
でインターンする**。CRuby の同じ API は `rb_check_id` —— 既にシンボルとして存在する
名前だけ引き、無ければインターンせずに「無い」と答える —— なので、問い合わせに
一意な文字列を渡しても表は増えない。1 個あたり String の複製 + hashbrown の項目 +
`names` の項目で ≈ 500 B、2 個で ≈ 1.1 KB/req、`malloc_increase_bytes` の傾きと一致する。

`IdentId::get_id` のミス経路に backtrace を仕込んだ scratch ビルドで、両方の呼び出し元は
**`builtins::regexp::match_` = `Regexp#match?`** だった。`regexp.rs:1126` の

```rust
let given = arg0.expect_symbol_or_string(globals)?.to_string();
```

は、マッチ**対象の文字列**をメソッド名と同じ経路で受けている —— String を丸ごと
インターンして IdentId にし、`to_string()` でまた String に戻す。Rails は
`ActionDispatch::RequestId` が `X-Request-Id` を、`Rack::Utils` がクッキー値を
`/…/.match?(str)` で検証するので、リクエストごとに一意な 2 つの文字列が永久に残る。
`Regexp#match?` を動的な文字列に使うアプリすべてで起きる（§7.4.9 の 27 k Ir/req の
正体でもある: 対象文字列の SipHash + 複製 2 回）。最小再現:

```ruby
r = /\A[\w\-]{1,255}\z/
100_000.times { |i| r.match?("request-#{i}") }   # monoruby: Symbol.all_symbols +100,000（CRuby +0）
100_000.times { |i| "request-#{i}" =~ r }        # +0
100_000.times { |i| r.match("request-#{i}") }    # +0
```

対策: 問い合わせ系 API に非インターンの `IdentId::try_get_id(&str) -> Option<IdentId>`
（`rev_table` の read lock 1 回）を通し、`get_id` は定義・代入の経路だけに残す。

### 7.8 副産物

- **`--features profile` のビルドが railsbench で segfault** していた（`guard_fail` が
  未書き込みスロットの raw 0 を `Value` として受け、`debug_class` で NULL を deref）。
  引数を `Option<Value>` にして修正した（本ブランチ）。JIT はクラスガードの対象に
  `None` のスロットを渡すことがある。
- F の後の master で「+53 k Ir/req」に見えた差は、`GCBox::free` / `mark` の差分に GC の
  タイミングが乗ったもの。`GC.stat` で数えた objects/req は F 前 1,592.7 / F 1,589.7 /
  master 1,589.7 で変化なし。§6 の差分法は GC 由来の行を ±10 k 程度揺らす。
- `--yjit-perf` の +250 k Ir/req（§7.1 の注意）。
- `leak.rb` の最初の実行（ブロックごとに `GC.start`）で 18,000〜20,000 リクエスト目に
  1 回だけ **HTTP 500** が返った。その後 24,000・50,000 リクエストの 2 回では再現せず、
  最初の実行は例外テキストを取りこぼしている。GC 直後に限って起きるなら根付け漏れの
  可能性があるので、`GC_STRESS=1` の手動ワークフローで railsbench を回す価値がある。

### 7.9 対策候補（コスト順）

| # | 施策 | 変更箇所 | 見込み（Ir/req） |
|---|---|---|---|
| H（実施済み） | `CGI.escapeHTML` / `h` を Rust のバイト走査に（`String#__escape_html`） | `builtins/string.rs`、`stdlib/cgi/escape.rb` | 見込み −170 k → **実測 −34 k（1.1 %）**。175 k の帰属を誤っていた（§7.4.1）。残り ≈ 140 k は `gsub!(regex, Hash)` 一般（JSON エンコーダ・`URI._encode_uri_component`） |
| I（実施済み） | `Hash#[]` のインライン生成器のキークラスガードを `BecamePolymorphic` 再コンパイル出口にし、PMC がキー多相を示すサイトはキー非依存の直接呼び出しに | `builtins/hash.rs`、`jitgen/compile/method_call.rs`（`arg_miss_deopt`） | 最小再現で deopt 200,000 → 11 回、−23 % 実時間。railsbench の profile 統計から `[Hash][String]` / `[Hash][Array]` の 3 サイト（計 11 回/req）が消えた（§7.4.5） |
| J | キーワード引数: `CallSiteInfo` を clone せず借用、kwrest 不要なら `RubyMap` を作らない | `codegen/runtime/args.rs:1392-1400` | −40 k |
| K | `Symbol#to_s` を 1 lock・0 clone に、インターン表を FxHash に、`"default"` を定数 IdentId に | `builtins/symbol.rs`、`id_table.rs`、`builtins/hash.rs:959` | −55 k |
| L | `Hash#each` でブロックが 2 引数なら pair Array を作らない | `builtins/hash.rb` | −69 個/req の Array |
| M（実施済み） | `File.file?` / `exist?` を `canonicalize`（realpath: 構成要素ごとの readlink）から 1 回の `stat`（`std::fs::metadata`）に。パスは正規化せず生のバイト列で渡す（CRuby の `rb_stat` と同じく、末尾 `/` の通常ファイルは ENOTDIR → false） | `builtins/file.rs` | syscall 数の削減（実時間 3〜6 % 見込み、§7.6） |
| N | `unpack("H*")` の nibble テーブル、`Regexp#match?` のコピー排除、`MatchData#[]` の `format!` 排除 | `string/pack.rs`、`builtins/regexp.rs:1126`、`match_data.rs` | −60 k |
| **O（実施済み）** | `Regexp#match?` / `=~` / `match` が対象文字列をインターンしない（文字列として読む）。問い合わせ系 API（`respond_to?`・`autoload?`・`MatchData` の名前参照）は非インターンの `IdentId::try_get_id`（CRuby の `rb_check_id`）で引く。`respond_to?` は CRuby の `obj_respond_to` の 2 形態（Symbol 名は `(Symbol, bool)` で真偽化、Symbol のない String 名はインターンして第 2 引数と戻り値を素通し）に揃え、`respond_to_missing?` が定数を返す本体ならインターンもしない | `id_table.rs`、`value.rs`、`builtins/{regexp,kernel,module,match_data}.rs` | **シンボル数が 10,000 req で一定（32,671）、RSS・malloc_increase も一定** —— +2 シンボル/req の増加が止まった |
| P | Rust runtime から呼ぶ `==` / `to_ary` / `to_s` / `default` のサイト別インラインキャッシュ | `globals/store`、`executor` | −50〜80 k |
| Q | 文字列補間の一括確保（合計長を先に計算） | `codegen/runtime.rs:893` | −40 k |

H〜N は互いに独立で、それぞれ 1 コミットの大きさ。合計で命令数 −350〜400 k（12 %）、
加えて M の実時間分。P と O は設計が要る。

### 7.10 H・O・I・M の実施結果

§7.9 の H・O・I・M を順に入れた（4 コミット）。同じ差分法（WARM=1,000、N=100/600）と
`small.rb` の実時間（WARM=2,000、N=2,000、3 回の中央値。**両側とも再ビルドして計測**:
`build.rs` が共有 install root にその木の stdlib を入れるので、古いバイナリを新しい
`cgi/escape.rb` に対して走らせると `__escape_html` が無く 500 になる）:

| | Ir/req | 実時間 ms/req |
|---|---:|---:|
| master `b730d15d` | 3,057,451 | 1.468 |
| + H | 3,023,430（−34,021） | — |
| + H, O, I, M | **2,937,444（−120,007、−3.9 %）** | **1.339（−8.8 %）** |

命令数より実時間の方が大きく縮んだのは M（syscall は callgrind の Ir に載らない）。
H から H+O+I+M への −86 k を名前つきシンボルで内訳すると（JIT コードの番地はビルド間で
動くので相殺して −9 k）:

| 内訳（Ir/req） | |
|---|---:|
| ファイルシステム（`realpath`、`Components::next`、`normalize_pathbuf`）—— M | −19,611 |
| インターン（`IdentId::get_id` / `get_name` / `to_string_lossy`）—— O | −10,955 |
| malloc / free（M のパスバッファ、O のインターン文字列と MatchData） | −21,829 |
| deopt 経路（`chain_deopt_into`）—— I | −1,643 |
| VM / JIT コード（I で消えた VM 再実行を含む） | −9,101 |

profile ビルドでは `[Hash][String]` / `[Hash][Array]` の deopt サイト 3 つ（11 回/req）が
統計から消え、`newsyms.rb` のシンボル数は 10,000 req で一定になった（§7.7 のリークは
O で止まった）。`railsbench_check.rb` は 15 チェックすべて通る（出力は変わらない）。

残る大物は変わらず: オブジェクト数（§7.3）、`gsub!(regex, Hash)` の残り ≈ 140 k
（§7.4.1: JSON エンコーダと `URI._encode_uri_component`）、`method_missing` 連鎖（§7.4.2）、
キーワード引数（§7.4.3）、グローバルメソッドキャッシュ（§7.4.6）。

### 7.11 追加した計測手順

```sh
# ブロックごとの ms/req・RSS・GC 回数（CRuby のウォームアップ長を見る）
BLOCKS=12 PER=2000 monoruby benchmarks/railsbench/blocks.rb      # ruby-bench 側に置いた薄いドライバ
# objects/req
WARM=1000 N=1000 monoruby benchmarks/railsbench/alloc_count.rb   # GC.stat[:total_allocated_objects] の差
# 定常状態の callgrind（WARM を上げる）
WARM=12000 N=100 valgrind --tool=callgrind --cache-sim=no ...
# cache/branch シミュレーション（HW カウンタの代替）
valgrind --tool=callgrind --cache-sim=yes --branch-sim=yes ...   # summary: 行の Ir I1mr D1mr DLmr Bc Bcm Bi Bim
```

JIT の帰属は §6 のとおり perf マップで行うが、**YJIT のマップは断片的**で、callgrind
が付ける番地が直前の断片の終端と一致することが多い。完全一致で見つからなければ 4 KiB
以内の直前の項目に倒すと 281 k / 343 k が名前に解決する（残り 62 k は ruby バイナリ側の
シンボル無し領域）。monoruby のマップは 1 メソッド 1 項目で 502 k / 507 k が解決する。
