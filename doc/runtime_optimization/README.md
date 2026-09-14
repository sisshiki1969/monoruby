# ランタイム最適化 — 目次と共通の前提

`runtime_optimization/` は、Ruby のコアクラスが monoruby の中でどう表現され、
VM と JIT がその上のホットな操作をどう安くしているかを、クラスごとに 1 文書
ずつまとめたものである。各文書は末尾に **CRuby との実装差異**の表を持つ。
すべて日本語。

| 文書 | 内容 |
|---|---|
| [array.md](array.md) | `SmallVec<[Value; 5]>` の inline / ヒープ、`[]` / `[]=` / `<<` / リテラル / 多重代入の VM・JIT 経路、Ruby で書かれた `each` / `map` / `initialize`、`sum` / `sort` / `hash` の注記。共有配列を持たない理由 |
| [hash.md](hash.md) | inline（≤ 3 ペア）/ boxed 線形 / boxed 索引の 3 表現、vm 不要の prehashed probe、機械語の probe（Symbol / String キー）、2 つのハッシュ関数、Ruby で書かれた走査とそのプリミティブ、計測と残課題。`ar_table` / `st_table` との対比 |
| [string.md](string.md) | `SmallVec<[u8; 32]>` と共有部分文字列（隠れ frozen root、copy-on-write）、エンコーディングと code range の O(1) 畳み込み、frozen リテラルと `"lit".freeze`、補間、`<<` / `getbyte` / `setbyte` / `==` の JIT、Hash の String キー |
| [regexp.md](regexp.md) | Onigmo とプロセス全体のパターンキャッシュ、`FrozenLiteral` としてのリテラル、`LFP_SVAR` の `$~` と 40 バイトの `MatchData`、`MatchData` を作らない経路、正規表現を使う `String` メソッドの高速経路、StringScanner |

---

## 共通の前提

以下は 4 文書が暗黙に使う事実で、それぞれの出典を示す。

### RValue は 64 バイト

ヒープオブジェクトはすべて `RValue`（`value/rvalue.rs`）で、8 バイトの
ヘッダ（live / frozen / chilled / GC の OLD・WB_UNPROTECTED・WB_ARMED /
年齢、型タグ `ObjTy`、型別メタデータバイト `ty_flags`、クラス）、8 バイトの
`var_table`（あふれた ivar）、**48 バイトのペイロード共用体** `ObjKind`
からなる。Array / String / Hash はこの 48 バイトに「小さいものは直置き、
大きければヒープ」の 2 段を入れていて、閾値はそれぞれ 5 要素 / 32 バイト /
3 ペアである。値の表現（Fixnum / Flonum / 即値 / ヒープポインタ）は
`CLAUDE.md` の「Value Representation」を参照。

### fork した `smallvec`

Array と String のバッファは `sisshiki1969/rust-smallvec`（`const_generics`）
で、fork している理由は `OFFSET_CAPA` / `OFFSET_INLINE` / `OFFSET_HEAP_PTR` /
`OFFSET_HEAP_LEN` を公開して JIT / VM がペイロードを直接アドレッシング
できるようにするためである。規約は「inline のあいだは `capacity` が長さを
兼ね、`capacity > inline 容量` ならヒープ変種」で、機械語の高速経路はすべて
`cmp capa, N` の 1 命令でこれを分岐する。

### 世代別 GC と write barrier

GC は非移動・stop-the-world・世代別（[`../gc.md`](../gc.md)）。子への参照を
持つオブジェクトが OLD へ昇格できるのは、その参照の格納がすべて
write barrier を通る場合だけである。Array / Hash は mutator を barrier 付きの
ラッパで隠し、String は隠れ root への 1 本の参照だけを barrier し、Regexp は
`Value` を持たないので無条件に昇格できる。JIT がインラインで格納するときは
`emit_write_barrier_rdi` / `_bulk` を自分で出す。

### インライン生成器と BOP

JIT は組み込みメソッドの呼び出しを、`InlineTable::add_inline` に
`inline_gen2!` で登録された生成器（抽象状態と AsmIR を受け取り、機械語を出す
か `false` で辞退する）で置き換える。生成器は class-version ガードと
レシーバクラスガードの後で呼ばれ、辞退すれば状態と IR が巻き戻される
（trial inlining、[`../inline.md`](../inline.md)）。

`Array#[]` / `Array#[]=` / `Hash#[]` / `String#freeze` / `String#!=` など
**基本演算（BOP）**として表（`globals/store/basic_op.rs`）に載っている組は、
class-version ガード無しの直接発火経路を取れ、再定義は記録した依存
（`record_bop_dep`）で該当本体だけを evict する
（[`../bop_redefinition.md`](../bop_redefinition.md)）。表に無いメソッドは
通常の class-version ガード付き経路に残る。

### deopt と「call で逃がす」

型ガードの失敗はインタプリタへの side exit（deopt）になるが、deopt が
再コンパイルを起こさない場面で毎回 deopt するサイトは慢性的に遅くなる
（[`../chain_deopt.md`](../chain_deopt.md)、[`../deopt_log.md`](../deopt_log.md)）。
そのため各文書に出てくる高速経路は、形が合わないケースを **deopt でなく
builtin への call** で逃がす設計を繰り返し選んでいる: Hash の probe が
inline / identity / 索引の表現を `hashindex` に渡す、`Array#[]` の多相
サイトが 2 腕の dispatch を出す、`String#<<` が fallback を末尾呼び出しする、
`String#setbyte` が共有レシーバを detach して再試行する、など。

### Ruby で書かれた組み込み

`builtins/*.rb` の `Array#each` / `Hash#each` / `Array#initialize` などが
Ruby なのは、JIT がインライン展開するのが `FuncKind::ISeq` の呼び出し先だけ
だからで、ホットなサイトではメソッド本体とブロックが `yield` の位置に展開
される。代償として、CRuby の C 実装なら無傷な `Integer#<` や `Array#size` の
再定義が組み込みの内部に及ぶ（意図したトレードオフ、
[`../bop_redefinition.md`](../bop_redefinition.md)）。

### 計測の規律

各文書の数値は、同じ計測機で**交互ラウンドの中央値の最小値**を取り、
マイクロベンチは「同じループから対象操作だけ抜いたもの」を baseline として
差し引いている。monoruby と CRuby ではループ自体のコストが 10 倍近く違う
ので、生の時間を並べても比較にならない。手順は [hash.md](hash.md) §8。
