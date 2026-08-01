# optcarrot `--opt` プロファイル調査と高速化ロードマップ

`bin/optcarrot --opt`（`--opt-ppu=all --opt-cpu=all`）を対象に、どこで時間を
使っているかを `perf` と `--features profile` で測り、追加すべき最適化を洗い
出した記録。調査日 2026-07-31 / 2026-08-01。調査の出発点は `ba8e5599`、
最終的な A/B は `111d1895`（PR のマージベース）に対して取り直したもの。

計測機は x86-64 / Linux。数値は `--frames 3000` を 9 回走らせた中央値
（`--frames 180` の既定計測は分散が大きいので、定常状態の比較にはフレーム数を
増やしたものを使う）。

---

## 1. 出発点

| 実装 | fps (`--opt`, 180 frames) |
|------|--------------------------:|
| CRuby 4.0.1 | 約 114 |
| CRuby 4.0.1 + YJIT | 約 153 |
| monoruby (`ba8e5599`) | 約 460〜494 (3000 frames 中央値) |
| monoruby (`111d1895`) | 486.5 (3000 frames 中央値) |

## 2. 時間の内訳

`perf record -F 4999 -g --call-graph=fp` の結果を大づかみに分類すると:

| 区分 | 割合 |
|------|-----:|
| JIT が吐いたコード本体 | 約 40 % |
| **JIT コードから呼び出す Rust 側 builtin / runtime** | **約 30 %** |
| JIT コンパイル自身（`AbstractState::join` ほか） | 約 8 % |
| GC / malloc / memmove | 約 2 % |

つまり「JIT の出すコードが遅い」のではなく、**ホットループが 1 命令ごとに
Rust の関数呼び出しへ抜けている**のが最大の損失だった。individual な内訳
（改善前）:

| シンボル | 自己時間 | 呼び出し元 |
|----------|--------:|-----------|
| `Array::push` (← `ary_shl`) | 10.0 % | `@output_pixels << …` ×8 連鎖 |
| `builtins::numeric::integer::index` | 3.9 % | `data[8]`, `@_a[6]`（CPU のビット取り出し） |
| `builtins::array::rotate_` | 3.6 % | `@bg_pixels.rotate!(8)` |
| `runtime::expand_array` | 3.0 % | 多重代入 `a, b = …` |
| `Array::set_index2` + `index_assign` | 4.3 % | `@bg_pixels[@scroll_xfine, 8] = …` |
| `Value::unpack` | 2.2 % | 上記 builtin の中 |
| `Encoding::classify` (← `$1`) | 1.3 % | `--opt` のソース書き換え（起動時） |

### `--opt` の起動コスト

`--frames` を振って外挿すると、定常状態は約 2.2 ms/frame、**固定コストが約
1.3 s**。optcarrot の `--opt` は起動時に Ruby ソースを生成・正規表現で書き換え
るため、そのぶんが丸ごと乗る。`fps` の数値自体には影響しないが、
`Encoding::classify` / `match_at` はここに属する。

---

## 3. 実施した最適化

### 3.1 `Integer#[nth]` の JIT インライン化

`data[8]` のようなビット取り出しが builtin 呼び出しになっていた。レシーバは
`INTEGER_CLASS` ガード（= `Guarded::Fixnum`）で Fixnum が確定しているので、
`nth` が定数なら 3 命令で済む。

Fixnum の表現は `2n+1` なので、`n` のビット `nth` はタグ付き値のビット
`nth+1` にある。算術右シフト `nth` でちょうどビット 1 に落ち、これは Fixnum
タグが求める位置そのもの:

```asm
sarq rdi, (nth.min(63))   ; nth>=63 は符号ビット複製で正しい答えになる
andq rdi, 2
orq  rdi, 1
```

負の `nth` は常に 0、両辺リテラルなら定数畳み込み。`[nth, len]` / Range 形式は
従来どおり generic path。x86-64 / aarch64 双方に実装
（`gen_bit_index_imm`）。

**効果: 中央値 492 → 510 fps（+4 %）**、`integer::index` は 3.9 % → 0.7 %、
`Value::unpack` は 2.2 % → 0.7 % に低下。

### 3.2 `Array#<<` の真のインライン化

`Array#<<` は `define_builtin_inline_func` に登録されてはいたが、
`emit_array_shl` の中身は `movq rax, (f); call rax;` ——つまり
「generic dispatch を飛ばした直接呼び出し」でしかなく、`Array::push` 本体
（プロローグ／エピローグ + `SmallVec` の inline/heap 判定 + 書き込みバリア）は
毎回実行されていた。optcarrot の生成コードは

```ruby
@output_pixels << @output_color[pixel0] << … << @output_color[pixel7]
```

を 1 フレームあたり 61440 回実行するため、これが単独で最大のホットスポット。

`ArrayInner` は `SmallVec<[Value; 5]>` で、レイアウトは

- `capacity > 5` ⇔ spill 済み
- inline のときは `capacity` フィールドが**そのまま長さ**、容量は 5 固定
- spill 済みなら `heap_ptr` / `heap_len` が別フィールド

なので、どちらの residency でも「2 ロード + 1 ストア + 長さ加算」で追記できる。
容量いっぱいのとき（`capacity` 回に 1 回、償却的にごく稀）だけ `ary_shl` に
落として再確保させる。x86-64 / aarch64 双方に実装。

**副産物として凍結チェックのバグを修正**: `ary_shl` は `Array::push` を直接
呼ぶだけで frozen を見ていなかったため、JIT 化された `a << v` は凍結配列に
書き込めてしまっていた。

```ruby
def go(a, v) = a << v
300.times { |i| go([], i) }      # JIT を温める
go([1].freeze, 2)                # CRuby: FrozenError / 修正前 monoruby: 素通り
```

インライン化されたストアは `Array::push` を経由しないので、インライン生成側で
`ir.guard_frozen(deopt)` を張り、凍結時はインタプリタへ deopt して正しく
`FrozenError` を上げるようにした。

**効果: 中央値 510 → 590 fps。`Array::push` / `ary_shl` はプロファイルから
完全に消滅。**

### 3.3 `Array#rotate!`

`rotate_` の自己時間の **約 48 % が `i % ary_len` の 32bit `div`** だった
（`@bg_pixels.rotate!(8)` は 16 要素配列に対する回転で、剰余は常に恒等）。
除算は数十サイクルかかる一方、回転量が配列長未満という圧倒的多数のケースでは
剰余は `|cnt|` そのものなので、範囲内なら除算を飛ばす `wrap_rotate_count` を
入れた。`Array#rotate` も同じ。

ついでに `(-i) % ary_len` が `i == i64::MIN` でオーバーフローする既存の穴も
塞いだ（先に剰余を取ってから符号を反転する）。

さらに JIT インライン生成を追加し、レシーバのクラスガード + `guard_frozen` の
あと `ary_rotate_(ary, i64)` を直接呼ぶようにした（generic dispatch と
`coerce_to_int_i64` が消える）。

### 3.4 `expand_array`（多重代入）

`AsmInst::ExpandArray` は常に `runtime::expand_array` を呼んでいた。`rest` なし
で `src` がすでに Array、かつ要素数が足りているケース——多重代入の圧倒的多数
——は `#to_ary` も nil 埋めも起きない単なる代入列なので、`len <= 8` のときは
`is_array_ty` チェック + 長さ確認 + 定数回のロード／ストアに展開し、外れた場合
だけ従来の runtime 呼び出しへ落ちるようにした。

### 3.5 `Array#[]=` の slice 形式

`array_index_assign` のインライン生成は `pos_num != 2` を弾いており、3 引数形式
（`@bg_pixels[@scroll_xfine, 8] = @bg_pattern_lut[@bg_pattern]`）が丸ごと
generic dispatch に落ちていた。`set_index2` は `try_array_ty` → `copy_within`
→ `resize` → `copy_from_slice` と一般形を通るが、エミュレータの内側ループが
書くのは「同じ長さの並びを配列の内側で置き換える」形だけ——**要素の増減も
移動もない、ただのコピー**。

そこで `len` をコンパイル時リテラルから取り、残り（`other` が Array であること、
`other.len() == len`、`0 <= start` かつ `start + len <= self.len()`）を実行時に
チェックして `len` 個コピーする。外れたもの（伸縮する splice、負のインデックス、
Array でない右辺、自己代入）はすべて `set_array_slice` に落ちて builtin と同じ
意味論を再現する。

> **落とし穴**: 最初 `state.is_fixnum(start)` でゲートしたら optcarrot では
> 一度も発火しなかった。`@scroll_xfine` は ivar から読んだ値で、抽象状態は
> Fixnum と**証明できていない**（`Guarded::Value`）ためである。`load_fixnum` は
> どのみちガードを張るので、「Integer 以外だと*証明されて*いない限り許可」と
> いう弱い述語 `may_be_fixnum` を足してゲートし直した。これで発火するようになり、
> 単独で 645 → 799 fps を稼いだ。

### 3.6 合計

段階ごとの寄与（`ba8e5599` を基準に測ったもの）:

| | fps (中央値, 3000 frames) |
|---|---:|
| `ba8e5599` | 460〜494（run 間で振れる） |
| + `Integer#[]` + `Array#<<` | 590.4 |
| + `rotate!` + `expand_array` + `[]=` slice | 779.0 |

最終的な A/B は PR のマージベース `111d1895` に対して取り直した:

| | fps (中央値, 3000 frames) |
|---|---:|
| `111d1895` | 486.5 |
| + 本変更 | **775.6** |

**+59.4 %**。180 frames の既定計測では CRuby ≈ 135〜141 / YJIT ≈ 157〜158
に対し `111d1895` が 498〜528、本変更で **854〜900 fps**。
checksum は全 run で一致（60838）。

改善後プロファイルでは JIT が吐いたコードが全体の約 70 % を占め、
`Array::push` / `ary_shl` / `integer::index` / `set_index2` / `index_assign` /
`expand_array` / `coerce_to_int_i64` / `SmallVec::resize` はいずれも上位から
消えた。

検証: `cargo test --release` 3073 件 pass / 0 fail、`--features gc-stress` の
lib テスト 2253 件 pass / 0 fail。加えて各最適化について、境界・エラー・凍結・
GC 書き込みバリアを突く Ruby スクリプトを release と gc-stress の両ビルドで
CRuby と差分比較（`Array#<<` の inline/spill/成長境界/自己追記、slice 代入の
伸縮・負インデックス・非 Array 右辺・自己代入・`to_ary` 強制、`rotate!` の
巨大/負/`to_int` 引数、`expand_array` の 1〜9 要素・`*rest`・`to_ary`）。

> **注意**: aarch64 側の実装はクロスコンパイラ (`gcc-aarch64-linux-gnu`) が
> 手元になく **型チェックできていない**。monoasm の arm64 命令定義と既存の
> aarch64 コードに照らした目視レビューのみ。CI の macOS arm64 ジョブが初回の
> ビルド検証になる。

---

## 4. 残っている改善余地

改善後プロファイル（`--frames 4000`）の、JIT コード外の上位:

| 対象 | 割合 | 内容 |
|------|-----:|------|
| `Array#rotate!` の実作業 | 3.3 % | `ary_rotate_` 1.7 + `ptr_rotate` 1.6 |
| `Encoding::classify` + `match_at` | 1.8 % | `--opt` の起動時ソース書き換え |
| JIT コンパイル自身 | 約 2 % | `AbstractState::join` 1.0 ほか |

### 4.1 `Array#rotate!` の回転そのもの

除算は消えたが、`core::slice::rotate::ptr_rotate`（三段リバース／ジャグリング
の汎用実装）は残っている。`@bg_pixels` のような 16 要素固定の小配列なら、
スタック上のテンポラリにコピーして戻すだけの特殊化のほうが速い。

### 4.2 `$1` 取得時のコードレンジ再走査

`get_match_nth` → `RStringInner::propagated_cr` → `Encoding::classify` が
1.7 %。`propagated_cr` は親の `code_range()` が `Unknown` だと O(N) の再分類に
落ちる。親文字列を生成した時点（リテラル、`String#+`、`gsub` の結果など）で
コードレンジを確定させておけば、`$1` ごとの再走査がなくなる。`fps` には効かな
いが `--opt` の 1.3 s の起動時間には効く。

### 4.3 JIT コンパイル時間 / 再コンパイル

`--features profile` の `jit recompile stats` で、巨大な生成メソッド
`run` が `RecompileReason::NotCached` で **CPU 側 16 回・PPU 側 6 回**
再コンパイルされている。数千の呼び出しサイトを持つメソッドでは、
「まだ暖まっていない 1 サイト」のたびに全体を捨てて再コンパイルすることになる。

- 冷たいサイトだけを遅延コンパイルする（サイド出口スタブ方式）
- あるいは一定数のサイトが暖まるまでコンパイルを遅らせる

のどちらかで、起動 1.3 s と定常 2.5 % の双方が縮む見込み。

### 4.4 多相インラインキャッシュ (PIC)

`jit class guard failed stats` を見ると `APU::Oscillator#poke_3` などの
スーパークラス共有メソッドが `Pulse` / `Noise` / `Triangle` の 3 クラスで
呼ばれ、単相ガードのミス→ deopt になっている。JIT コード内に 2〜4 way の
PIC を持たせれば deopt を避けられる。

### 4.5 Ruby メソッドのインライン展開条件の緩和

現在 `specialized_iseq`（callee の iseq を呼び出し側に展開）が起動する条件は
`is_simple_call` かつ **レシーバか引数のどれかがコンパイル時即値**、または
`...` フォワーディングのとき。APU のように「レシーバは ivar から読んだ
オブジェクトだが、インラインキャッシュ上はクラスが単相」という典型的な
ケースは対象外になっている。

「単相 かつ callee の iseq が小さい（バイトコード N 命令未満）」を追加条件に
すれば、`Oscillator#active?` のような薄いメソッドを取り込める。機構
（`JitType::Specialized`、`specialize_level` による深さ制限）はすでにあるので、
ゲートの緩和とサイズヒューリスティクスの追加が主な作業になる。

---

## 5. 調べ方の再現手順

```sh
# プロファイル
cargo build --release --features perf
perf record -F 4999 -g --call-graph=fp -o oc.data \
  target/release/monoruby ../optcarrot/bin/optcarrot -b --opt --frames 3000 \
  ../optcarrot/examples/Lan_Master.nes
perf report -i oc.data --no-children -g none --stdio   # 自己時間の一覧
perf report -i oc.data --no-children --stdio -S <symbol>  # 呼び出し元

# deopt / 再コンパイル / メソッドキャッシュ統計
cargo build --release --features profile
target/release/monoruby ../optcarrot/bin/optcarrot -b --opt --frames 500 \
  ../optcarrot/examples/Lan_Master.nes 2> prof.txt

# A/B は必ずインターリーブして中央値を取る（単発は ±20 % 振れる）
```
