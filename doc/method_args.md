# メソッド引数の処理

## pos_num

引数スロットの数。req の他に optional 引数を含み、self、子引数（分割代入で用いられる仮引数）は含まない。

ex. def f(a,(b,c),d,e=42) => pos_num = 4

## passed_args

caller から渡される実引数の数。splat operator やブロック呼び出し時の Array expand により増加する。

## Caller

- 単に引数を順番に並べるのみ
- splat operator がある場合は引数オブジェクトを SPLAT オブジェクトでラップする。
- 実引数の個数を callee へ渡す

## Callee

### prologue での処理 (interpreter: INIT_METHOD/ INIT_BLOCK)

- SPLAT オブジェクトがある場合には展開
  - passed_args を調整する
- [BLOCK] 実引数 passed_args == 1 かつ Array で、pos_num >= 2 の場合、Array を展開
  - passed_args を調整する
- [METHOD] passed_args < required, pos_num < passed_args の場合はエラーを返す
- [BLOCK] 余った必須仮引数（required - passed_args）には nil を埋める。
- 余ったオプション引数（pos_num - max(required, passed_args)）には０を埋める。
- rest 引数があれば生成
- 一時変数スロットを nil で初期化。

### bytecode での処理 (bytecode.rs/compile_func())

- 分割代入がある場合は分割されるスロットを再帰的に展開（余った子引数は nil で埋める）
- 省略可能引数がある場合は実引数が引き渡されていなければ初期化

```text
               <------pos_num------>
               <---reqopt_num---->
               <-req_num->
               +---------+-------+-+--------
               |   req   |  opt  |r|  temp
               +---------+-------+-+--------
               +---------+-------+---+-----
ARG >= pos_num |         ARG         |
               +---------+-------+---+-----
               |         |       |  /
               +---------+-------+-+--------
               |   req   |  opt  |r|  temp
               +---------+-------+-+--------

               +---------+--+----+-+--------
req_num <= ARG |     ARG    | 0  | |  temp
               +---------+--+----+-+--------
               +---------+--+----+=+--------
ARG < req_num  | ARG |nil|   0   | |  temp
               +---------+--+----+-+--------
```
