# メソッド引数の処理

## pos_num

引数スロットの数。req の他に optional, rest 引数を含み、self、子引数（分割代入で用いられる仮引数）は含まない。

ex. def f(a,(b,c),d,e=42) => pos_num = 4

## passed_args

caller から渡される実引数の数。

## Caller

- Splat 引数を展開（set_arguments(), jit_set_arguments()）
- 呼び出し先がブロックの場合(InitBlock)、引数が１個の Array であれば展開。(block_arg_expand()/single_arg_expand())
- req, opt, rest, keyword引数を処理 (vm_handle_arguments())
  - 余ったreqは nil 、余ったoptは None で埋める
  - 引数の位置個数をチェックして不正ならエラーを返す
  - keyword 引数の割り当て
- 実引数の個数 passed_args を rdx に入れて callee へ渡す

## Callee

### prologue での処理 (InitMethod / InitBlock)

- スタックの調整
- block 引数がある場合はスロットへ入れる
- 引数以外のローカル変数・一時変数スロットを nil で初期化。

### bytecode での処理 (bytecode.rs/compile_func())

- 分割代入がある場合は分割されるスロットを再帰的に展開（余った子引数は nil で埋める）

```text
        +--------------+
        |   +----------+----+
  a , ( b , c ) , d    |    |
  |     |   |     |    |    |
  v     +-+-+     v    v    v
  0       1       2    3    4

```

- opt引数がある場合は実引数が引き渡されていなければ初期化

```text
               <------pos_num------>
               <---reqopt_num---->
               <-req_num->
               +---------+-------+-+----+-+-----+-
               |   req   |  opt  |r| kw |b|decon|
               +---------+-------+-+----+-+-----+-
               +---------+-------+---+-----
ARG >= pos_num |         ARG         |
               +---------+-------+---+-----
               |         |       |  /
               +---------+-------+-+--------
               |   req   |  opt  |r|  
               +---------+-------+-+--------

               +---------+--+----+-+--------
req_num <= ARG |     ARG    | 0  | |  
               +---------+--+----+-+--------
               +---------+--+----+=+--------
ARG < req_num  | ARG |nil|   0   | |  
               +---------+--+----+-+--------
```
