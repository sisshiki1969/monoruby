# メソッド引数の処理

## pos_num

引数スロットの数。req の他に optional, rest 引数を含み、self、子引数（分割代入で用いられる仮引数）は含まない。

ex. def f(a,(b,c),d,e=42) => pos_num = 4

## passed_args

caller から渡される実引数の数。

## Caller

- Splat 引数を展開
- 呼び出し先がブロックの場合、引数が１個の Array であれば展開。(vm_handle_arguments)
- req, opt, rest引数を処理 (vm_handle_arguments)
- 余ったreqは nil 、余ったoptは None で埋める（callee 側で引数の個数をチェック） (vm_handle_arguments)
- keyword 引数の割り当て (vm_handle_arguments)
- 実引数の個数 passed_args を callee へ渡す

### Callerの最適化

- arg_num >= reqopt_num かつ no_rest かつ no_keyword なら vm_handle_arguments を呼ばなくて済む。

## Callee

### prologue での処理 (interpreter: INIT_METHOD/ INIT_BLOCK)

- [METHOD] passed_args < required, (reqopt_num < passed_args) && no_rest の場合はエラーを返す
- block 引数の処理
- 引数以外のスロットを nil で初期化。
- 必要な情報： req_num, req_opt_num, block_pos, args_num

### bytecode での処理 (bytecode.rs/compile_func())

- 分割代入がある場合は分割されるスロットを再帰的に展開（余った子引数は nil で埋める）
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
