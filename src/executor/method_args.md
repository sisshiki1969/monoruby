# メソッド引数の処理

## arg_num

引数スロットの数。req の他に optional 引数を含み、self、子引数（分割代入で用いられる仮引数）は含まない。

ex. def f(a,(b,c),d,e=42) => arg_num = 4

## passed_args

caller から渡される実引数の数。splat operator やブロック呼び出し時の Array expand により増加する。

## Caller

- 単に引数を順番に並べるのみ
- splat operator がある場合は引数オブジェクトを SPLAT オブジェクトでラップする。
- 実引数の個数を callee へ渡す

## Callee

- SPLAT オブジェクトがある場合には展開
  - passed_args を調整する
- [BLOCK] 実引数が１個かつ Array で仮引数が２個以上の場合、Array を展開
  - passed_args を調整する
- 余った仮引数（arg_num - passed_args）には０を埋める（子引数は不要）
- 分割代入がある場合は分割されるスロットを再帰的に展開（余った子引数は nil で埋める）
- ０のスロットがあるかどうかチェック(required >= passed_args)
  - [BLOCK] ０のスロットを nil に書き換える
  - [METHOD] ０のスロットがあったらエラーを返す
- 省略可能引数がある場合は実引数が引き渡されていなければ初期化
