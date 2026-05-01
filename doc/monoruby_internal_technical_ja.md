# monoruby 内部技術ドキュメント（日本語）

このドキュメントは、monoruby の実行系を「フロントエンド（解析）→ バイトコード生成 → 実行（VM/JIT）→ ランタイム管理」の流れで把握するための内部向けメモです。

## 1. 全体アーキテクチャ

monoruby のコア crate（`monoruby/src/lib.rs`）は、以下の主要モジュールを中心に構成されています。

- `bytecodegen`: AST から命令列（register-based bytecode）を生成
- `executor`: バイトコード実行器（VM 本体・フレーム管理・例外状態）
- `codegen`: JIT を含むコード生成系
- `globals`: 定数/クラス/メソッド定義など、実行時グローバル状態
- `value`: Ruby の `Value`/オブジェクト表現
- `builtins`: 組み込みクラス・メソッド実装

`lib.rs` には実行時の基礎定数として `MAX_STACK_SIZE` や、`RubyMap`/`RubySet` の型エイリアスが定義されます。

## 2. コンパイルパイプライン（スクリプト/Eval）

`bytecodegen` 層のエントリポイントは次の 2 つです。

- `bytecode_compile_script(globals, ParseResult)`
- `bytecode_compile_eval(globals, ParseResult, outer, loc, binding)`

どちらも最終的に `bytecode_compile()` を経由し、対象 `FuncId` から順に `bytecode_compile_func()` を適用して各関数の ISeq を生成します。

### 2.1 エラー時の compile queue 後始末

`bytecode_compile()` は compile error 発生時に `globals.functions.clear_compile_info()` を明示的に呼び、未消費の `CompileInfo` が後続ファイルのコンパイルに混入することを防いでいます。これは複数ファイルを連続ロードする実行時の健全性を守る重要なガードです。

### 2.2 ローカル/テンポラリレジスタ設計

内部的には `BcReg`（`Self_`/`Local`/`Temp`）で抽象化され、`BcLocal` と `BcTemp` を分けて管理しています。Ruby の式評価中の一時値とローカル変数スロットを分離することで、命令選択と後段最適化の見通しが良くなります。

## 3. Executor（VM）設計

`Executor` はバイトコード実行時の中心オブジェクトで、以下を保持します。

- `cfp`: control frame pointer
- `rsp_save`: fiber/suspend 用の rsp 保存領域
- `parent_fiber`: 親 fiber 参照
- `stack_limit`: スタック上限（オーバーフロー防止）
- `lexical_class`: レキシカルクラススタック
- 正規表現特殊変数相当の状態（`$&`, `$1...` など）
- `temp_stack`: 一時オブジェクト退避
- `exception`: 例外スロット

### 3.1 extern "C" 境界と panic 封じ込め

`catch_panic_extern_c()` は Rust panic を捕捉し、プロセス abort ではなく Ruby 側の fatal error として伝搬させるラッパです。JIT/FFI 的な `extern "C"` 境界での保険として機能し、VM の例外スロットに診断情報を設定したうえでスタックトレースをダンプします。

### 3.2 スタック上限管理

`init_stack_limit()` → `set_stack_limit(rsp)` の流れで、現在 rsp を基準に `MAX_STACK_SIZE` 分だけ下げたアドレスを上限として記録します。深い再帰や想定外ネストに対する安全装置です。

### 3.3 起動時初期化

`Executor::init()` では次を実施します。

1. `$0` 設定
2. `$PROGRAM_NAME` を `$0` のエイリアス化
3. `~/.monoruby/builtins/startup.rb` を `require`
4. 必要なら `rubygems` ロード
5. 起動時の stale な `$!` クリア

これにより、Ruby 実行環境として最低限必要なグローバル状態を整えてからユーザコードを走らせます。

## 4. GC ルートと到達可能性

`Executor` は `alloc::GC<RValue>` を実装し、`temp_stack` と `cfp` 連鎖（各 `lfp`）を mark しています。VM のフレームから参照される値を GC ルートとして辿る基本形で、ローカル変数・一時値の生存判定の土台です。

## 5. 代表的な実行時データフロー

大雑把には以下の流れです。

1. パーサが `ParseResult` を作る
2. `bytecode_compile_*` が `FuncId` 単位で ISeq 化
3. `globals.gen_wrapper(func_id)` で呼び出し側エントリを準備
4. `Executor` がフレームを積んで命令実行
5. ホットパスは `codegen` 側（JIT）へ接続

## 6. 関連ドキュメント

本ドキュメントは概説です。詳細は既存資料を参照してください。

- `doc/jit_architecture.md`: JIT の全体設計
- `doc/jit.md`: JIT 実装ノート
- `doc/stack_frame.md`: フレームレイアウト
- `doc/inline.md`: インライン化戦略
- `doc/method_args.md`: 引数処理
- `doc/native_func.md`: ネイティブ関数境界

## 7. 今後の拡張観点（内部メモ）

- コンパイルキューの失敗復旧（clear 以外の部分再利用）
- frame/slot 可視化ツール（デバッグ容易化）
- panic→fatal 変換時の診断情報強化（site ごとの structured metadata）
- regex 特殊変数状態の省メモリ化・遅延化

