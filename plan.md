# 修正プラン: instance_eval spec failures #2, #4, #5, #6

## 対象テスト

| # | テスト | 現状 | 期待 |
|---|--------|------|------|
| #2 | default location | `(eval at spec.rb:87)` | `(eval at spec.rb:89)` |
| #4 | filename/lineno in backtrace | `backtrace.first` = `<internal>:in 'Kernel#require'` | `a_file:10:...` |
| #5 | negative lineno in backtrace | `backtrace.first` = `<internal>:in 'Kernel#raise'` | `b_file:-98:...` |
| #6 | filename with to_str | `backtrace.first` = `<internal>:...` | `file.rb:...` |

## 根本原因の分析

### #2: `get_caller_loc` が関数定義位置を返す

`store.rs:310-322` の `get_caller_loc` が `iseq.loc`（関数/ブロック定義位置）を使っている。
呼び出し位置のPCベースのsourcemap lookupを使うべき。

### #4, #5, #6: バックトレースの先頭にゴミエントリ

monorubyのバックトレース:
```
0: <internal>:in 'Kernel#require'      ← 起動時のrequire
1: /usr/.../rubygems.rb:1434:in '/main' ← 起動時のrequire
2: <internal>:in 'Kernel#raise'         ← raise自体の内部トレース
3: a_file:10:in 'block in /main'        ← 本来の先頭 ★
4: <internal>:in 'BasicObject#instance_eval'
5: -e:3:in '/main'
```

CRubyのバックトレース:
```
0: a_file:10:in '<main>'               ← 先頭
1: -e:3:in 'BasicObject#instance_eval'
2: -e:3:in '<main>'
```

問題点:
1. 起動時の `Kernel#require` / `rubygems.rb` のトレースが残っている
2. `Kernel#raise` の内部フレームがトレースに含まれる（CRubyでは含まない）

---

## 修正ステップ

### Step 1: `get_caller_loc` にPCベースの位置解決を追加

**ファイル**: `monoruby/src/globals/store.rs`

`get_caller_loc` のシグネチャを変更し、pcパラメータを受け取れるようにする:

```rust
pub fn get_caller_loc(&self, cfp: Cfp, pc: Option<BytecodePtr>) -> String {
    let func_id = cfp.lfp().func_id();
    if let Some(iseq_id) = self[func_id].is_iseq() {
        let iseq = &self[iseq_id];
        let loc = if let Some(pc) = pc {
            let bc_index = iseq.get_pc_index(Some(pc));
            iseq.sourcemap[bc_index.to_usize()]
        } else {
            iseq.loc
        };
        format!(
            "{}:{}",
            iseq.sourceinfo.file_name(),
            iseq.sourceinfo.get_line(&loc)
        )
    } else {
        "<internal>:0".to_string()
    }
}
```

### Step 2: 呼び出し側を更新（pcを渡す）

3箇所の呼び出しを更新:

**ファイル**: `monoruby/src/builtins/object.rs` (instance_eval)
- `instance_eval` の `_: BytecodePtr` を `pc: BytecodePtr` に変更
- `instance_eval_inner` に `pc` を引数として追加
- `get_caller_loc(caller_cfp)` → `get_caller_loc(caller_cfp, Some(pc))`

**ファイル**: `monoruby/src/builtins/kernel.rs` (eval)
- `_: BytecodePtr` を `pc: BytecodePtr` に変更
- `get_caller_loc(caller_cfp)` → `get_caller_loc(caller_cfp, Some(pc))`

**ファイル**: `monoruby/src/builtins/module.rs` (module_eval)
- `_: BytecodePtr` を `pc: BytecodePtr` に変更
- `get_caller_loc(caller_cfp)` → `get_caller_loc(caller_cfp, Some(pc))`

### Step 3: バックトレースから起動時のrequireトレースを除外

**ファイル**: `monoruby/src/value/rvalue/exception.rs`

`trace_location` メソッドで、起動時のrequire関連エントリをフィルタする。
または、`raise` で新たに作られるエラーのトレースが起動時のフレームを含まないようにする。

具体的には `handle_error` (`codegen/jit_module.rs:377-389`) で builtin 関数の内部トレースを push する際、
`Kernel#raise` の場合はトレースに追加しないようにする:

```rust
FuncKind::Builtin { .. } => {
    let lfp = vm.cfp().lfp();
    if let MonorubyErrKind::MethodReturn(val, target_lfp) = vm.exception().unwrap().kind() {
        return if lfp == *target_lfp {
            let val = *val;
            vm.take_error();
            ErrorReturn::return_normal(val)
        } else {
            ErrorReturn::return_err()
        };
    }
    // Kernel#raise の内部フレームはトレースに追加しない
    let fid = meta.func_id();
    let name = globals.store.func_description(fid);
    if name != "Kernel#raise" {
        vm.push_internal_error_location(fid);
    }
}
```

ただし、起動時の `Kernel#require` / `rubygems.rb` の問題は別のアプローチが必要。
スタック巻き戻しが起動時フレームまで到達してしまっている可能性がある。

### Step 4: 起動時フレームのトレース混入を調査・修正

**仮説**: eval内の `raise` → `instance_eval` のブロック実行フレーム → ... と巻き戻る際、
起動スクリプトのフレーム（`require "rubygems"` etc.）まで巻き戻りが到達し、
rescue で捕捉される前にそれらのフレームがトレースに追加されている。

**調査ポイント**: `handle_error` のISeqパス（332-376行）で、rescue handler が見つかった場合は
`ErrorReturn::goto(bc_base + rescue)` で rescue に飛ぶが、その前に `push_error_location` を
呼んでいる（367行）。つまり rescue があるフレームでもトレースに追加される。

起動時のrequireフレームにrescueハンドラがあり、そこでトレースにpushされた後
rescue handler で例外を再raiseしている可能性。

→ この問題は起動スクリプトの構造に依存するため、Step 3 の `Kernel#raise` フィルタリングと合わせて
影響を確認し、必要に応じて追加対応する。

---

## 修正の優先順位

1. **Step 1 + Step 2**: `get_caller_loc` のPC対応 → #2 を修正
2. **Step 3**: `Kernel#raise` のトレース除外 → #4, #5, #6 の部分修正
3. **Step 4**: 起動時require トレース混入の調査・修正 → #4, #5, #6 の完全修正

## テスト方法

各ステップ後に以下を確認:
- `cargo test` が通ること
- `bin/spec` (mspec core/basicobject) で対象テストが pass すること
- `monoruby -e` でバックトレースの内容を手動確認
