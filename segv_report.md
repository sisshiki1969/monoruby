# monoruby ruby/spec core カテゴリ クラッシュレポート

## 概要

ruby/spec の core カテゴリを mspec 経由で実行し、SEGV/SIGABRT（クラッシュ）が発生するテストを特定しました。

## クラッシュが確認されたカテゴリ

| カテゴリ | 終了コード | クラッシュするspecファイル |
|---------|-----------|------------------------|
| core/array | 134 (SIGABRT) | 個別ファイルでは再現せず（組み合わせ時のみ） |
| core/fiber | 134 | transfer_spec.rb |
| core/numeric | 134 | step_spec.rb |
| core/proc | 134 | call_spec.rb, case_compare_spec.rb, element_reference_spec.rb, yield_spec.rb |
| core/set | 134 | flatten_spec.rb |
| core/string | 134 | casecmp_spec.rb, index_spec.rb, modulo_spec.rb, rindex_spec.rb, split_spec.rb |

## 個別のクラッシュ詳細と最小再現コード

### 1. String#index — 空文字列検索で文字列末尾位置を指定 ✅再現可

**再現コード:** `repro/crash6_string_index.rb`
```ruby
"blablabla".index("", 9)
```

**原因:** `string.rs:1638` で `s.char_indices().nth(char_pos).unwrap()` が呼ばれるが、`char_pos` が文字列長と等しい場合、`nth()` が `None` を返しパニック。

---

### 2. String#rindex — 文字列末尾でのゼロ幅正規表現マッチ ✅再現可

**再現コード:** `repro/crash8_string_rindex.rb`
```ruby
"abc".rindex(/\z/)
```

**原因:** `string.rs:1715` で `last_char_pos.unwrap()` がパニック。`\z` は文字列の絶対末尾にマッチするため、マッチ位置（`last_byte_pos`）が文字列長と等しくなり、`char_indices()` のループ内で `last_char_pos` が設定されない。

---

### 3. String#casecmp — 無効UTF-8バイト列 ✅再現可

**再現コード:** `repro/crash5_casecmp.rb`
```ruby
"\xc3".casecmp("\xe3")
```

**原因:** `rvalue.rs:1455` で `check_utf8().unwrap()` がパニック。`casecmp` 内部で `is_str()` が呼ばれ、無効なUTF-8バイト列を `&str` に変換しようとして失敗。

---

### 4. String#split — 無効UTF-8パターンでの分割 ✅再現可

**再現コード:** `repro/crash9_string_split.rb`
```ruby
p = "\xDF".dup.force_encoding("UTF-8")
"hello:world".split(p)
```

**原因:** `split` 内の `arg0.is_str()` で区切り文字列を `&str` に変換する際、`rvalue.rs:1455` の `check_utf8().unwrap()` がパニック。無効UTF-8パターンのエラーハンドリングが不足。

---

### 5. Proc#call — detachedコンテキストからのyield ✅再現可

**再現コード:** `repro/crash3_proc_yield.rb`
```ruby
obj = Object.new
def obj.create
  Proc.new do |&b|
    yield
  end
end
a_proc = obj.create { 7 }
p a_proc.call { 3 }
```

**原因:** `codegen/runtime.rs` の `get_yield_data` で `vm.get_block().unwrap()` がパニック。Proc内の `yield` が元の作成コンテキストのブロックを参照するが、フレームが既にポップされているため `None` が返る。

---

### 6. Fiber#transfer — mspecコンテキストでのメソッド未定義エラー ⚠️mspec実行時のみ

**再現コード:** mspec経由でのみ再現
```bash
cd /home/user/spec
mspec run core/fiber/transfer_spec.rb -t monoruby
```

**原因:** `RValue::debug` で `unreachable!()` に到達。`send(:transfer)` でメソッド未定義エラーを生成する際、レシーバの `to_s` 表現作成中に `Const(nil)` 種別の `FuncInfo` に遭遇しパニック（`function.rs:963`）。

---

### 7. Numeric#step — JITコンパイラの型状態マージ失敗 ⚠️mspec実行時のみ

**再現コード:** mspec経由でのみ再現
```bash
cd /home/user/spec
mspec run core/numeric/step_spec.rb -t monoruby
```

**原因:** JITコンパイラの `gen_bridge` (`state/slot.rs:1333`) で `unreachable!()` に到達。多様な型パターン（Fixnum/Float）で `Integer#step` が繰り返し呼ばれ、JITの型状態マージで Fixnum が XMM レジスタ（Float用）に配置される矛盾が発生。

**関連バグ:** `Integer#step` を整数・浮動小数点引数を混ぜて9回以上呼ぶと、JITコンパイル後に無限ループが発生する。

---

### 8. String#% — JIT再コンパイル時のインラインキャッシュ更新失敗 ⚠️mspec実行時のみ

**再現コード:** mspec経由でのみ再現
```bash
cd /home/user/spec
mspec run core/string/modulo_spec.rb -t monoruby
```

**原因:** `iseq.rs:431` で `unwrap()` がパニック。JIT再コンパイル中の `update_inline_cache` で、関数が iseq（命令列）ではない場合に `as_iseq()` が `None` を返す。

---

### 9. Set#flatten — スタックオーバーフロー ⚠️mspec実行時のみ

**再現コード:** mspec経由でのみ再現
```bash
cd /home/user/spec
mspec run core/set/flatten_spec.rb -t monoruby
```

**原因:** 再帰的なSet構造（`set << set; set.flatten`）で無限再帰が発生し、Rustのスタックがオーバーフロー。CRubyではサイクル検出して `ArgumentError` を発生させる。

---

## バグの分類

### 即時再現可能（単体Rubyスクリプトで再現）: 5件
1. `String#index` — 境界値チェック漏れ (`unwrap` on `None`)
2. `String#rindex` — 末尾ゼロ幅マッチの未対応 (`unwrap` on `None`)
3. `String#casecmp` — 無効UTF-8のエラーハンドリング不足 (`unwrap` on `Err`)
4. `String#split` — 無効UTF-8パターンのエラーハンドリング不足 (`unwrap` on `Err`)
5. `Proc#call` with `yield` — detachedブロックの参照 (`unwrap` on `None`)

### mspec実行コンテキストでのみ再現: 4件
6. `Fiber#transfer` — `RValue::debug` の未対応型
7. `Numeric#step` — JIT型状態マージバグ
8. `String#%` — JITインラインキャッシュ更新バグ
9. `Set#flatten` — 再帰検出なし（スタックオーバーフロー）
