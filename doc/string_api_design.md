# RStringInner API 整理案

## 背景

現在 `RStringInner` を扱う多くのコードで、

```rust
let bytes = inner.as_bytes();           // 1) バイト列を取り出す
out.extend_from_slice(bytes);           // 2) Vec<u8> に詰める
RStringInner::from_encoding(&out, enc)  // 3) cr=Unknown で再構築
```

というパターンが頻出している。`RStringInner` がキャッシュしている
`CodeRange`(`SevenBit` / `Valid` / `Broken` / `Unknown`)が毎回失われる
ため、後段で `code_range()` が呼ばれるたびにバッファ全体を再スキャンする
ことになり O(N²) に縮退する。

既に `extend(&self, &Self, &Store)` には合成ロジックが入っている
([string.rs:1144-1175](../monoruby/src/value/rvalue/string.rs#L1144-L1175))
ので、置換系・連結系の API を `&[u8]` 経由から `&RStringInner` 経由に
寄せれば call site も短くなり、cr も保たれる。

## 影響を受ける主な call site

| 場所 | 現状 | 失っている情報 |
|------|------|----------------|
| `replace_byte_range` ([string.rs:1094-1103](../monoruby/src/builtins/string.rs#L1094-L1103)) | `Vec<u8>` で splice → `from_string` | エンコーディング(UTF-8 強制) + cr |
| `unicode_normalize_` ([string.rs:5791-5794](../monoruby/src/builtins/string.rs#L5791-L5794)) | `bytesplice` に `String::as_bytes` を渡す | replacement の cr (NFC/NFD 後の文字列は Valid 確定) |
| `scrub_inner` ([string.rs:5431-5452](../monoruby/src/builtins/string.rs#L5431-L5452)) | `Vec<u8>` 経由で `from_encoding` | repl の cr |
| `scrub_inner_with_block` ([string.rs:5460-5521](../monoruby/src/builtins/string.rs#L5460-L5521)) | 同上 | block 戻り値の cr |
| `gsub`/`sub` 系 (regexp.rs:359-460) | `expect_str` で `&str` 経由 → `String::replace_range` → `Value::string` | エンコーディング全体 (非 UTF-8 互換は壊れる) + cr |

## 提案する API

### Phase 1: `RStringInner` に閉じる API

#### 1. `bytesplice_with`

```rust
impl RStringInner {
    /// `start..start+len` のバイト範囲を `replacement` で置き換える。
    ///
    /// - 互換エンコーディングを `compatible_encoding` で算出し、
    ///   不一致なら `Encoding::CompatibilityError` を返す。
    /// - `replacement.code_range()` のキャッシュを利用して、結果の
    ///   cr を O(1) で合成する(SevenBit + SevenBit → SevenBit、
    ///   どちらかが Valid → Valid、Broken なら Unknown 落ち)。
    /// - UTF-8 + 文字境界一致 + replacement.cr が SevenBit/Valid の
    ///   場合は `from_utf8(replacement)` の再検証を省略できる。
    ///   既存 `bytesplice` のスロー再 classify パスは保持。
    pub fn bytesplice_with(
        &mut self,
        start: usize,
        len: usize,
        replacement: &RStringInner,
        store: &Store,
    ) -> Result<()>;
}
```

実装の骨子:

```rust
let result_enc = self
    .compatible_encoding(replacement)
    .ok_or_else(|| MonorubyErr::incompatible_encoding(store, self.ty, replacement.ty))?;

let prev_cr = self.cr.get();
let prev_ty = self.ty;
let repl_cr = replacement.cr.get();
let end = start + len;

// バッファ操作 (現 bytesplice と同じ shift / copy_within / truncate)
self.splice_bytes(start, end, replacement.as_bytes());
self.ty = result_enc;

// cr の合成: replacement.cr がキャッシュ済みなら walk 不要
let utf8_boundaries_ok = matches!(prev_ty, Encoding::Utf8)
    && matches!(prev_cr, CodeRange::Valid | CodeRange::SevenBit)
    && is_utf8_char_boundary(&self.content_before, start)
    && is_utf8_char_boundary(&self.content_before, end);

let new_cr = match (prev_cr, repl_cr) {
    (CodeRange::SevenBit, CodeRange::SevenBit)
        if prev_ty.is_ascii_compatible() => CodeRange::SevenBit,
    (CodeRange::SevenBit | CodeRange::Valid,
     CodeRange::SevenBit | CodeRange::Valid)
        if utf8_boundaries_ok => CodeRange::Valid,
    _ => self.ty.classify(&self.content), // 現行スローパスと同等
};
// Broken + UTF-8 互換は ASCII-8BIT に降格(CRuby 互換)
if matches!(new_cr, CodeRange::Broken) && self.ty.is_utf8_compatible() {
    self.ty = Encoding::Ascii8;
    self.cr.set(CodeRange::Valid);
} else {
    self.cr.set(new_cr);
}
Ok(())
```

既存 `bytesplice(&mut self, start, len, &[u8])` は内部から
`bytesplice_with` を呼ぶ薄いラッパとして残し、callers は段階的に
新 API へ移行する。

#### 2. `with_encoding_capacity`

```rust
impl RStringInner {
    /// 指定エンコーディングの空 RStringInner を、`cap` バイト分の
    /// 容量で確保する。空文字列の cr は SevenBit。
    pub fn with_encoding_capacity(enc: Encoding, cap: usize) -> Self;
}
```

これにより `scrub_inner_*` の `Vec<u8>` を `RStringInner` で代替できる:

```rust
// before
let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
out.extend_from_slice(repl.as_bytes());
RStringInner::from_encoding(&out, enc)

// after
let mut out = RStringInner::with_encoding_capacity(enc, bytes.len());
out.extend(repl, store)?;   // cr 合成済み
out
```

### Phase 2: gsub / sub の RStringInner 化

Phase 1 とは独立。regexp 側の onigmo 呼び出しは内部的に `&[u8]` ベース
なので、現在 `&str` 経由になっている入口を `&RStringInner` ベースに
切り替えれば、

- 非 UTF-8 互換エンコーディング(Sjis / EucJp / UTF-16 / UTF-32)が
  gsub で壊れる現バグも同時に直る
- replacement と self の cr が Phase 1 の `bytesplice_with` 経由で
  保たれる

ための具体案:

```rust
impl RegexpInner {
    pub(crate) fn replace_all_inner(
        vm: &mut Executor,
        globals: &mut Globals,
        regexp: Value,
        given: &RStringInner,           // ← &str から変更
        replace: &RStringInner,         // ← &str から変更
    ) -> Result<(RStringInner, bool)>;
}
```

返り値は `(String, bool)` ではなく `(RStringInner, bool)` にすることで、
caller (`gsub` / `sub`) は `Value::string_from_inner` で包むだけになる。

ただしこれは regexp の内部表現変更が伴うため、Phase 1 を完了してから
別タスクとして着手する。

## 移行手順 (Phase 1)

1. `bytesplice_with` と `with_encoding_capacity` を追加 + ユニットテスト
   (cr 伝播 / 互換エンコーディング / UTF-8 境界 / Broken 降格を網羅)。
2. `replace_byte_range` を `bytesplice_with` で書き換え。`subst: &str` は
   `RStringInner::from_str_scanned(subst)` で wrap して渡す。
3. `unicode_normalize_` を `bytesplice_with` で書き換え。NFC/NFD 出力は
   常に Valid なので `from_string_scanned` を使う。
4. `scrub_inner` / `scrub_inner_with_block` を builder パターンに書き換え。
5. `start_with` / `end_with` / `casecmp` 等で `inner.as_bytes().to_vec()`
   している箇所は、純粋な比較なので Phase 1 の対象外(cr 関係なし)。

## 既存テストの確認

- `bin/test` の string サブセット (`tests/string*.rs`) は CRuby 出力差分
  比較なので、cr 伝播の改善はそのまま透過。
- `ruby/spec` の `core/string` (特に `bytesplice_spec.rb`,
  `unicode_normalize_spec.rb`, `scrub_spec.rb`) を Phase 1 完了時に
  全実行して回帰がないか確認。
