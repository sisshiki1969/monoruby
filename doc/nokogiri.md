# nokogiri を monoruby で動かす方法の検討

nokogiri は C 拡張（libxml2 / libxslt / gumbo）の上に Ruby を載せた gem で、
monoruby では `require "nokogiri"` が `nokogiri/nokogiri` の LoadError で止まる
（precompiled gem の `.so` は CRuby ABI なので読めない）。本稿は、psych
（`doc/…` の libyaml 移植）・zlib（`libz-sys` 同梱）と同じ「gem の Ruby 半分は
そのまま、ネイティブ半分を monoruby 側で提供する」路線を nokogiri に適用する
場合の設計と、代替案との比較をまとめる。`doc/c_extention.md`（CRuby C API 互換層）
との関係も述べる。

調査対象: nokogiri 1.19.1（x86_64-linux precompiled gem）と 1.18.9（source gem）。

## 1. nokogiri の構造と規模

| 部分 | 規模 | 備考 |
|---|---|---|
| Ruby 半分 `lib/nokogiri/**` | 77 ファイル、11,893 行 | xml 5,605 / html4 2,739 / css 1,444 / sax 650 / html5 542 / xslt 49 |
| C 拡張 `ext/nokogiri/*.c` | 36 ファイル、10,018 行 | ネイティブメソッド定義 202 個 |
| C 拡張が使う libxml2 / libxslt API | 227 関数 | |
| C 拡張が使う CRuby C API | `rb_*` 201 シンボル | `TypedData_Get_Struct` 77 箇所、`StringValueCStr` 85 箇所、`rb_funcall` 64 箇所 |
| 同梱ライブラリ | libxml2 2.13.x（nokogiri 独自パッチ 6〜10 本）、libxslt 1.1.43（1 本）、gumbo-parser（HTML5、42 ファイル 33,067 行 C）、zlib、libiconv | source gem の `dependencies.yml` と `patches/` |

ネイティブメソッドの分布（多い順）: `XML::Node` 54、`XML::Reader` 26、
`HTML4::ElementDescription` 14、`XML::NodeSet` 12、`XML::Document` 12、
`XML::SAX::ParserContext` 10、`XML::DTD` 7、`XML::SAX::PushParser` 6、
`XML::ElementContent` 6、`XML::XPathContext` 5、以下 XSLT / Schema / RelaxNG /
EncodingHandler / gumbo など。

重要なのは **CSS セレクタは Ruby 側**（racc 生成の `css/parser.rb` が CSS を
XPath に変換する）で、ネイティブに要るのは XPath 1.0 エンジンだけ、という点。
一方 `Node#to_xml` / `to_html` の整形、HTML4 パーサの補正、XPath の関数・
比較規則、`SyntaxError` のメッセージはすべて libxml2 の挙動そのもので、CRuby
と同じ出力を出すには同じ libxml2（同じパッチ）を使うのが唯一確実な道になる。

## 2. 選択肢

### A. ネイティブ半分を Rust で書き直す（libxml2 / libxslt / gumbo は C のまま同梱）

zlib と同じ形。libxml2 等は `-sys` 相当の workspace crate で C ソースから
ビルドして静的リンクし、`ext/nokogiri` の 202 メソッドを Rust の builtin
として書き直す。gem の Ruby 半分（`lib/nokogiri/**`）は psych と同様に
`gem/nokogiri/` に vendoring し、`nokogiri/nokogiri` の require をスタブで
受ける。

- 長所: CRuby と同じ libxml2 なので出力・エラー・XPath の意味論が一致する。
  XSLT / Schema / RelaxNG / Reader / SAX が全部手に入る。psych・zlib で
  確立した手順（Ruby 半分 vendoring + native stand-in + CRuby との出力比較
  テスト）をそのまま使える。
- 短所: 10k 行の C を Rust に移植する作業量。libxml2 のビルドを自前で持つ
  （autotools を使わず `cc` crate で ~60 ファイルを直接コンパイルし、
  `config.h` を Linux / macOS 向けに固定する）。ノードの寿命管理（後述）は
  nokogiri の C コードで最も込み入った部分で、そこはそのまま移植が要る。

### B. CRuby C API 互換層（`doc/c_extention.md` の Path C）で nokogiri の C 拡張をそのまま動かす

`ruby.h` 互換ヘッダと `rb_*` シムを作り、`gem install nokogiri` の再コンパイル
で nokogiri.so を monoruby 向けに生成する。

- 長所: 汎用。nokogiri に限らず sqlite3 / pg / bcrypt など C 拡張 gem 全体が
  射程に入る。
- 短所: nokogiri 一つで `rb_*` 201 シンボル、`TypedData`（mark / free / size
  関数付き）、`rb_protect` / `rb_ensure` / `rb_rescue` の例外モデル、
  `rb_enc_*`、IO コールバック（`rb_io_*` 27 箇所）まで要る。C 拡張層の
  「最小ライン」（hello world の `.so`）から nokogiri までの距離が非常に長く、
  最初のマイルストーンとしては重い。`doc/c_extention.md` §4 のレイアウト
  不一致（`RSTRING_PTR` 等）も nokogiri は踏む（`StringValueCStr` 85 箇所）。

### C. 純 Rust で再実装（html5ever / quick-xml + 自前 DOM + XPath）

crates.io の状況（2026-09）: `html5ever` 0.39（HTML5、Servo 由来で
仕様準拠）、`quick-xml` 0.42 / `xml-rs` 1.0 / `roxmltree` 0.21（XML 読み）、
`scraper` 0.27（CSS セレクタ）、XPath は `sxd-xpath` 0.4（2018 年で停止、
XPath 1.0 部分実装）、`xrust` 2.2 / `xee` 0.1（XPath 3.1 / XSLT 3.0、開発中）。
libxml2 を同梱ビルドする `-sys` crate は無い（`libxml` 0.3 はシステムの
libxml2 に pkg-config でリンクする wrapper）。

- 長所: C のビルドが無い。
- 短所: libxml2 と同じ整形・同じ HTML4 補正・同じ XPath 1.0 の細部・同じ
  エラーメッセージを別実装で再現するのは現実的でなく、XSLT / Schema /
  RelaxNG / DTD は丸ごと欠ける。CRuby との出力一致を検証の軸にしている
  monoruby の方針（psych・zlib・json）と合わない。「よくあるスクレイピング
  が動く」止まりの部分互換にしかならない。

### 比較

| | A: Rust 書き直し + C ライブラリ同梱 | B: C API 互換層 | C: 純 Rust |
|---|---|---|---|
| CRuby との出力一致 | 同じ libxml2 なので一致 | 一致 | 一致しない |
| 機能の網羅 | 全部 | 全部 | XML/HTML のみ、XPath は部分 |
| 作業量 | 中〜大（C 10k 行の移植） | 大（汎用層の構築が先） | 大（エンジン自作） |
| 他 gem への波及 | なし | 大 | なし |
| 既存の前例 | psych / zlib / json | なし | — |

**推奨は A。** B は長期の別プロジェクトとして価値があるが、nokogiri を動かす
最短路ではない。A で作る Rust 側のオブジェクト・寿命モデルは、将来 B を
やるときの `TypedData` の設計にも流用できる。

## 3. A の設計

### 3.1 crate 構成

```
monoruby/
├── libxml2-src/        # workspace crate: libxml2 + libxslt (+libexslt) を cc でビルド
│   ├── build.rs        # ~60 + ~40 ファイルを直接コンパイル、config.h は同梱の固定版
│   ├── vendor/         # nokogiri の dependencies.yml と同じ版に patches/ を適用したもの
│   └── src/lib.rs      # 使う 227 関数の extern "C" 宣言（手書き。bindgen は使わない）
├── gumbo-src/          # nokogiri 同梱の gumbo-parser（42 ファイル）を cc でビルド
├── monoruby/src/builtins/nokogiri/   # ext/nokogiri の移植
│   ├── document.rs, node.rs, node_set.rs, xpath_context.rs, sax.rs, reader.rs,
│   │   xslt.rs, schema.rs, html4.rs, html5.rs, encoding.rs, syntax_error.rs
│   └── mod.rs          # クラス登録
└── monoruby/gem/nokogiri/            # gem の lib/nokogiri/** を vendoring
    └── nokogiri/nokogiri.rb          # C 拡張の代わりに読まれるスタブ（定数定義など）
```

libz-sys の前例どおり、C ソースは monoruby のビルドで静的リンクする。
iconv は libxml2 の encoding 変換に必要で、glibc と macOS は libc に持つので
libiconv は同梱しない（nokogiri の precompiled ビルドだけが同梱している）。
zlib は既存の libz-sys を共有する。

libxml2 は nokogiri のパッチを当てた版を使う。とくに `0009-allow-wildcard-
namespaces`（`*:foo` の XPath）と `0019-xpath-…static-hash-table` は
nokogiri の挙動そのものなので外せない。

### 3.2 オブジェクトと寿命

nokogiri の C 側で最も難しい部分。libxml2 のノードはドキュメントに属し、
`xmlFreeDoc` で木ごと解放される。Ruby 側のオブジェクトとの対応は:

- `Document` が `xmlDocPtr` を所有し、ドキュメントの解放と同時に木を解放する。
- 各 `xmlNode` は `_private` に対応する Ruby オブジェクトを 1 つだけ持つ
  （同じノードを 2 回取り出しても同じ `Node` が返る）。ドキュメントは
  `node_cache`（Ruby Array）で生成済み `Node` を強参照する。
- `unlink` されたノードは木から外れるが `Document` の `unlinkedNodes` 集合に
  入り、ドキュメントと一緒に解放される（別ドキュメントへの `add_child` は
  `xmlDOMWrapAdoptNode` / reparent で所有権を移す）。
- `NodeSet` は `xmlNodeSetPtr` を持ち、含むノードの `Document` を参照して
  生かす。XPath の結果（`XPathContext#evaluate`）はここから作る。

monoruby では、zlib の「整数ハンドル + スレッドローカル表 + `ObjectSpace`
finalizer」方式ではなく、**`RValue` に新しい種別（`ObjTy::XML_DOC` /
`XML_NODE` / `XML_NODE_SET` など）を足して生ポインタを直接持たせる**のが
適切。理由は (1) ノードは大量に生成され、表経由の間接参照は遅い、(2) GC の
mark で `Node` → `Document` の参照を辿る必要がある、(3) `Document` の解放時に
`xmlFreeDoc` を呼ぶ drop が要る、の 3 点。`RValue` の mark / drop に種別ごとの
処理を足す形で、`Fiber` や `Binding` と同じ扱いになる。

ノード → Ruby オブジェクトの逆引きは nokogiri と同じく `_private` に
`Value` を入れる（GC 移動が無い mark-and-sweep なのでそのまま持てる）。
ドキュメントの mark でキャッシュ済みノードを全部 mark する。

### 3.3 Ruby との境界

- **パース**: `read_memory` は文字列から、`read_io` は `xmlReadIO` に Ruby の
  `IO#read` を呼ぶコールバックを渡す。コールバック中の Ruby 例外は
  `Executor` のエラー状態に積んでパースを中断し、戻ってから投げ直す
  （nokogiri は `rb_protect` でやっている部分）。
- **エラー**: libxml2 の structured error handler で `xmlError` を受け、
  `Nokogiri::XML::SyntaxError` を作って `document.errors` に積む。
  `RECOVER` 無しなら最初のエラーで raise。メッセージは libxml2 のものを
  そのまま使う（CRuby と一致させる要）。
- **シリアライズ**: `native_write_to` は `xmlSaveToIO` に Ruby の
  `IO#write` コールバック。`to_xml` / `to_html` / `to_s` の整形はこれで
  libxml2 任せ。HTML5 は Ruby 側の `html_standard_serialize` が gumbo の
  シリアライザを呼ぶ。
- **XPath**: `XPathContext#evaluate` で文字列を評価し、結果の型
  （nodeset / bool / number / string）を変換。`register_ns`、
  `register_variable`、Ruby ハンドラによるカスタム関数（`xmlXPathRegisterFunc`
  に Rust の trampoline を登録し、そこから Ruby のメソッドを呼ぶ）。
- **SAX / PushParser / Reader**: libxml2 の SAX ハンドラを Rust で受けて
  `Nokogiri::XML::SAX::Document` の各メソッドを呼ぶ。Reader は
  `xmlTextReader` の薄い wrapper。
- **XSLT**: `Stylesheet.parse_stylesheet_doc` / `transform` / `serialize`。
  EXSLT 関数の登録込み。カスタム XSLT 関数（Ruby ハンドラ）は後回しで良い。
- **Schema / RelaxNG / DTD**: `xmlSchemaParse` / `xmlRelaxNGParse` と
  `validate_document`、DTD の `ElementDecl` / `AttributeDecl` /
  `EntityDecl` / `ElementContent` は `XML::Node` のサブクラスとして生ポインタを
  読むだけ。

### 3.4 スタブと Ruby 半分

`gem/psych/psych.rb` と同じく `gem/nokogiri/nokogiri/nokogiri.rb` を置き、
C 拡張が定義していた定数（`Nokogiri::LIBXML_COMPILED_VERSION`、
`LIBXML_PARSER_VERSION`、`VERSION_INFO` の libxml 部分、`XML::Node::*_NODE`
の型定数、`XML::ParseOptions` の値）と、ネイティブメソッドを持つクラスの
骨格をここで定義する。Ruby 半分は gem のものを無改造で使う。`extension.rb`
は `nokogiri/#{RUBY_VERSION}/nokogiri` を試してから `nokogiri/nokogiri` に
落ちるので、後者をスタブで受ければ足りる。

## 4. 進め方と検証

ruby-bench に nokogiri のベンチマークは無いので、動機は「動かす」こと自体と
Rails 系（rails-dom-testing、loofah、rails-html-sanitizer は nokogiri 必須）
の解放。段階:

1. **XML の基本**: `libxml2-src` のビルド、`Document.parse` / `Node` の
   走査・属性・`content` / `xpath` / `to_xml` / `SyntaxError`。ここで
   3.2 の寿命モデルを確定させる。
2. **HTML4 と CSS**: `HTML4::Document.parse`（libxml2 の HTML パーサ）、
   CSS → XPath は Ruby 側なので `css` / `at_css` はほぼ自動で通る。
   `ElementDescription`。
3. **編集と再シリアライズ**: `add_child` / `replace` / `unlink` /
   `add_namespace_definition`、ドキュメント間の移動、`Builder`（Ruby 側）。
4. **HTML5**: gumbo の同梱と `HTML5::Document.parse` / `html_standard_serialize`。
5. **SAX / PushParser / Reader**。
6. **XSLT / Schema / RelaxNG / DTD**。

検証は二本立て:

- nokogiri 本体のテストスイート（GitHub の `test/`、minitest）。gem には
  入っていないので checkout が要る。まず `test/xml/test_document.rb`、
  `test_node.rb`、`test_node_set.rb`、`test_xpath.rb`、`html4/test_document.rb`
  から。
- `tests/ruby_bench_outputs.rs` と同じ形の CRuby 出力比較テスト（同じ
  HTML / XML を両方でパースして `to_xml` / `xpath` の結果を突き合わせる）。
  同じ libxml2 なので byte 単位で一致するはず。

## 5. 見積りの目安

既存の native stand-in は yaml.rs 496 行（libyaml-safer の上）、zlib.rs 635 行、
prism.rs 172 行で、いずれも下に完成したライブラリがある。nokogiri は
`ext/nokogiri` 10k 行の移植が本体で、`RValue` の新種別と GC 連携、
libxml2 のビルドが付く。段階 1〜3 で ext の 6 割程度（Node 54 + Document 12
+ NodeSet 12 + XPathContext 5 + HTML4 + SyntaxError）を占め、そこまでで
一般的な用途（スクレイピング、Rails のビュー検証、loofah のサニタイズ）は動く。

B（C API 互換層）を将来やるなら、ここで作る `ObjTy::XML_*` の mark / drop の
形がそのまま `TypedData` の受け皿になるので、A の作業は無駄にならない。

## 6. 実装状況（段階 1〜2 の一部、2026-09）

`libxml2-src/`（libxml2 2.13.8 + nokogiri 1.18.9 の patches、`cc` でビルド、
`config.h` は手書き、`xmlversion.h` は build.rs が生成）、
`monoruby/src/builtins/nokogiri/`（Rust 側 ~2.5k 行）、`monoruby/gem/nokogiri/`
（gem 1.19.1 の Ruby 半分をそのまま + `nokogiri/nokogiri.rb` のスタブ）。
`tests/nokogiri.rs` が CRuby の nokogiri gem と同じスクリプトを走らせて出力を
突き合わせる（gem が無ければ skip、CI は入れる）。

動くもの:

- `XML::Document`: `parse` / `read_memory` / `read_io`（Ruby の IO を
  コールバックで読む）/ `new` / `root` / `root=` / `encoding` / `version` /
  `url`、`errors`、strict モードの `SyntaxError`（message / line / column /
  domain / code / level / file / str1..3 / int1 が CRuby と一致）。
- `XML::Node` の 54 のネイティブのうち `dup` 系（`initialize_copy_with_args`）、
  `canonicalize`、`process_xincludes`、
  `html_standard_serialize`（HTML5）、`prepend_newline?`、`create_external_subset`
  以外: 走査、属性（`get` / `set` / `key?` / `attribute_nodes`）、名前空間、
  `content` / `native_content=`、`path` / `line`、`unlink`、`add_child` /
  `add_next_sibling` / `add_previous_sibling` / `replace`（`reparent_node_with`
  と `relink_namespace` の移植: 別ドキュメントやテキストノードの複製、
  隣接テキストの併合、名前空間の再結合）、`in_context`（`DocumentFragment`、
  `Node#parse`、`inner_html=`、`add_child(String)`）、`native_write_to`
  （`to_xml` / `to_html` / `to_s`、インデント、エンコーディング指定）、
  `dump_html`。`Text` / `Comment` / `CDATA` / `ProcessingInstruction` / `Attr`
  / `DocumentFragment` のコンストラクタ、`Attr#value=`、`Namespace`。
- `XML::NodeSet`（`&` `|` `-` `[]` `slice` `delete` `include?` `length` `push`
  `to_a` `unlink` `initialize_copy`）、`XML::XPathContext`（`evaluate` /
  `register_ns` / `register_variable` / `node=`、`css-class` と
  `local-name-is` の組み込み関数なので CSS セレクタが全部通る）。
- `HTML4::Document`（`read_memory` / `read_io` / `new` / `type`）、
  `HTML4::EntityLookup`、`EncodingHandler`。
- DTD 一式（`dtd.rs`）: `XML::DTD`（`entities` / `elements` / `attributes` /
  `notations` の各ハッシュ、`validate`、`external_id` / `system_id`、
  `create_internal_subset` / `internal_subset` / `external_subset`）、
  `EntityDecl`（`content` / `original_content` / `entity_type` / 各 ID と型定数）、
  `ElementDecl`（`element_type` / `content` / `prefix`）、`AttributeDecl`
  （`attribute_type` / `default` / `enumeration`）、`ElementContent`（内容モデルの
  木、`NATIVE` クラスで `@document` を持つ）、`Document#create_entity`。

まだ無いもの（段階 2〜6）: `XML::SAX::*`（`SAX::PushParser` は
`HTML4::EncodingReader` が使うので **HTML の IO からのパース**もまだ）、
`XML::Reader`、`XML::Schema` / `RelaxNG`、`XSLT`（定数は `0.0.0` の
プレースホルダ）、HTML5（gumbo）、`HTML4::ElementDescription`
（`Node#description`）、XPath のカスタム関数ハンドラ（`evaluate` の第 2
引数は受け取るが無視）、`Node#dup` / `Document#dup`。

設計上わかったこと:

- ネイティブオブジェクトの種別 `ObjTy::NATIVE`（`RValue` に
  `Box<dyn NativeData>` を持たせ、`mark` と `Drop` を型ごとに実装）を足した。
  そのクラスは **`instance_ty` を `NATIVE` で定義しなければならない**
  （`Store::define_class_with_instance_ty`）: JIT は `instance_ty` が `OBJECT`
  のクラスの ivar をインラインスロット（`kind` 共用体）に読み書きするので、
  普通の `define_class` で作ると `@errors = ...` がペイロードの Box を上書きして
  落ちる。
- パースのエラー収集は **libxml2 のグローバル（スレッドローカル）structured
  handler**（`xmlSetStructuredErrorFunc`）で行い、コンテキストの
  `xmlCtxtSetErrorHandler` は使わない。`NOERROR` / `NOWARNING` オプション
  （`DEFAULT_HTML` に入っている）が付くと libxml2 はコンテキストのハンドラを
  素通りするが、グローバルのハンドラには依然として届く。nokogiri が
  `document.errors` を埋め、strict モードで raise できるのはこのため。
- ビルトインの中で作った `Value` を Ruby 呼び出し（`initialize`、`decorate`、
  `SyntaxError.new`）を跨いで持つときは `vm.temp_push` で根付けする
  （`doc/gc.md` §8.1）。`wrap_document` / `wrap_node_set` / `errors_to_array`
  / `NodeSet#to_a` がそれ。
