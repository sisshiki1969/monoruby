# ActiveRecord on monoruby - 実装計画

## 概要

ActiveRecord を monoruby 上で動かすための段階的な実装計画。
ActiveRecord は Ruby on Rails の ORM コンポーネントで、以下の依存関係を持つ:

```
activerecord
├── activesupport (コアユーティリティ)
├── activemodel (バリデーション、コールバック等)
└── arel (SQLクエリビルダー、Rails 7+ では内蔵)
```

各フェーズは独立してテスト可能な単位で設計し、段階的に進める。

---

## 現状分析

### monoruby が既に持っている強み

- **メタプログラミング**: `method_missing`, `define_method`, `class_eval`, `instance_eval` 等 — ActiveRecord の動的メソッド生成に必須
- **モジュール合成**: `include`, `prepend`, `extend`, `Module#included` フック
- **コアクラス**: String (85メソッド), Array (65メソッド), Hash (45メソッド) が充実
- **FFI/Fiddle**: C ライブラリ呼び出し可能 — データベースアダプタに利用可能
- **require/autoload**: モジュールロードシステム
- **Proc/Lambda/Block**: クロージャサポート
- **正規表現**: onigmo ベースの Regexp
- **Fiber**: コルーチンサポート
- **Struct**: 動的構造体

### 不足している機能 (重要度順)

| 機能 | 重要度 | 用途 |
|------|--------|------|
| `Date` / `DateTime` | 最重要 | 日付カラム、マイグレーション |
| `Set` | 最重要 | ActiveSupport 内部で多用 |
| `Encoding` 完全対応 | 重要 | 文字列エンコーディング変換 |
| `Thread` / `Mutex` 実装 | 重要 | コネクションプール |
| `ObjectSpace` | 中 | ファイナライザ等 |
| `Logger` | 中 | SQLログ出力 |
| `URI` | 中 | データベースURL解析 |
| `StringIO` | 中 | バッファリング |
| `ERB` | 低 | テンプレート (ActiveRecord 単体では不要) |
| `YAML (Psych)` | 低 | fixtures、設定ファイル |

---

## フェーズ 1: Ruby 言語機能の補完

**目標**: ActiveSupport が最低限動くために必要な Ruby 言語機能を実装する。

### 1.1 欠損しているコアメソッドの追加

ActiveSupport は Ruby コアクラスに多数のメソッドを追加 (monkey-patch) するため、
まず元のコアクラスメソッドの網羅性を上げる必要がある。

- [ ] `Object#respond_to_missing?`
- [ ] `Object#frozen?` / `Object#freeze` の完全対応
- [ ] `Object#tap`, `Object#then` / `Object#yield_self`
- [ ] `Module#const_missing`
- [ ] `Module#method_defined?`, `Module#instance_method`
- [ ] `Module#ancestors` の完全な MRO (Method Resolution Order) 対応
- [ ] `Module#name` (無名モジュールで nil を返す)
- [ ] `Kernel#caller`, `Kernel#caller_locations` (バックトレース)
- [ ] `Kernel#__dir__`
- [ ] `Kernel#at_exit`

**検証方法**: 各メソッドの CRuby との互換性テスト

### 1.2 Hash の拡張

- [ ] `Hash#transform_keys`, `Hash#transform_keys!`
- [ ] `Hash#transform_values`, `Hash#transform_values!`
- [ ] `Hash#slice`, `Hash#except`
- [ ] `Hash#deep_merge` (ActiveSupport が追加するが、基盤として必要)
- [ ] `Hash#dig`
- [ ] `Hash#each_with_object`

### 1.3 String の拡張

- [ ] `String#encode`, `String#encoding` の完全対応
- [ ] `String#freeze` (frozen string literal 対応)
- [ ] `String#unicode_normalize`
- [ ] `String#unpack` / `String#pack` の拡張
- [ ] `String#squeeze`, `String#delete`

### 1.4 Comparable / Enumerable の拡張

- [ ] `Enumerable#each_with_object`
- [ ] `Enumerable#chunk`, `Enumerable#chunk_while`
- [ ] `Enumerable#flat_map` (既存なら確認)
- [ ] `Enumerable#tally`
- [ ] `Enumerable#filter_map`
- [ ] `Comparable#clamp`

### 1.5 例外クラスの追加

- [ ] `NotImplementedError`
- [ ] `EncodingError`
- [ ] `IOError`, `EOFError`
- [ ] `Errno::*` クラス群 (ENOENT, EACCES, EEXIST 等)
- [ ] `SignalException`, `Interrupt`

**推定作業量**: 中〜大
**依存関係**: なし (他のフェーズと並行可能)

---

## フェーズ 2: 標準ライブラリの実装

**目標**: ActiveSupport / ActiveRecord が require する標準ライブラリを実装する。

### 2.1 Date / DateTime / Time の完全実装

ActiveRecord の日付カラム処理に必須。monoruby は `Time` のみ部分実装済み。

- [ ] `Date` クラス (date.rb 相当)
  - `Date.today`, `Date.new`, `Date.parse`, `Date.strptime`
  - `Date#year`, `#month`, `#day`, `#wday`, `#yday`
  - `Date#+`, `Date#-`, `Date#>>`, `Date#<<` (月送り)
  - `Date#strftime`, `Date#to_s`, `Date#iso8601`
  - `Date#to_time`, `Date#to_datetime`
- [ ] `DateTime` クラス (Date のサブクラス)
- [ ] `Time` の拡張
  - `Time#strftime` 完全対応
  - `Time#to_date`, `Time#to_datetime`
  - `Time.parse`, `Time.strptime` (require 'time')
  - タイムゾーン対応基盤

**実装方針**: Rust の `chrono` クレートは既に依存関係にあるため活用する。

### 2.2 Set クラス

- [ ] `Set.new`, `Set#add`, `Set#delete`, `Set#include?`
- [ ] `Set#each`, `Set#map`, `Set#select`
- [ ] `Set#&`, `Set#|`, `Set#-`, `Set#^`
- [ ] `Set#merge`, `Set#subset?`, `Set#superset?`
- [ ] `SortedSet` (オプション)

**実装方針**: Ruby で実装可能 (Hash をバックエンドとして使用)。

### 2.3 StringIO

- [ ] `StringIO.new`, `StringIO#read`, `StringIO#write`
- [ ] `StringIO#puts`, `StringIO#gets`
- [ ] `StringIO#rewind`, `StringIO#pos`, `StringIO#string`

**実装方針**: Ruby で実装可能。IO インターフェースの部分実装。

### 2.4 Logger

- [ ] `Logger.new(output)`
- [ ] `Logger#debug`, `#info`, `#warn`, `#error`, `#fatal`
- [ ] `Logger#level`, `Logger#formatter`

### 2.5 Monitor

現在スタブのみ。シングルスレッドのままでも、正しいインターフェースが必要。

- [ ] `Monitor#enter`, `Monitor#exit`, `Monitor#synchronize`
- [ ] `MonitorMixin` モジュール

### 2.6 JSON (完全実装)

- [ ] `JSON.parse` (既存パーサーの拡張)
- [ ] `JSON.generate`, `JSON.dump`
- [ ] `JSON.pretty_generate`
- [ ] `#to_json` メソッド (各コアクラス)

### 2.7 URI

- [ ] `URI.parse`, `URI.encode_www_form`, `URI.decode_www_form`
- [ ] `URI::Generic`, `URI::HTTP`, `URI::HTTPS`

### 2.8 SecureRandom

- [ ] `SecureRandom.hex`, `SecureRandom.uuid`
- [ ] `SecureRandom.random_bytes`, `SecureRandom.alphanumeric`

**推定作業量**: 大
**依存関係**: フェーズ 1 の一部

---

## フェーズ 3: ActiveSupport コア

**目標**: ActiveSupport の中核機能を動かす。
全てを実装する必要はなく、ActiveRecord が使う部分に集中する。

### 3.1 ActiveSupport::Concern

ActiveRecord のモジュール設計の基盤。`included` ブロックと `class_methods` を提供。

- [ ] `ActiveSupport::Concern` モジュール
- [ ] `included` / `class_methods` DSL
- [ ] 依存関係の自動解決

**ポイント**: `Module#included` フックと `extend` を利用。monoruby は既にサポート済み。

### 3.2 コアエクステンション (core_ext)

ActiveSupport が Ruby コアクラスに追加するメソッド群。ActiveRecord が使うものに限定。

- [ ] `String` 拡張: `#camelize`, `#underscore`, `#constantize`, `#pluralize`, `#singularize`, `#tableize`, `#classify`, `#foreign_key`, `#demodulize`, `#blank?`, `#present?`, `#squish`
- [ ] `Object` 拡張: `#blank?`, `#present?`, `#presence`, `#try`, `#try!`, `#in?`
- [ ] `NilClass` / `TrueClass` / `FalseClass`: `#blank?`
- [ ] `Hash` 拡張: `#stringify_keys`, `#symbolize_keys`, `#deep_symbolize_keys`, `#reverse_merge`, `#except`, `#slice`, `#extract!`, `#with_indifferent_access`
- [ ] `Array` 拡張: `#extract_options!`, `#wrap`, `#in_groups_of`
- [ ] `Module` 拡張: `#delegate`, `#delegate_missing_to`, `#mattr_accessor`
- [ ] `Class` 拡張: `#class_attribute`, `#cattr_accessor`
- [ ] `Integer` / `Numeric` 拡張: `#ordinalize`, `#bytes`, `#kilobytes`, `#megabytes`, `#hours`, `#days`, `#weeks`, `#ago`, `#from_now`
- [ ] `Regexp` 拡張: `#multiline?`
- [ ] `Time` 拡張: `#change`, `#advance`, `#beginning_of_day`, `#end_of_day`

### 3.3 HashWithIndifferentAccess

Rails のパラメータ処理等で多用。

- [ ] `ActiveSupport::HashWithIndifferentAccess`
- [ ] 文字列キーとシンボルキーの透過的アクセス

### 3.4 Inflector (語形変換)

テーブル名 ↔ クラス名の変換に必須。

- [ ] `ActiveSupport::Inflector`
- [ ] 英語の複数形/単数形変換ルール
- [ ] `camelize`, `underscore`, `tableize`, `classify`
- [ ] カスタム語形変換ルールの登録

### 3.5 Callbacks

ActiveRecord のライフサイクルフック基盤。

- [ ] `ActiveSupport::Callbacks`
- [ ] `define_callbacks`, `set_callback`, `run_callbacks`
- [ ] `:before`, `:after`, `:around` フィルタ

### 3.6 その他必須コンポーネント

- [ ] `ActiveSupport::Autoload` (自動ロード機構)
- [ ] `ActiveSupport::LazyLoadHooks` (`ActiveSupport.on_load`)
- [ ] `ActiveSupport::Configurable`
- [ ] `ActiveSupport::Notifications` (計装、SQL ログ等)
- [ ] `ActiveSupport::DescendantsTracker` (サブクラス追跡)

**推定作業量**: 大
**依存関係**: フェーズ 1, 2

---

## フェーズ 4: ActiveModel

**目標**: ActiveRecord の基盤となるモデル抽象レイヤーを動かす。

### 4.1 属性 (Attributes)

- [ ] `ActiveModel::Attributes`
- [ ] `attribute` DSL
- [ ] 型キャスティング基盤

### 4.2 バリデーション

- [ ] `ActiveModel::Validations`
- [ ] `validates`, `validate`
- [ ] 組み込みバリデータ: `presence`, `uniqueness`, `length`, `format`, `numericality`

### 4.3 コールバック

- [ ] `ActiveModel::Callbacks` (ActiveSupport::Callbacks を利用)
- [ ] `before_validation`, `after_validation` 等

### 4.4 ダーティトラッキング

- [ ] `ActiveModel::Dirty`
- [ ] `changed?`, `changes`, `previous_changes`
- [ ] `*_changed?`, `*_was`, `*_previously_changed?`

### 4.5 名前・変換

- [ ] `ActiveModel::Naming`
- [ ] `ActiveModel::Conversion`
- [ ] `ActiveModel::Translation`

**推定作業量**: 中
**依存関係**: フェーズ 3

---

## フェーズ 5: データベースアダプタ (SQLite3)

**目標**: SQLite3 を介したデータベース操作を可能にする。

### 5.1 SQLite3 バインディング

monoruby の FFI/Fiddle で `libsqlite3.so` を呼び出す。

- [ ] `sqlite3_open`, `sqlite3_close`
- [ ] `sqlite3_prepare_v2`, `sqlite3_step`, `sqlite3_finalize`
- [ ] `sqlite3_bind_*` (パラメータバインド)
- [ ] `sqlite3_column_*` (結果取得)
- [ ] `sqlite3_errmsg` (エラーメッセージ)
- [ ] `sqlite3_exec` (単純なSQL実行)

**実装方針**: `Fiddle` を使って libsqlite3.so を dlopen し、関数を呼び出す。
CRuby の sqlite3 gem の API を模倣した Ruby ラッパーを作る。

### 5.2 sqlite3 gem 互換レイヤー

- [ ] `SQLite3::Database.new(path)`
- [ ] `SQLite3::Database#execute(sql, params)`
- [ ] `SQLite3::Database#prepare(sql)`
- [ ] `SQLite3::Statement#execute`, `#step`, `#columns`
- [ ] `SQLite3::ResultSet`

### 5.3 Fiddle::Closure の実装

SQLite3 のコールバック機能に必要。現在 "not yet supported"。

- [ ] `Fiddle::Closure` / `Fiddle::Closure::BlockCaller`
- [ ] コールバック関数ポインタの生成

**推定作業量**: 中
**依存関係**: フェーズ 1 (Fiddle の拡張)

---

## フェーズ 6: ActiveRecord コア

**目標**: ActiveRecord の基本的な CRUD 操作を動かす。

### 6.1 接続管理

- [ ] `ActiveRecord::Base.establish_connection`
- [ ] コネクションアダプタの抽象化
- [ ] SQLite3 アダプタ

### 6.2 スキーマ / マイグレーション

- [ ] `ActiveRecord::Schema.define`
- [ ] `create_table`, `add_column`, `add_index`
- [ ] `ActiveRecord::Migration` (基本的なDSL)
- [ ] `ActiveRecord::SchemaDumper`

### 6.3 基本 CRUD

- [ ] `ActiveRecord::Base` 基底クラス
- [ ] `Model.create`, `Model.new` + `#save`
- [ ] `Model.find`, `Model.find_by`
- [ ] `Model.where`, `Model.order`, `Model.limit`
- [ ] `Model#update`, `Model#destroy`
- [ ] `Model.all`, `Model.first`, `Model.last`

### 6.4 Arel (クエリビルダー)

Rails 7+ では ActiveRecord に内蔵。

- [ ] `Arel::Table`, `Arel::SelectManager`
- [ ] 基本的な SQL 構文ノード (SELECT, WHERE, ORDER BY, LIMIT, JOIN)
- [ ] バインドパラメータのサポート

### 6.5 属性 API

- [ ] カラム型の自動検出 (INTEGER, TEXT, REAL, BLOB)
- [ ] 型キャスティング (String ↔ Integer, Time ↔ String 等)
- [ ] `ActiveRecord::Type::*` (String, Integer, Float, Boolean, DateTime, etc.)

### 6.6 アソシエーション (基本)

- [ ] `has_many`, `belongs_to`, `has_one`
- [ ] 外部キーの自動推定
- [ ] Eager loading (`includes`)

### 6.7 スコープとクエリインターフェース

- [ ] `scope` DSL
- [ ] `ActiveRecord::Relation` (遅延評価クエリ)
- [ ] チェーン可能なクエリメソッド

**推定作業量**: 特大
**依存関係**: フェーズ 3, 4, 5

---

## 推奨実装順序

```
フェーズ 1 (言語機能) ──────────────────────┐
                                              │
フェーズ 2 (標準ライブラリ) ─────────────────┤
                                              ├─→ フェーズ 3 (ActiveSupport)
フェーズ 5.1-5.2 (SQLite3 バインディング) ──┤       │
                                              │       ↓
                                              │   フェーズ 4 (ActiveModel)
                                              │       │
                                              │       ↓
                                              └─→ フェーズ 6 (ActiveRecord)
```

フェーズ 1, 2, 5 は並行して進められる。
フェーズ 3 → 4 → 6 は順序依存。

---

## マイルストーンと検証ポイント

### M1: 最小限の ActiveSupport (フェーズ 1 + 2 + 3 の一部)
```ruby
require 'active_support'
require 'active_support/core_ext/string'
"hello_world".camelize  #=> "HelloWorld"
"User".tableize         #=> "users"
```

### M2: SQLite3 直接操作 (フェーズ 5)
```ruby
require 'sqlite3'
db = SQLite3::Database.new(':memory:')
db.execute("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
db.execute("INSERT INTO users (name) VALUES (?)", ["Alice"])
db.execute("SELECT * FROM users")  #=> [[1, "Alice"]]
```

### M3: ActiveRecord 基本 CRUD (フェーズ 6.1-6.3)
```ruby
require 'active_record'
ActiveRecord::Base.establish_connection(adapter: 'sqlite3', database: ':memory:')
ActiveRecord::Schema.define do
  create_table :users do |t|
    t.string :name
    t.integer :age
  end
end

class User < ActiveRecord::Base
end

user = User.create(name: "Alice", age: 30)
User.find(user.id)
User.where(name: "Alice").first
user.update(age: 31)
user.destroy
```

### M4: アソシエーション (フェーズ 6.6)
```ruby
class User < ActiveRecord::Base
  has_many :posts
end

class Post < ActiveRecord::Base
  belongs_to :user
end

user = User.create(name: "Alice")
user.posts.create(title: "Hello")
User.includes(:posts).all
```

---

## リスクと対策

| リスク | 影響 | 対策 |
|--------|------|------|
| ActiveSupport の依存範囲が広すぎる | 実装量の爆発 | ActiveRecord が使う部分のみに絞る。`require` で未実装部分に触れた時に `LoadError` ではなくスタブを返す |
| Encoding 周りの非互換 | 文字列処理の不具合 | SQLite は UTF-8 前提で割り切る |
| Fiddle::Closure が必要 | SQLite3 コールバック不可 | libffi クレートで Rust 側に実装 |
| スレッド安全性 | コネクションプール不可 | シングルスレッド前提で進める。Mutex はスタブで対応 |
| gem のバージョン差異 | API 不一致 | ActiveRecord 7.1 を基準に固定 |

---

## 最初に取り組むべきこと

**まず M2 (SQLite3 直接操作) を目指すことを推奨する。**

理由:
1. ActiveSupport/ActiveRecord は巨大で、全体をカバーするのは長期戦になる
2. SQLite3 バインディングは比較的独立しており、既存の Fiddle インフラで実装可能
3. データベース操作ができれば、ActiveRecord を「下から」積み上げられる
4. 目に見える成果が早く得られ、モチベーション維持になる

同時に、フェーズ 1 の欠損メソッドを逐次追加していく。
ActiveSupport の各ファイルを `require` して、エラーになった箇所から順に対応する
「ボトムアップ + エラードリブン」アプローチが現実的。
