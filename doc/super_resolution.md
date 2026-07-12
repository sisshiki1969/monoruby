# super のメソッド名解決と呼び出し元 PC を用いた実装

`super` は「いま実行中のメソッドと**同じ名前**のメソッドを、祖先チェーンの
**現在位置より先**から探して呼ぶ」命令である。単純に見えるが、「同じ名前とは
どの名前か」「現在位置とはどこか」の 2 点が自明でなく、CRuby は両方をメソッド
エントリ(callable method entry)に持たせて解決している。monoruby はフレームに
メソッドエントリを持たない(FuncId しか持たない)ため、同じ情報を
**呼び出し元 PC(cont-frame スロット)から復元**する。本書はその機構を説明する。

対象ソース:

- `monoruby/src/codegen/runtime.rs` — `entered_by` / `super_run` /
  `super_resolution` / `find_super` / `defined_super` / `find_method`
- `monoruby/src/globals/store/class.rs` — `body_dispatched_by` /
  `super_occurrences` / `check_super` / `check_super_at` /
  `change_method_visibility_for_class`
- `monoruby/src/globals/store.rs` — `MethodTableEntry`
- `monoruby/src/codegen/jitgen/compile/method_call.rs` — JIT 側の
  ambiguous-super ガード

---

## 1. CRuby の意味論

CRuby では、メソッド呼び出しのたびに **callable method entry (cme)** が
フレームに紐付く。cme は以下を保持する:

| フィールド | 意味 | super での役割 |
|---|---|---|
| `called_id` | 呼び出しに使われた名前 | (直接は使わない) |
| `def->original_id` | 定義時の名前 | **super はこの名前で検索する** |
| `defined_class` | メソッドが見つかった ICLASS(チェーン上の位置) | **この位置の直後から検索を始める** |

この 2 つの情報がどう効くかを、問題になるケースごとに見る。

### 1.1 一つの本体が複数の名前を持つ場合(define_method)

```ruby
sub = Class.new(sup) do
  [:a, :b].each do |name|
    define_method(name) { super() }
  end
end
```

`define_method` は呼び出しごとに**独立したメソッドエントリ**を作る
(`original_id` はそれぞれ `:a` / `:b`)。ブロック本体は共有されていても、
`sub.new.a` の super は `:a` を、`sub.new.b` の super は `:b` を検索する。
つまり super の検索名は「本体に焼き付いた名前」ではなく
「**その呼び出しでディスパッチされたエントリの original_id**」である。

### 1.2 alias の場合

```ruby
class Alias3 < Alias2
  alias_method :name3, :name   # Alias2#name を :name3 として登録
end
Alias3.new.name3   # 中の super は :name で検索される
```

alias が作るエントリは `original_id = :name` を保持する。`name3` で呼ばれても
super は **元の定義名 `:name`** で検索する。さらに `defined_class` は
**元の定義位置(Alias2)** を指すため、super は Alias2 の直後
(= Alias1)から探し始める。alias を登録した Alias3 は位置として数えない。

### 1.3 同じ本体がチェーンに複数回現れる場合

```ruby
class Base
  def self.whatever
    mod = Module.new do
      def a(ary); ary << "anon"; super; end
    end
    include mod
  end
  def a(ary); ary << "non-anon"; end
end
class Twice < Base
  whatever   # 匿名モジュール 1 個目
  whatever   # 2 個目(同じ `def a` バイトコードの再実行)
end
Twice.new.a([])  #=> ["anon", "anon", "non-anon"]
```

チェーンは `Twice → mod2 → mod1 → Base`。mod2#a の super は mod1#a
(**同じ本体!**)を呼び、mod1#a の super が Base#a を呼ぶ。CRuby では各
フレームの cme が異なる `defined_class`(mod2 の ICLASS / mod1 の ICLASS)を
持つため、同じ本体でも「チェーン上のどの出現か」を区別できる。

### 1.4 可視性の再宣言は「位置」ではない

```ruby
class C
  include A   # private def derp(msg)
  include B   # private def derp; super('...'); end
  public :derp
end
```

`public :derp` は C に **ZSUPER メソッドエントリ**(可視性だけを上書きする
委譲エントリ)を作る。これは定義ではないので、B#derp の super の起点は
あくまで B の位置であり、C を位置として数えて B 自身へ再入してはならない。

---

## 2. monoruby の表現とギャップ

monoruby のフレーム(LFP/CFP)が持つメソッド識別情報は **FuncId のみ**である。
`FuncInfo` には名前が 1 つ焼き付く(`FuncInfo::name()`)が、これは
「最初に登録されたときの名前」であり、上記 1.1〜1.3 の情報をすべて失う:

- define_method で複数名に登録された本体 → 名前は最初の 1 つだけ
- 同じ `def` バイトコードの再実行 → 同一 FuncId がチェーンの複数位置に登録
  され、フレームからはどの出現か分からない
- alias → FuncId は共有(エントリ側に `original_name` は残る)

一方、メソッドテーブル側(`MethodTableEntry`)には必要な情報が揃っている:

```rust
pub(crate) struct MethodTableEntry {
    owner: ClassId,
    func_id: Option<FuncId>,
    visibility: Visibility,
    is_basic_op: bool,
    original_name: IdentId,     // alias / define_method(Method) 経由の元定義名
    visibility_shadow: bool,    // `public :inherited` 型の可視性シャドウ
}
```

欠けているのは「**このフレームはどのエントリでディスパッチされたか**」という
動的情報だけである。これを呼び出し元 PC から復元する。

---

## 3. 呼び出し元 PC(cont-frame スロット)

### 3.1 スロットの位置と書き込み

すべての呼び出しで、callee フレームの **CFP+24**(`Cfp::caller_pc_slot`,
`executor/frame.rs`)に「呼び出し元の**コールサイト**のバイトコード PC」が
入る。書き込み経路は 3 つ:

1. **VM tier** — `push_cont_frame`(`arch/x86_64/vmgen/method_call.rs`):
   `subq rsp, 8; pushq r13; subq [rsp], 16`。ディスパッチ時の r13 は
   コールサイト + 16(send は 2 バイトコード単位 = 32 バイト)なので
   16 を引いてコールサイト先頭を保存する。aarch64 は最初からコールサイト PC を
   保存する。
2. **JIT tier**(#889) — cont-frame 16 バイト領域は cont モードの
   `FprSave` が予約済み(xmm 退避はその上に置かれる)なので、
   `AsmInst::ContFramePc` が `movq [rsp], pc`(a64: `str x10, [sp]`)を
   send / specialized send / yield / specialized yield の 4 箇所すべてで発行する。
3. **invoker / native 経路** — 書かれない(ゴミが残る)。読む側が必ず検証する。

### 3.2 読み出しと検証

読み手(`Kernel#caller` と本機構)は共通のパターンで検証する
(`runtime.rs::entered_by`):

```
slot != 0 かつ slot % 8 == 0
→ BytecodePtr として解釈
→ 呼び出し元フレーム(cfp.prev())の iseq の範囲内か(contains_pc)
→ その位置のオペコードが send 系か
```

send 系オペコード(`bytecodegen/encode.rs`):

| opcode | 命令 |
|---|---|
| 30 / 31 | メソッド呼び出し(simple / generic) |
| 32 / 33 | **super** |
| 34 / 35 | yield |

30〜33 の第 1 ワード下位 32 ビットが **CallSiteId** であり、
`CallSiteInfo::name` から「**呼び出しに使われた名前**」が得られる。
検証に失敗した場合(invoker 境界など)は `None` を返し、後述の
フォールバックに落ちる。

---

## 4. 解決アルゴリズム(`find_super`)

`super` 実行時、ランタイムは次の 2 つを復元する
(`runtime.rs::super_resolution`)。

### 4.1 呼び出し名の復元 → original_name への写像

1. メソッドフレームを特定する。ブロック内の super は外側メソッドに属するため、
   `lfp.outermost()`(proc-method 境界で停止する外側連鎖)でメソッド LFP を
   求め、その LFP を実行している CFP まで下る。
2. そのフレームの cont-frame スロットを `entered_by` で復号し、通常 send
   (opcode 30/31)なら CallSiteId → `CallSiteInfo::name` =
   **呼ばれた名前** を得る。
3. 呼ばれた名前をレシーバクラスのメソッドテーブルで引き、
   **そのエントリが本当にこのフレームの本体へディスパッチするか**検証する:
   - `entry.func_id == 実行中 FuncId`、または
   - エントリが proc-method ラッパー(`FuncKind::Proc`)で
     `proc.func_id() == 実行中 FuncId`
     (define_method はラッパー FuncId を登録するが、実行フレームには
     ブロック本体の FuncId が乗るため)。
4. 検証に通れば **`entry.original_name`** を検索名とする。これで
   define_method 複数名(1.1: original_name = 各インストール名)と
   alias(1.2: original_name = 元定義名)の両方が CRuby と一致する。
5. 復元できない場合は従来どおり `FuncInfo::name()`(焼き付け名)に
   フォールバックする。

### 4.2 出現インデックス(occurrence)の復元

同じ本体がチェーンに複数回現れるケース(1.3)のために、
「このフレームはその本体の**何番目の出現**か」を数える
(`runtime.rs::super_run`):

```
k = 1, cfp = メソッドフレーム
loop:
  entered_by(cfp) が super オペコード(32/33)で、かつ
  呼び出し元フレームが 同じ本体(method_func_id 一致)を
  同じレシーバ(self 一致)で実行している
    → k += 1 して呼び出し元へ(super 連鎖の 1 ホップ)
  通常 send → 連鎖の底。 (k, そのコールサイト, exact=true)
  別本体からの super → (k, なし, exact=true)
  復号失敗 → (k, なし, exact=false)
```

super 連鎖であることをオペコードで確認するのが重要で、単なる再帰呼び出し
(`obj.a` を a の中から呼ぶ)は連鎖を切る。再帰はディスパッチをチェーン先頭から
やり直すので、出現カウントもリセットされるのが正しい。

### 4.3 チェーン検索(`check_super_at`)

```rust
check_super_at(self_class, current_fid, name, occurrence: Option<usize>)
```

チェーンを歩き、**定義位置**を `body_dispatched_by` で判定する:

> クラス/モジュール m の**自身の**メソッドテーブルが `name` を実行中本体へ
> ディスパッチする(直接、または proc-method ラッパー経由)。ただし
> `visibility_shadow` エントリは除外。

- 名前で引く(FuncId の登録位置全部ではなく)ことで alias 登録先(1.2)を
  位置から除外する。
- `visibility_shadow` の除外が 1.4 に対応する。owner 登録の有無では判定
  できない(同じクラスへの `alias_method` が owner を汚染するため)。

`occurrence = Some(k)` なら **k 番目**の定義位置から先を検索し、見つかった
ものを(**同一 FuncId でも**)返す — これが 1.3 の「同じ本体へ super する」
挙動である。`None`(復号失敗時)なら従来のヒューリスティック
「同一 FuncId が見つかったら次の出現まで歩き続ける」で前進を保証する。

名前ベースの位置が 1 つも見つからない場合(stale な可視性シャドウが
差し替え済みの旧本体をディスパッチした場合など)は、#890 以前の
**owner 登録ベースの走査**にフォールバックし、それも失敗したら
レシーバクラスからの直接検索(UnboundMethod#bind 対応)を試す。

---

## 5. キャッシュとの整合

super の解決結果はコールサイトのインラインキャッシュに
(レシーバクラス, FuncId) で刻まれるが、上記のとおり super の正解は
**フレーム依存**になり得る(同一コールサイト・同一レシーバクラスでも、
呼び名や出現位置で行き先が変わる)。そのため:

- **VM**: `find_super` が cacheable フラグを返す。
  `!is_block_style && super_occurrences(...) <= 1` のときだけキャッシュ可。
  不可なら `find_method` はキャッシュタグに **ClassId 0** を返す
  (`ClassId` は `NonZeroU32` なので実クラスと一致せず、そのサイトは
  毎回スローパスで再解決される)。
- **JIT**(`jitgen/compile/method_call.rs`): コンパイル時に同じ条件
  (mother FuncId が block-style、または出現数 > 1)を検出したら、その
  super サイトは **plain-deopt**(VM 実行)にする。`Recompile` にすると
  VM がキャッシュを温めない(タグ 0)ため再コンパイルが収束しない。
- コンパイル時/キャッシュ更新用の `check_super`(3 引数版)は、出現数 > 1
  なら `None` を返して辞退する。

`defined?(super)` (`defined_super`) も同じ `super_resolution` +
`check_super_at` を使い、実行時セマンティクスと一致させている。

---

## 6. 既知の限界

- **invoker 境界**: `Method#call` / `send` / Fiber などで入ったフレームは
  cont-frame スロットが無効なので、呼び出し名の復元も出現カウントも
  フォールバックに落ちる(焼き付け名 + 旧ヒューリスティック)。
  define_method 複数名のメソッドを `send` で呼ぶと、super の検索名は
  焼き付け名になる。
- **可視性シャドウの staleness**: `public :derp` はシャドウエントリに
  継承先の FuncId をコピーするため、その後に継承元が再定義されると
  シャドウが旧本体をディスパッチする(CRuby の ZSUPER エントリは委譲なので
  この問題がない)。可視性を再宣言すればスーパークラス解決で再同期される。
  super 解決側は owner-walk フォールバックで旧本体からでも前進できる。
- 出現カウントは「連続する super 連鎖」を前提にしており、途中に
  invoker 境界が挟まると `exact=false` となり旧ヒューリスティックに
  切り替わる(過小カウントによる無限 super ループを防ぐため)。

## 7. 関連 PR

- #887 — VM tier がコールサイト PC を cont-frame スロットに保存、
  `Kernel#caller` の行番号解決
- #888 — specialized JIT 呼び出しの lazy 解決(#889 で置換)
- #889 — JIT/specialized 呼び出しも eager にコールサイト PC を保存
  (`AsmInst::ContFramePc`)
- #890 — 本書の super 解決機構
