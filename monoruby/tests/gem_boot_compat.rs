//! Language / stdlib gaps found booting ruby-bench's fluentd and
//! lobsters benchmarks (each is what stopped the boot at the time, once
//! the C extensions were stood in for):
//!
//! - `(...)` forwarded from inside a lambda *literal* in a pure
//!   forwarding trampoline passed the lazy-forwarding marker on as an
//!   Integer (Rails' `MiddlewareStackProxy#use(...)` registers a
//!   `-> middleware { middleware.use(...) }`).
//! - `extend` on a singleton class landed the module on `Class`'s
//!   singleton, i.e. on every class's, so a `class << self; extend
//!   Forwardable` anywhere made ActiveSupport's `Module#delegate`
//!   resolve to `Forwardable#delegate`.
//! - `Socket::Constants` was a class (io-endpoint `include`s it).
//! - `require "enumerator"` (and the other built-in feature names CRuby
//!   pre-lists in `$LOADED_FEATURES`) raised LoadError.
//! - `StringScanner::Version` was missing (rexml 3.4 branches on it).
use monoruby::tests::*;

#[test]
fn forwarding_from_lambda_literal() {
    run_test(
        r##"
        class S
          def use(*a, **kw, &b); [a, kw, b ? :blk : nil]; end
        end
        $s = S.new
        def a(...); l = -> { $s.use(...) }; l.call; end
        def b(...); l = -> { $s.send(:use, ...) }; l.call; end
        def c(...); l = ->(z) { [z, $s.use(...)] }; l.call(9); end
        def d(...); l = lambda { $s.use(...) }; l.call; end
        res = []
        res << a(Integer) << a(Integer) { } << a(Integer, 2) { } << a { } << a(k: 1)
        res << b(Integer) { } << c(String, 1) << d(Integer)
        res
        "##,
    );
}

#[test]
fn extend_on_singleton_class() {
    run_test(
        r##"
        module M; def hi; :hi; end; end
        class Foo
          class << self
            extend M
          end
        end
        class Bar; end
        Bar.singleton_class.extend(M)
        module Mod; end
        Mod.singleton_class.extend(M)
        [
          Class.singleton_class.include?(M),
          Foo.singleton_class.singleton_class.include?(M),
          Foo.singleton_class.respond_to?(:hi),
          Foo.singleton_class.hi,
          Class.respond_to?(:hi),
          String.singleton_class.respond_to?(:hi),
          Bar.singleton_class.singleton_class.ancestors.first(3).map(&:inspect),
          Mod.singleton_class.singleton_class.include?(M),
          Module.singleton_class.include?(M),
        ]
        "##,
    );
}

#[test]
fn delegate_resolves_to_module_after_singleton_extend() {
    run_test_once(
        r##"
        require "forwardable"
        class Module
          def delegate(*names, to:)
            names.each { |n| define_method(n) { |*a| send(to).send(n, *a) } }
          end
        end
        class Reg
          class << self
            extend Forwardable
          end
        end
        class Other
          class << self
            delegate :size, to: :list
            def list; [1, 2, 3]; end
          end
        end
        Other.size
        "##,
    );
}

#[test]
fn socket_constants_is_a_module() {
    run_test_once(
        r##"
        require "socket"
        class W
          include ::Socket::Constants
        end
        [Socket::Constants.class, Socket::Constants::AF_INET == Socket::AF_INET, W::SOCK_STREAM == Socket::SOCK_STREAM]
        "##,
    );
}

#[test]
fn builtin_features_are_preloaded() {
    run_test_once(
        r##"
        %w[enumerator thread fiber rational complex ruby2_keywords set].map { |f| require f }
        "##,
    );
}

#[test]
fn strscan_version() {
    run_test_once(
        r##"
        require "strscan"
        [StringScanner::Version, StringScanner::Version >= "1.0.0"]
        "##,
    );
}

/// net-imap's response parser (eager-loaded by Rails through `mail`)
/// builds its UTF-8 byte classes as `/[\xC2-\xDF][\x80-\xBF]/n` pieces
/// and interpolates their `Regexp.union` into `/#{…}+/`: the literal
/// must come out BINARY and byte-matched, as must `Regexp.new` of a
/// BINARY source that spells its high bytes as `\xHH` escapes. And
/// `Regexp#match?` read a BINARY subject lossily, so `/\xC3/n` never
/// matched a lone "\xC3".
#[test]
fn binary_regexp_from_escapes() {
    run_test_once(
        r##"
        u = Regexp.union(/[\x01-\x7f]/n, /[\xC2-\xDF][\x80-\xBF]/n)
        r = /#{u}+/
        [
          u.encoding.name, r.encoding.name, r.fixed_encoding?,
          r.match?("a\xC3\x9F".b), r.match("a\xC3\x9F".b)[0].bytes,
          /\xC3/n.match?("\xC3".b), /[\xC2-\xDF]/n.match?("\xC3".b),
          /[\xC2-\xDF]/n.match?("a\xC3".b, 1), /[\xC2-\xDF]/n.match?("a\xC3".b, 2),
          /[\xC2-\xDF]/n.match?("a\xC3".b, 5), /[\xC2-\xDF]/n.match?("a\xC3".b, -1),
          Regexp.new("[\xC2-\xDF]".b).encoding.name, Regexp.new("[\xC2-\xDF]".b).match?("\xC3".b),
          Regexp.new("(?-mix:[\xC2-\xDF])+".b).match?("\xC3".b),
          Regexp.new("abc".b).encoding.name, Regexp.new("abc".b).fixed_encoding?,
          Regexp.new("abc".b).match?("xabc\u00e9"),
          (/#{/\xC2/n}x/).encoding.name,
          (/#{/\xC2/n}#{/\u00e9/}/ rescue $!.class),
        ]
        "##,
    );
}

/// ActionView's `capture(*, **) { value = yield(*, **) }`: an anonymous
/// `*` / `**` forwarded from inside a block or lambda of the binding
/// method was read at depth 0 — the block's own frame — so the yield
/// received nothing and `form_with` handed its builder as `nil`.
#[test]
fn anonymous_splat_forwarding_from_nested_scope() {
    run_test(
        r##"
        def h(*a, **k); [a, k]; end
        def a(*, **, &block); v = nil; [1].each { v = yield(*, **) }; v; end
        def b(*, **, &block); v = nil; [1].each { v = block.call(*, **) }; v; end
        def d(*, **); v = nil; l = -> { v = yield(*, **) }; l.call; v; end
        def e(*, &block); v = nil; [1].each { v = yield(*) }; v; end
        def f(**, &block); v = nil; [1].each { v = yield(**) }; v; end
        def g(*); [[1]].map { |x| x.map { h(*) } }; end
        def j(*, **); [1].map { h(*, **) + [->(z) { h(*) }.call(0)] }; end
        def k(*, **); h(*, **); end
        [a(:x, k: 1) { |f, **kw| [f, kw] }, b(:x, k: 1) { |f, **kw| [f, kw] }, d(:x, k: 1) { |f, **kw| [f, kw] },
         e(:x) { |f| f }, f(k: 1) { |**kw| kw }, g(1, 2), j(1, k: 2), k(3, z: 4)]
        "##,
    );
}

/// `require "date"` reopens Time with `to_date` / `to_datetime` /
/// `to_time`, and ActiveSupport's `Time#advance` — run on every
/// session-cookie expiry — goes `to_date.gregorian.advance(…)`; the
/// stand-in had only Date's side of the conversions and no calendar
/// switches.
#[test]
fn time_to_date_conversions() {
    run_test_once(
        r##"
        require "date"
        t = Time.new(2026, 9, 17, 1, 2, 3.5r, "+09:00")
        [t.to_date.to_s, t.to_datetime.to_s, t.to_datetime.iso8601(3),
         t.to_datetime.offset, t.to_datetime.sec_fraction, t.to_time.equal?(t),
         Time.new(2026, 1, 2, 3, 4, 5, "-05:00").to_datetime.to_s,
         DateTime.new(2026, 9, 17, 1, 2, 3.5r, "+09:00").to_time.to_s,
         Date.new(2026, 9, 17).to_time.hour, (Time.now + 86400).to_date > Time.now.to_date,
         t.to_date.gregorian.inspect, t.to_date.gregorian.start, t.to_date.gregorian == t.to_date,
         t.to_date.new_start(Date::ENGLAND).start.to_i, (t.to_date.gregorian >> 1).to_s]
        "##,
    );
}

/// ActiveSupport's cache coder reads its header with `unpack1("@3E")`
/// and friends: `unpack1` cut the parsed template to its first
/// directive, so a leading `@` / `x` / `X` yielded nothing and every
/// cached entry's `expires_at` came back nil.
#[test]
fn unpack1_after_position_directives() {
    run_test_once(
        r##"
        s = "\x00\x11".b + [1, 123.5, -1].pack("CEl<") + "payload"
        [s.unpack1("@2C"), s.unpack1("@3E"), s.unpack1("@11l<"), "abcdef".unpack1("x2a2"),
         "abcdef".unpack1("@1X1a2"), "abc".unpack1("@1a"), "abcdef".unpack1("x2 a2"),
         "abcd".unpack1("@2"), s.unpack1("@2C", offset: 0), "xxabcd".unpack1("@1a2", offset: 2),
         s.unpack("@2C@3E@11l<"), "\x01\x02\x03\x04".unpack1("L"), "ab".unpack1("C")]
        "##,
    );
}

/// lobsters' `Story#send_referrer?` does `created_at <= 1.hour`: a
/// `TimeWithZone` against an `ActiveSupport::Duration`. That works in
/// CRuby because `Time#<=` is Comparable's, which goes through the
/// `<=>` ActiveSupport redefines, and because `Date#<=>` compares with
/// a Numeric by `ajd` and lets anything else `coerce`. monoruby's Time
/// had native `<` / `<=` / `>` / `>=` that refused every non-Time, and
/// its Date compared only with Dates.
#[test]
fn time_and_date_compare_through_spaceship() {
    run_test_once(
        r##"
        require "date"
        t = Time.at(1_700_000_000)
        errs = [1, "a", nil, :s, 1.5].map { |o| (t <= o) rescue $!.message }
        class Time
          alias __orig_cmp <=>
          def <=>(o); o.is_a?(Time) ? __orig_cmp(o) : (o == :less ? -1 : 1); end
        end
        overridden = [t < 5, t > 5, t <= 5, t >= 5, t == 5, t < :less, t.between?(1, 2), t < t + 1, t >= Time.at(0)]
        class Time
          def <=>(o); o.is_a?(Time) ? __orig_cmp(o) : (o == :less ? -0.5 : 0.5); end
        end
        floaty = [t < 5, t > 5, t <= :less, t >= :less]
        class Time
          def <=>(o); o.is_a?(Time) ? __orig_cmp(o) : "x"; end
        end
        stringy = (t < 5 rescue $!.class)
        class Time; alias <=> __orig_cmp; end
        d = DateTime.new(2026, 9, 17, 1, 2, 3, "+09:00")
        scalar = Class.new do
          attr_reader :value
          def initialize(v); @value = v; end
          def <=>(o); value <=> o.value; end
          def coerce(other); [self.class.new(other), self]; end
        end
        dur = scalar.new(3600)
        [errs, overridden, floaty, stringy,
         d <=> 3600, Date.new(2026, 9, 17) <=> 2461301.5r, Date.new(2026, 9, 17).ajd, d.ajd, d.amjd,
         Date.new(2026, 9, 17) <=> 2461300, Date.new(2026, 9, 17) <=> "x",
         Date.new(2026, 9, 17) <=> DateTime.new(2026, 9, 17, 0, 0, 1),
         DateTime.new(2026, 9, 17, 0, 0, 1) <=> Date.new(2026, 9, 17),
         d <=> dur, d <= dur, Date.new(2026, 9, 17) <= dur,
         Date.new(2026, 9, 17) == 2461300.5r, Date.new(2026, 9, 17) === 2461301,
         Date.new(2026, 9, 17) === Date.new(2026, 9, 17), Date.new(2026, 9, 17) === DateTime.new(2026, 9, 17, 5)]
        "##,
    );
}

/// A finalized object's `object_id` must stay unique for as long as its
/// finalizer is registered: Tempfile's `FinalizerManager` keys its open
/// files by it, and monoruby's ids are addresses, so a recycled cell
/// handed a later Tempfile the same id and the finalizer `close`d nil
/// (lobsters' request bodies go through `Rack::RewindableInput`).
#[test]
fn finalized_object_ids_stay_unique() {
    run_test_once(
        r##"
        ids = []
        300.times do |i|
          o = Object.new
          ids << o.object_id
          ObjectSpace.define_finalizer(o, proc { |id| })
          o = nil
          GC.start if i % 7 == 0
        end
        [ids.uniq.size, ids.size]
        "##,
    );
}

/// The finalizer registry after the change above: a finalizer `==` to
/// one already registered on the object is recorded once, and `dup` /
/// `clone` copy the object's finalizers (CRuby does both).
#[test]
fn finalizer_registry_dedupe_and_copy() {
    run_test_once(
        r##"
        o = Object.new
        pr = proc { |id| }
        a = ObjectSpace.define_finalizer(o, pr)
        b = ObjectSpace.define_finalizer(o, pr)
        c = ObjectSpace.define_finalizer(o, proc { |id| })
        d = o.dup
        e = o.clone
        ObjectSpace.undefine_finalizer(d)
        [a[0], b[1].equal?(pr), c[1].equal?(pr), a[1].equal?(b[1]), d.frozen?, e.frozen?]
        "##,
    );
}
