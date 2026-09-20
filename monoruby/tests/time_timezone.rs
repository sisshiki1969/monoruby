extern crate monoruby;
use monoruby::tests::*;
use std::sync::{Mutex, MutexGuard, OnceLock};

// The system local timezone is read through libc (`tzset` / `localtime_r` /
// `mktime`), not chrono's `Local`, which resolves the zone once and caches
// it for the life of the process. CRuby re-reads `TZ` on every conversion,
// so `ENV['TZ'] = "Asia/Tokyo"` changes what `Time.now` and `Time.local`
// report from the next call on — and ruby/spec's `with_timezone` helper is
// built on exactly that. With the cached zone every such spec saw whatever
// zone the process started in.
//
// Its own test binary: the body rewrites the process's `TZ`, which would
// otherwise race with any other test reading a local time.

/// Every test in this file rewrites the process's `TZ`, so no two of
/// them can run at once. `cargo test` runs one binary's tests on
/// threads of a single process — nextest, which CI uses, gives each its
/// own — so they take this lock and the lock is what makes the file's
/// isolation real either way.
fn tz_lock() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

#[test]
fn local_times_follow_a_runtime_tz_change() {
    let _tz = tz_lock();
    run_test_once(
        r#"
        r = []
        old = ENV['TZ']
        ENV['TZ'] = 'Asia/Tokyo'
        r << Time.local(2020, 1, 1).utc_offset
        # No daylight saving in Japan: the same offset all year.
        r << Time.local(2020, 7, 1).utc_offset
        r << Time.new(2020, 1, 2, 3, 4, 5).to_s
        ENV['TZ'] = 'America/New_York'
        r << Time.local(2020, 1, 1).utc_offset
        r << Time.local(2020, 7, 1).utc_offset
        r << Time.at(0).getlocal.to_s
        ENV['TZ'] = 'Europe/Amsterdam'
        # 1970 was CET (+01:00) there, a rule chrono's cached zone could
        # never have applied.
        r << Time.local(1970, 1, 1).to_s
        r << Time.local(2020, 7, 1).utc_offset
        r << Time.local(2020, 1, 1).utc_offset
        ENV['TZ'] = 'UTC'
        r << Time.local(2020, 1, 1).utc_offset
        r << Time.local(2020, 1, 1).to_s
        ENV['TZ'] = old
        r
        "#,
    );
}

#[test]
fn zone_name_and_dst() {
    let _tz = tz_lock();
    // `Time#zone` was nil for every local time and `#dst?` was hardwired
    // to false, because `TimeInner::Local` could not tell a time in the
    // system zone from one at a plain offset — CRuby answers the zone's
    // abbreviation and a real DST flag for the first, nil and false for
    // the second. Both now come from libc (`tm_zone` / `tm_isdst`) and
    // are *captured when the time is localized*, as CRuby captures them:
    // a later `ENV['TZ'] = …` does not rewrite a Time that already
    // exists, while arithmetic — which localizes again at the new
    // instant — does move across a DST boundary.
    run_test_once(
        r#"
        old = ENV['TZ']
        ENV['TZ'] = 'America/New_York'
        r = []
        def row(t) = [t.zone, t.dst?, t.utc_offset, t.utc?]
        r << row(Time.local(2020, 1, 1))
        r << row(Time.local(2020, 7, 1))
        r << row(Time.new(2020, 1, 1))
        r << row(Time.new(2020, 1, 1, 0, 0, 0, "+09:00"))
        r << row(Time.new(2020, 1, 1, 0, 0, 0, 0))
        r << row(Time.new(2020, 1, 1, 0, 0, 0, "UTC"))
        r << row(Time.at(0))
        r << row(Time.at(0, in: "+09:00"))
        r << row(Time.utc(2020, 1, 1))
        r << row(Time.utc(2020, 7, 1).getlocal)
        r << row(Time.utc(2020, 7, 1).getlocal("+09:00"))
        r << row(Time.utc(2020, 7, 1).dup.localtime)
        # `#localtime` with no argument re-zones a fixed-offset time too.
        r << row(Time.new(2020, 7, 1, 0, 0, 0, "+09:00").dup.localtime)
        r << row(Time.local(2020, 7, 1).getutc)
        r << row(Time.local(2020, 7, 1) + 1)
        r << row(Time.local(2020, 7, 1).round)
        # Arithmetic across a DST boundary lands on the right side of it.
        s = Time.local(2020, 1, 1) + 86400 * 180
        r << [row(s), s.to_s]
        d = Time.local(2020, 7, 1) - 86400 * 180
        r << [row(d), d.to_s]
        r << Time.local(2020, 7, 1).to_a
        r << Time.new(2020, 7, 1, 0, 0, 0, "+09:00").to_a
        # `%Z` is the abbreviation, and empty for a plain offset.
        r << [Time.local(2020, 7, 1).strftime("%Z|%z"),
              Time.new(2020, 7, 1, 0, 0, 0, "+09:00").strftime("%Z|%z"),
              Time.utc(2020, 1, 1).strftime("%Z|%z")]
        r << Time.local(2020, 7, 1).zone.encoding.to_s
        t = Time.local(2020, 7, 1)
        ENV['TZ'] = 'Asia/Tokyo'
        # `t` keeps what it captured; a new Time gets the new zone.
        r << row(t)
        r << row(Time.local(2020, 7, 1))
        r << [t.getlocal.zone, t.dup.localtime.zone]
        # A dump carries the zone *name*; the rebuilt time is at a plain
        # offset, so it reports the name and `#dst?` false alike.
        r << Marshal.load(Marshal.dump(t)).then { |l| [l.zone, l.dst?, l.utc_offset] }
        r << Marshal.load(Marshal.dump(Time.new(2020, 7, 1, 0, 0, 0, "+09:00"))).then { |l| [l.zone, l.dst?] }
        r << Marshal.load(Marshal.dump(Time.utc(2020, 7, 1))).then { |l| [l.zone, l.utc?] }
        ENV['TZ'] = old
        r
        "#,
    );
}

#[test]
fn timezone_objects() {
    let _tz = tz_lock();
    // The timezone-object protocol. `Time.now(in: tz)` did not try it at
    // all — it handed the object straight to the offset parser, which
    // answered "can't convert … into an exact number" — and the offset
    // it derives comes from the *broken-down fields* of whatever
    // `#utc_to_local` returns, with that object's own `#zone` and
    // `#utc_offset` ignored, as CRuby reads them. `#getlocal` resolves a
    // zone *name* through the receiver class's `.find_timezone`, the
    // zone object rides along through arithmetic and rounding, and
    // `Marshal` dumps it by its `#name` and asks `.find_timezone` for it
    // back.
    run_test_once(
        r#"
        r = []
        class TZ2
          attr_reader :offset
          def initialize(offset) = @offset = offset
          def local_to_utc(t) = t - @offset
          def utc_to_local(t) = t + @offset
          def ==(o) = o.is_a?(TZ2) && o.offset == @offset
        end
        class TZName < TZ2
          attr_reader :name
          def initialize(name, offset) = (@name = name; super(offset))
        end
        class TWF2 < Time
          def self.find_timezone(name) = TZName.new(name.to_s, 19800)
        end
        z = TZ2.new(5*3600+30*60)
        t = Time.new(2012, 1, 1, 12, 0, 0, z)
        r << [t.to_s, t.utc_offset, t.zone.class.to_s]
        r << [(t + 1).zone.class.to_s, (t - 1).zone.class.to_s, t.round.zone.class.to_s]
        # An Integer result is its own wall clock.
        zi = Object.new
        def zi.utc_to_local(tt) = tt.to_i + 3600
        r << Time.now(in: zi).utc_offset
        # A Struct's #zone / #utc_offset are ignored: only its fields count.
        zs = Object.new
        def zs.utc_to_local(tt)
          Struct.new(:year, :mon, :mday, :hour, :min, :sec, :isdst, :to_i, :zone, :utc_offset)
                .new(tt.year, tt.mon, tt.mday, tt.hour, tt.min, tt.sec, tt.isdst, tt.to_i, 'America/New_York', -5*60*60)
        end
        r << Time.now(in: zs).utc_offset
        # Likewise a Time built at +09:00 whose fields are unchanged.
        zt = Object.new
        def zt.utc_to_local(tt) = Time.new(tt.year, tt.mon, tt.mday, tt.hour, tt.min, tt.sec, 9*60*60)
        r << Time.now(in: zt).utc_offset
        # More than a day apart is out of range.
        zbig = Object.new
        def zbig.utc_to_local(tt)
          u = Time.utc(tt.year, tt.mon, tt.day, tt.hour, tt.min, tt.sec) - 24*60*60
          Time.utc(u.year, u.mon, u.day, u.hour, u.min, u.sec)
        end
        begin; Time.now(in: zbig); r << :no_raise; rescue => e; r << [e.class, e.message]; end
        g = TWF2.utc(2000, 1, 1, 12, 0, 0).getlocal("Asia/Colombo")
        r << [g.zone.class.to_s, g.zone.name, g.utc_offset]
        zn = TZName.new("Asia/Colombo", 19800)
        m = TWF2.new(2000, 1, 1, 12, 0, 0, zn)
        l = Marshal.load(Marshal.dump(m))
        r << [l.class.to_s, l.zone.class.to_s, l.zone.name, l.utc_offset, l.to_s]
        # A plain Time has no `.find_timezone`, so the name stays a String.
        plain = Time.new(2000, 1, 1, 12, 0, 0, zn)
        lp = Marshal.load(Marshal.dump(plain))
        r << [lp.class.to_s, lp.zone, lp.utc_offset]
        # A zone that cannot name itself makes its times undumpable.
        begin
          Marshal.dump(Time.new(2000, 1, 1, 12, 0, 0, z))
          r << :no_raise
        rescue NoMethodError => e
          r << [e.class, e.message[/undefined method [`']name'/]]
        end
        r
        "#,
    );
}

#[test]
fn marshal_payload_holds_the_utc_clock() {
    let _tz = tz_lock();
    // The 8-byte `Time#_dump` payload carries the **UTC** clock whatever
    // zone the time is in; the `:offset` ivar beside it is what puts it
    // back. monoruby wrote the *local* clock, which round-tripped within
    // monoruby but not with CRuby — a dump of a `Asia/Tokyo` local time
    // read back nine hours out, in either direction. These are the exact
    // bytes CRuby 4.0.6 writes.
    run_test_once(
        r#"
        old = ENV['TZ']
        ENV['TZ'] = 'Asia/Tokyo'
        r = []
        t = Time.local(2000, 1, 1, 12, 0, 0)
        r << Marshal.dump(t).bytes
        r << Marshal.load(Marshal.dump(t)).then { |l| [l.to_s, l.utc_offset, l.zone, l.to_i] }
        f = Time.new(2000, 1, 1, 12, 0, 0, "+05:30")
        r << Marshal.dump(f).bytes
        r << Marshal.load(Marshal.dump(f)).then { |l| [l.to_s, l.utc_offset, l.zone, l.to_i] }
        u = Time.utc(2000, 1, 1, 12, 0, 0)
        r << Marshal.dump(u).bytes
        r << Marshal.load(Marshal.dump(u)).then { |l| [l.to_s, l.utc?, l.to_i] }
        # A `Time` subclass loads back as itself.
        class MarshalTimeSub < Time; end
        r << Marshal.load(Marshal.dump(MarshalTimeSub.utc(2000, 1, 1))).class.to_s
        ENV['TZ'] = old
        r
        "#,
    );
}

// `Time.local`'s C-style 10-argument form carries an `isdst` flag, and
// it is not advisory: in the hour a zone falls back, the same wall clock
// names two instants and `isdst` is the only thing that says which.
// monoruby dropped the argument and let libc pick (`tm_isdst = -1`), so
// `Time.local(0, 30, 1, 30, 10, 2005, 0, 0, true, tz)` answered EST
// where CRuby answers EDT.
//
// The flag only resolves that ambiguity. A wall clock in the hour a zone
// *springs forward* names no instant at all, and CRuby extrapolates
// forward there whatever the flag says — so a hint that moves the answer
// to a different wall clock is discarded.
#[test]
fn time_local_isdst_picks_the_side_of_a_fall_back() {
    let _tz = tz_lock();
    run_test_once(
        r#"
        old = ENV['TZ']
        r = []
        ENV['TZ'] = 'America/New_York'
        # 2005-10-30 01:30 happens twice: once EDT, once EST.
        [true, false, nil].each do |isdst|
          t = Time.local(0, 30, 1, 30, 10, 2005, 0, 0, isdst, ENV['TZ'])
          r << [isdst, t.utc_offset, t.to_i, t.to_s, t.dst?, t.zone]
        end
        # CRuby reads the flag for its truthiness alone, so 0 and a
        # String both mean daylight time.
        [0, "x", :dst].each do |isdst|
          r << Time.local(0, 30, 1, 30, 10, 2005, 0, 0, isdst, ENV['TZ']).utc_offset
        end
        # No argument at all: libc's own choice, the standard side here.
        r << Time.local(2005, 10, 30, 1, 30, 0).then { |t| [t.utc_offset, t.to_s] }
        # 2005-04-03 02:30 never happens. Every flag lands on the same
        # extrapolated 03:30 EDT.
        [true, false, nil].each do |isdst|
          t = Time.local(0, 30, 2, 3, 4, 2005, 0, 0, isdst, ENV['TZ'])
          r << [isdst, t.utc_offset, t.to_i, t.to_s]
        end
        # An unambiguous time ignores the flag either way.
        [true, false, nil].each do |isdst|
          r << Time.local(0, 0, 12, 15, 6, 2005, 0, 0, isdst, ENV['TZ']).to_i
          r << Time.local(0, 0, 12, 15, 1, 2005, 0, 0, isdst, ENV['TZ']).to_i
        end
        # The southern hemisphere falls back in April instead.
        ENV['TZ'] = 'Australia/Sydney'
        [true, false, nil].each do |isdst|
          t = Time.local(0, 30, 2, 2, 4, 2006, 0, 0, isdst, ENV['TZ'])
          r << [isdst, t.utc_offset, t.to_i, t.to_s]
        end
        # A zone with no daylight saving at all.
        ENV['TZ'] = 'Asia/Tokyo'
        [true, false, nil].each do |isdst|
          r << Time.local(0, 30, 1, 30, 10, 2005, 0, 0, isdst, ENV['TZ']).to_i
        end
        # `Time.utc`'s 10-argument form has no local zone to be
        # ambiguous in, so the flag changes nothing.
        r << [true, false, nil].map { |d| Time.utc(0, 30, 1, 30, 10, 2005, 0, 0, d, "x").to_i }.uniq
        ENV['TZ'] = old
        r
        "#,
    );
}
