extern crate monoruby;
use monoruby::tests::*;

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

#[test]
fn local_times_follow_a_runtime_tz_change() {
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
