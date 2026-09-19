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
