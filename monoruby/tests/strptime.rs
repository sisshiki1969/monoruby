extern crate monoruby;
use monoruby::tests::*;

// The strptime gem over monoruby's stand-in for strptime.so
// (gem/strptime/strptime.rb). Every case is compared against the host
// CRuby, which runs the gem's real C extension — so these pin the stand-in
// to the extension's directive set, field defaults and error texts.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and strptime is an ordinary gem. The local-time cases
// go through the live oracle, since their answers depend on the host's
// time zone.

/// `Strptime#exec` with an offset in the input (`%z` in its forms, `Z`
/// for UTC): every supported directive, up-to-width digit reads, month
/// names in either case and length, whitespace runs, literal text,
/// trailing input ignored, a negative year, sub-second `%N`, and `execi`.
#[test]
fn strptime_exec_fixed_offset() {
    run_test_once(
        r##"
        require "rubygems"
        require "strptime"
        res = []
        f = ->(fmt, s) { t = Strptime.new(fmt).exec(s); [t.to_i, t.nsec, t.utc_offset, t.utc?] }
        res << f.("%Y-%m-%dT%H:%M:%S%z", "2023-07-27T09:00:00+0530")
        res << f.("%Y-%m-%dT%H:%M:%S%z", "2023-07-27T09:00:00+05:30")
        res << f.("%Y-%m-%dT%H:%M:%S%z", "2023-07-27T09:00:00-01")
        res << f.("%Y-%m-%dT%H:%M:%S%z", "2023-07-27T09:00:00Z")
        res << f.("%Y-%m-%dT%H:%M:%S%z", "2023-07-27T09:00:00z")
        res << f.("%Y-%m-%d %H:%M:%S.%N %z", "2023-07-27 09:00:00.123456789 +0000")
        res << f.("%Y-%m-%d %H:%M:%S.%N %z", "2023-07-27 09:00:00.5 +0000")
        res << f.("%b %d %Y %z", "Jul 27 2023 +0000")
        res << f.("%B %e %Y %z", "july 27 2023 +0000")
        res << f.("%h %d %Y %z", "JUN 5 2023 +0000")
        res << f.("%y-%m-%d %z", "23-07-27 +0000")
        res << f.("%y-%m-%d %z", "69-01-01 +0000")
        res << f.("%y-%m-%d %z", "68-01-01 +0000")
        res << f.("%Y %z", "2023 +0000")
        res << f.("%Y %z", "-0001 +0000")
        res << f.("%Y %z", "+0033 +0000")
        res << f.("%Y-%m-%d %z", "2023-07-27x +0000".sub("x +0000", " +0000x"))
        res << f.("%Y-%m-%d%z", "2023-02-30+0000")
        res << f.("time:%Y %z", "time:2023 +0000")
        res << f.("%Y %m %z", "2023   07 +0000")
        res << f.("%Y%n%m%z", "2023\t\n07+0000")
        res << f.("%Y%m%d%H%M%S%z", "20230727090000+0900")
        res << f.("%Y-%m-%d %H:%M:%S %z", "2023-07-27 09:00:60 +0000")
        res << [Strptime.new("%Y-%m-%d %H:%M:%S %z").execi("2023-07-27 09:00:00 +0000"),
                Strptime.new("%Y-%m-%d %H:%M:%S %z").execi("2023-07-27 09:00:00 +0900"),
                Strptime.new("%Y-%m").source, Strptime.new("%Y-%m").source.frozen?]
        res << Strptime.new("%z").exec("+0530").utc_offset
        res
        "##,
    );
}

/// What `Strptime` refuses: directives the extension does not implement
/// (`%Z`, `%L`, `%s`, `%a`, `%A`, `%j`, `%c`, `%%`), input that does not
/// match (the wrong separator, too few fields, out-of-range values), and
/// non-String arguments.
#[test]
fn strptime_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "strptime"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        %w[%Z %L %s %a %A %j %c %% %F %T %p %I].each { |d| res << t.() { Strptime.new("%Y #{d}") } }
        res << t.() { Strptime.new("%") }
        res << t.() { Strptime.new("%Y-%m-%d").exec("2023/07/27") }
        res << t.() { Strptime.new("%Y-%m-%d").exec("2023-07") }
        res << t.() { Strptime.new("%Y-%m-%d").exec("2023-13-45") }
        res << t.() { Strptime.new("%Y-%m-%d").exec("2023-00-01") }
        res << t.() { Strptime.new("%H:%M:%S").exec("24:00:00") }
        res << t.() { Strptime.new("%H:%M:%S").exec("23:60:00") }
        res << t.() { Strptime.new("%H:%M:%S").exec("23:00:61") }
        res << t.() { Strptime.new("%b").exec("Foo") }
        res << t.() { Strptime.new("%Y").exec("abcd") }
        res << t.() { Strptime.new("%z").exec("xyz") }
        res << t.() { Strptime.new("%N").exec("x") }
        res << t.() { Strptime.new("%Y").exec(nil) }
        res << t.() { Strptime.new("%Y").exec(2023) }
        res << t.() { Strptime.new(1) }
        res << t.() { Strptime.new(nil) }
        res << t.() { Strptime.new("%Y\0") }
        res
        "##,
    );
}

/// Local time (no `%z`): the Time is in the host's zone, and unset fields
/// default per the extension — with a year, month and day to 1 and the
/// time to 00:00:00; without one, from the current time with the fields
/// after the first given one reset.
#[test]
fn strptime_local_live() {
    run_test_once_live(
        r##"
        require "rubygems"
        require "strptime"
        res = []
        x = Strptime.new("%Y-%m-%d %H:%M:%S").exec("2023-07-27 09:00:00")
        res << [x.class, x.to_a[0, 6], x.utc_offset == Time.local(2023, 7, 27, 9).utc_offset, x.utc?]
        res << Strptime.new("%Y").exec("2023").to_a[0, 6]
        res << Strptime.new("%Y-%m").exec("2023-07").to_a[0, 6]
        res << Strptime.new("%Y-%m-%d %H").exec("2023-07-27 09").to_a[0, 6]
        res << Strptime.new("%b %d %Y").exec("Jul 27 2023").to_a[0, 6]
        now = Time.now
        h = Strptime.new("%H:%M").exec("09:30")
        res << [h.hour, h.min, h.sec, [h.year, h.month, h.day] == [now.year, now.month, now.day] || (now.hour == 23 && now.min == 59)]
        m = Strptime.new("%m-%d").exec("03-04")
        res << [m.month, m.day, m.hour, m.min, m.sec, m.year == now.year || now.month == 12]
        res << [Strptime.new("%Y-%m-%d %H:%M:%S").execi("2023-07-27 09:00:00") == Time.local(2023, 7, 27, 9).to_i]
        res
        "##,
    );
}

/// `Strftime`: the supported directives (`%H %L %M %N %S %Y %d %b %m %y
/// %z`) in the Time's own offset, `execi` for an Integer / Float /
/// Rational epoch in UTC, `source`, the result's encoding, and the
/// refusals (other directives, a non-Time argument).
#[test]
fn strftime() {
    run_test_once(
        r##"
        require "rubygems"
        require "strptime"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        tm = Time.at(1700000000, 123456789, :nsec).utc
        res << Strftime.new("%Y-%m-%dT%H:%M:%S%z").exec(tm)
        res << Strftime.new("%H:%M:%S.%N %L %b %y|%d").exec(tm)
        res << Strftime.new("%Y-%m-%dT%H:%M:%S%z").exec(Time.at(1700000000, 0, :nsec, in: 32400))
        res << Strftime.new("%z").exec(Time.at(0, 0, :nsec, in: -3600))
        res << Strftime.new("%z").exec(Time.at(0, 0, :nsec, in: -36000))
        res << Strftime.new("%Y").exec(Time.utc(1900, 1, 1))
        res << Strftime.new("%Y").exec(Time.utc(1968, 1, 1))
        res << Strftime.new("%Y").exec(Time.utc(2069, 1, 1))
        res << Strftime.new("%Y-%m-%d %H:%M:%S %z").execi(1700000000)
        res << Strftime.new("%Y-%m-%d %H:%M:%S.%L %N").execi(1700000000.25)
        res << Strftime.new("%Y-%m-%d %H:%M:%S.%L %N").execi(1700000000.25r)
        res << [Strftime.new("literal %Y no").source, Strftime.new("x%Y").exec(tm).encoding.name, Strftime.new("x%Y".encode("US-ASCII")).exec(tm).encoding.name]
        res << [Strftime.new("").exec(tm), Strftime.new("plain text").exec(tm)]
        %w[%c %F %T %e %j %a %A %p %I %Z %s %3N %%].each { |d| res << t.() { Strftime.new("#{d}") } }
        res << t.() { Strftime.new("%Y").exec("x") }
        res << t.() { Strftime.new("%Y").exec(1) }
        res << t.() { Strftime.new("%Y").exec(nil) }
        res << t.() { Strftime.new(1) }
        res
        "##,
    );
}
