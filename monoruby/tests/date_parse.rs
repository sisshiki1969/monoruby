extern crate monoruby;
use monoruby::tests::*;

// `Date._parse` (stdlib/date_core.rb) is a transcription of CRuby's
// date_parse.c sub-parsers; `DateTime.parse` / `Date.parse` / `Time.parse`
// are built on it. Inputs that depend on today's date are avoided.

#[test]
fn date_underscore_parse_matches_cruby() {
    run_test_once(
        r#"
        require "date"
        [
          "8 May 2005 19:09:13 +0000", "Sun, 8 May 2005 19:09:13 +0000",
          "Wed, 14 Aug 2013 15:00:00 -0500", "2013-08-14", "2013-08-14T15:00:00Z",
          "2013-08-14T15:00:00.123456+09:00", "2013-08-14 15:00:00 UTC", "20130814",
          "20130814T150000Z", "2013-226", "2013-08-14 15:00", "15:00", "3:04pm",
          "3:04:05 PM EST", "Aug 14 2013", "Aug 14, 2013", "August 14th, 2013 3pm",
          "14 Aug 2013", "14th August 2013", "Wed Aug 14 15:00:00 +0000 2013",
          "Wed Aug 14 15:00:00 2013", "08/14/2013", "8/14/13", "2013/08/14", "14.08.2013",
          "2013.08.14", "14-Aug-2013", "Aug-14-2013", "H25.08.14", "--08-14", "May 2005",
          "Tuesday", "Sat Aug 28 21:00:00 GMT+9 2010", "12/25", "1999-12-31 23:59:59.5 +0530",
          "Thu, 01 Jan 1970 00:00:00 GMT", "2005-05-08T19:09:13-07:00", "8 May 05 19:09:13 +0000",
          "8 May 69 19:09:13 +0000", "19:09:13.250Z", "1:02:03 am JST", "2013-08-14T15:00:00+0900",
          "Wednesday, August 14, 2013", "'13-08-14", "2013-W33-3", "12:00 noon", "T15:00:00",
          "", "garbage", "13:00:00 A.M.", "5pm", "2013-08-14T15",
        ].map { |s| [s, Date._parse(s)] } +
        [["8 May 05", Date._parse("8 May 05", false)], ["8/14/13", Date._parse("8/14/13", false)]]
        "#,
    );
}

#[test]
fn datetime_parse_and_time_parse() {
    run_test_once(
        r#"
        require "date"
        require "time"
        r = []
        ["8 May 2005 19:09:13 +0000", "2013-08-14 15:00:00", "Aug 14 2013 3pm PST",
         "2013-08-14T15:00:00.5+09:00", "Wed, 14 Aug 2013 15:00:00 -0500"].each do |s|
          d = DateTime.parse(s)
          r << [s, d.to_s, d.year, d.month, d.day, d.hour, d.min, d.sec, d.offset, d.zone,
                d.sec_fraction, d.to_time.utc.to_s, d.iso8601(3)]
          t = Time.parse(s)
          r << [t.utc.to_s, t.utc_offset, t.usec]
        end
        r << Date.parse("14 Aug 2013").to_s << Date.parse("2013-226").to_s << Date.parse("2013-W33-3").to_s
        r << (begin; Date.parse("garbage"); rescue ArgumentError => e; e.message; end)
        r << (begin; Date._parse("x" * 200); rescue ArgumentError => e; e.message; end)
        r
        "#,
    );
}
