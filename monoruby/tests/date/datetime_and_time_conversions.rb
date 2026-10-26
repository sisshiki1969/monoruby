require "date"
require "time"
dt = DateTime.new(2001, 2, 3, 4, 5, 6.5, "+09:00")
t = Time.utc(2001, 2, 3, 4, 5, 6)
r = [
  dt, dt.to_s, dt.iso8601(3), dt.rfc3339, dt.xmlschema(2), dt.jisx0301(1), dt.httpdate, dt.strftime("%H:%M:%S.%L %z %Z %s"),
  dt.hour, dt.minute, dt.second, dt.sec_fraction, dt.offset, dt.zone, dt.day_fraction,
  dt.new_offset("-05:00"), dt.new_offset(0).to_s, dt + Rational(1, 24), (dt - DateTime.new(2001, 2, 3)).class,
  dt.to_date, dt.to_time.utc, dt.to_time.class, t.to_date, t.to_datetime, t.to_datetime.offset, t.to_date.to_time.class,
  DateTime.parse("2001-02-03T04:05:06+07:00"), DateTime.iso8601("2001-02-03T04:05:06.789Z").sec_fraction,
  DateTime.strptime("2001-02-03 04:05:06 PM +0900", "%Y-%m-%d %I:%M:%S %p %z"),
  (DateTime.new(2001, 2, 3, 25) rescue [$!.class, $!.message]), DateTime.new(2001, 2, 3, -1).hour,
  DateTime.jd(2451944, 12), DateTime.ordinal(2001, 34, 1, 2, 3), DateTime.commercial(2001, 5, 6, 7),
  Time.parse("2001-02-03T04:05:06Z").utc, Time.iso8601("2001-02-03T04:05:06.123Z").usec, Time.rfc2822("Sat, 03 Feb 2001 04:05:06 -0000").utc,
  Time.httpdate("Sat, 03 Feb 2001 04:05:06 GMT").to_i, Time.strptime("2001-02-03 04:05:06 UTC", "%Y-%m-%d %H:%M:%S %Z").utc,
  t.iso8601(2), t.xmlschema, t.rfc2822, t.httpdate,
  Marshal.load(Marshal.dump(dt)) == dt, Marshal.load(Marshal.dump(Date.new(2001, 2, 3))),
  Date.new(2001, 2, 3).frozen?, dt.frozen?, Date.new(2001, 2, 3).dup, dt.clone.to_s,
]
r.each { |v| p v }
