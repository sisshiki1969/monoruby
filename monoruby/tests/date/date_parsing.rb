require "date"
r = [
  Date.parse("2010-10-31"), Date.parse("Oct 31 2010"), Date.parse("31st October 2010"), Date.parse("H22.10.31"),
  Date.iso8601("2010-10-31"), Date.rfc3339("2010-10-31T00:00:00+09:00"), Date.xmlschema("2010-10-31"),
  Date.rfc2822("Sun, 31 Oct 2010 00:00:00 +0900"), Date.httpdate("Sun, 31 Oct 2010 00:00:00 GMT"), Date.jisx0301("H22.10.31"),
  Date.strptime("2010-10-31", "%Y-%m-%d"), Date.strptime("31/10/10", "%d/%m/%y"), Date._strptime("2010-10-31 x", "%Y-%m-%d").to_a,
  Date._parse("Sat Aug 28 21:00:00 GMT+9 2010").to_a, Date._parse("2013-08-14T15:00:00.123456+09:00").to_a,
  Date._iso8601("2013-226").to_a, Date._rfc2822("Wed, 14 Aug 2013 15:00:00 -0500").to_a,
  (Date.parse("garbage") rescue [$!.class, $!.message]),
  (Date.strptime("2010-13-01", "%Y-%m-%d") rescue [$!.class, $!.message]),
  (Date.parse("x" * 200) rescue [$!.class, $!.message]),
  (Date.parse("2010-10-31", true, Date::GREGORIAN, limit: 5) rescue [$!.class, $!.message]),
  Date.valid_date?(2010, 2, 30), Date.valid_jd?(nil), Date.valid_ordinal?(2010, 366), Date.valid_commercial?(2010, 53, 1),
]
r.each { |v| p v }
