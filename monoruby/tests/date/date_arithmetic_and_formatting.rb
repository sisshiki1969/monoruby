require "date"
d = Date.new(2001, 2, 3)
r = [
  d, d + 30, d - 1, d >> 1, d << 13, d.next_month(2), d.prev_year,
  (Date.new(2001, 3, 1) - d), (Date.new(2001, 3, 1) - d).class,
  d.yday, d.wday, d.cwday, d.cweek, d.jd, d.ajd, d.mjd, d.ld,
  d.leap?, Date.leap?(2000), Date.valid_date?(2001, 2, 29),
  d.strftime("%A %-d %B %Y %j %U %W %u"), d.iso8601, d.rfc3339, d.httpdate, d.jisx0301,
  d.to_s, d.inspect, d.succ, d.mday,
  d.upto(Date.new(2001, 2, 6)).map(&:day), d.step(Date.new(2001, 2, 10), 3).to_a.size,
  d.downto(Date.new(2001, 1, 30)).to_a.size, (d === Date.new(2001, 2, 3)),
  d <=> Date.new(2001, 2, 4), d == DateTime.new(2001, 2, 3), d.eql?(Date.new(2001, 2, 3)),
  d.hash == Date.new(2001, 2, 3).hash, [d, Date.new(2000, 1, 1)].min,
  Date.new(2001, 2, -1), Date.new(2001, -1, -1), Date.ordinal(2001, 34), Date.commercial(2001, 5, 6),
  Date.jd(2451944), Date.new(1582, 10, 4).succ, Date.new(1582, 10, 4, Date::GREGORIAN).succ,
  Date.new(2001, 2, 3).julian?, Date.new(1000, 1, 1).julian?, Date.new(1000, 1, 1).gregorian,
  Date::ITALY, Date::ENGLAND, Date::JULIAN, Date::GREGORIAN, Date::MONTHNAMES[2], Date::ABBR_DAYNAMES,
]
r.each { |v| p v }
