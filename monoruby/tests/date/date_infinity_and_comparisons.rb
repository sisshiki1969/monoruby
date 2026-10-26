require "date"
inf = Date::Infinity.new
ninf = -Date::Infinity.new
d = Date.new(2001, 2, 3)
r = [
  inf.infinite?, ninf.infinite?, inf.finite?, inf.nan?, inf.zero?, inf.abs.to_f, (+inf).to_f, ninf.to_f, inf.coerce(1),
  Date::JULIAN <=> inf, Date::GREGORIAN <=> ninf, inf <=> Date::JULIAN, inf <=> 1, 1 <=> inf, inf <=> inf, inf <=> ninf,
  inf > 10**100, ninf < -(10**100), (inf <=> "x"),
  d.infinite?, (d <=> 3), (d == 3), (d <=> inf), (d >> :a rescue $!.class), (d + "x" rescue $!.class),
  (d + Rational(1, 2)).to_s, (d + 1.5).to_s, d.ajd.class, d - Rational(1, 2),
  (Date.new(2001, 2, 3) <=> DateTime.new(2001, 2, 3, 12)), (Date.new(2001, 2, 3) == DateTime.new(2001, 2, 3, 0)),
  [Date.new(2001, 2, 3), Date.new(2000, 1, 1), DateTime.new(2001, 2, 3, 1)].sort.map(&:to_s),
  (Date.new(2001, 2, 3)..Date.new(2001, 2, 6)).map(&:day), (Date.new(2001, 2, 3)..Date.new(2001, 2, 6)).include?(Date.new(2001, 2, 4)),
  Date.new(2001, 2, 3).between?(Date.new(2001, 1, 1), Date.new(2001, 12, 31)),
]
r.each { |v| p v }
