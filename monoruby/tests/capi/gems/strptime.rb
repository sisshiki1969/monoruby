# Run on monoruby (the gem installed through its C API layer, over the
# Time API) and on the host CRuby by tests/capi_gems.rs; both must print
# the same.
gem "strptime", ENV.fetch("CAPI_GEM_VERSION")
require "strptime"
puts $LOADED_FEATURES.grep(/\.(so|bundle)\z/).map { |f| File.basename(f, ".*") }.grep(/\Astrptime\z/).sort.inspect
def try
  yield
rescue => e
  [e.class, e.message]
end
pr = Strptime.new("%Y-%m-%dT%H:%M:%S%z")
t = pr.exec("2024-02-29T12:34:56+09:00")
p t.class, t.to_i, t.utc_offset, t.inspect, pr.execi("2024-02-29T12:34:56+09:00"), pr.source
p pr.exec("2024-02-29T12:34:56Z").inspect, pr.exec("1999-12-31T23:59:59-00:30").inspect
p Strptime.new("%Y%m%d").execi("20240101"), Strptime.new("%Y-%m").exec("2024-03").month, Strptime.new("%Y%m%d%H%M%S%z").execi("20240101000000+0000")
n = Strptime.new("%H:%M:%S.%N").exec("01:02:03.123456789")
p n.nsec, n.hour, n.min, n.sec, n.year
l = Strptime.new("%Y-%m-%d %H:%M:%S").exec("2024-06-15 10:20:30")
p l.utc_offset == Time.local(2024, 6, 15, 10, 20, 30).utc_offset, l.to_a[0, 6]
p Strptime.new("%b %d %Y").exec("Mar 05 2021").to_a[3, 3], Strptime.new("%y-%m-%d").exec("99-01-02").year, Strptime.new("%e/%m/%Y").exec("7/08/2020").day
p try { pr.exec("not a date") }, try { Strptime.new("%Q") }, try { Strptime.new(1) }, try { pr.exec(nil) }
p pr.exec("2000-01-01T00:00:00+00:00").to_i, pr.execi("2000-01-01T00:00:00+00:00")
ft = Strftime.new("%Y-%m-%dT%H:%M:%S%z")
p ft.exec(Time.utc(2024, 2, 29, 12, 34, 56)), ft.exec(Time.at(1700000000).localtime("+09:00")), ft.execi(0), ft.source
p Strftime.new("%N %L").exec(Time.at(1700000000, 123456789, :nanosecond).utc), Strftime.new("%b %d %H:%M").exec(Time.utc(2021, 3, 5, 1, 2))
p Strftime.new("%Y").exec(Time.utc(2030)), try { ft.exec("x") }, try { ft.exec(nil) }, try { Strftime.new("%Y").exec(Time.utc(2024).localtime(-3600)) }
p Strftime.new("%z").exec(Time.utc(2024)), Strftime.new("%y %b %m %d %H").exec(Time.utc(2024, 7, 4, 15))
