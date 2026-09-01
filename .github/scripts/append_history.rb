# Appends one CSV row per benchmark to history.csv from a portal.json.
# Column order matters: consumers parse positionally (the published file
# carried a stale header for months), so new columns are only ever added
# at the END of the row. Current schema (13 columns):
#   timestamp,commit,benchmark,monoruby_ms,yjit_ms,ratio,
#   monoruby_failed,yjit_failed,monoruby_reason,yjit_reason,
#   zjit_ms,zjit_failed,zjit_reason
require 'json'

data = JSON.parse(File.read(ARGV[0]))
ts  = data['timestamp']
sha = data['commit']
q   = ->(s) { s.nil? ? '' : '"' + s.to_s.gsub('"', '""') + '"' }

data['benchmarks'].each do |b|
  mr   = b['monoruby_failed'] ? '' : '%.4f' % b['monoruby_ms']
  yj   = b['yjit_failed']     ? '' : '%.4f' % b['yjit_ms']
  zj   = b['zjit_failed'] == false ? '%.4f' % b['zjit_ms'] : ''
  ra   = b['ratio']           ? '%.4f' % b['ratio'] : ''
  mr_r = q.call(b['monoruby_reason'])
  yj_r = q.call(b['yjit_reason'])
  zj_r = q.call(b['zjit_reason'])
  $stdout.puts [ts, sha, b['name'], mr, yj, ra,
                b['monoruby_failed'], b['yjit_failed'], mr_r, yj_r,
                zj, b['zjit_failed'], zj_r].join(',')
end
