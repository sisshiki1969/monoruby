require 'json'

data = JSON.parse(File.read(ARGV[0]))
ts  = data['timestamp']
sha = data['commit']
q   = ->(s) { s.nil? ? '' : '"' + s.to_s.gsub('"', '""') + '"' }

data['benchmarks'].each do |b|
  mr   = b['monoruby_failed'] ? '' : '%.4f' % b['monoruby_ms']
  yj   = b['yjit_failed']     ? '' : '%.4f' % b['yjit_ms']
  ra   = b['ratio']           ? '%.4f' % b['ratio'] : ''
  mr_r = q.call(b['monoruby_reason'])
  yj_r = q.call(b['yjit_reason'])
  $stdout.puts [ts, sha, b['name'], mr, yj, ra,
                b['monoruby_failed'], b['yjit_failed'], mr_r, yj_r].join(',')
end
