# Emits the markdown table rows for the GITHUB_STEP_SUMMARY benchmark
# table, from bench-results/portal.json. The header row lives in the
# workflow (it carries the per-arch title).
require 'json'

data = JSON.parse(File.read(ENV['GITHUB_WORKSPACE'] + '/bench-results/portal.json'))

fmt_ms = ->(failed, v) { failed ? '**ERROR**' : '%.3f' % v }
fmt_ratio = ->(v) { v ? '%.3fx' % v : '-' }

data['benchmarks'].each do |b|
  mr = fmt_ms.call(b['monoruby_failed'], b['monoruby_ms'])
  yj = fmt_ms.call(b['yjit_failed'], b['yjit_ms'])
  zj = fmt_ms.call(b['zjit_failed'], b['zjit_ms'])
  reasons = %w[monoruby yjit zjit].filter_map { |e|
    r = b["#{e}_reason"]
    "#{e}: #{r}" if r
  }.join(' / ')
  puts "| #{b['name']} | #{mr} | #{yj} | #{zj} " \
       "| #{fmt_ratio.call(b['ratio'])} | #{fmt_ratio.call(b['zjit_ratio'])} " \
       "| #{reasons.empty? ? '-' : reasons} |"
end
