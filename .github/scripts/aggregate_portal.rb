# Builds bench-results/portal.json from yjit-bench's raw.json and
# raw.failures.json. Shared by the amd64 and arm64 bench jobs (the two
# per-job heredocs it replaces had already drifted once). Prints the
# JSON document to stdout.
#
# Environment:
#   GITHUB_WORKSPACE   checkout root; bench-results/ lives under it
#   TIMESTAMP          snapshot timestamp (ISO 8601)
#   SHORT_SHA          monoruby commit the run measured
#   MR_TIMEOUT, YJ_TIMEOUT, ZJ_TIMEOUT
#                      per-engine timeout(1) budget in seconds
#   RUBY_HEAD_SHA      short sha of the ruby/ruby HEAD the zjit engine
#                      was built from ('' when unavailable)
#   ZJIT_BUILD         'ok' when the ruby HEAD build succeeded, anything
#                      else marks zjit results as missing because of the
#                      build rather than the benchmarks
require 'json'

out = ENV['GITHUB_WORKSPACE'] + '/bench-results'
raw = JSON.parse(File.read(out + '/data/raw.json'))
fails_path = out + '/data/raw.failures.json'
fails = File.exist?(fails_path) ? JSON.parse(File.read(fails_path)) : {}

ENGINES = %w[monoruby yjit zjit].freeze
data  = ENGINES.to_h { |e| [e, raw.dig('raw_data', e) || {}] }
efail = ENGINES.to_h { |e| [e, fails[e] || {}] }
timeouts = {
  'monoruby' => ENV.fetch('MR_TIMEOUT', '400').to_i,
  'yjit'     => ENV.fetch('YJ_TIMEOUT', '400').to_i,
  'zjit'     => ENV.fetch('ZJ_TIMEOUT', '400').to_i,
}

reason = ->(status, timeout_s) {
  return nil if status.nil?
  case status
  when 0   then 'ok'
  when 1   then 'exit 1'
  when 124 then "timeout (#{timeout_s}s)"
  when 137 then "SIGKILL after timeout (#{timeout_s}s)"
  when 132 then 'SIGILL'
  when 134 then 'SIGABRT'
  when 136 then 'SIGFPE'
  when 138 then 'SIGBUS'
  when 139 then 'SIGSEGV'
  when 143 then 'SIGTERM'
  else
    if status > 128
      "signal #{status - 128} (exit #{status})"
    else
      "exit #{status}"
    end
  end
}

median = ->(arr) {
  return nil if arr.nil? || arr.empty?
  s = arr.sort
  n = s.size
  n.odd? ? s[n / 2] : (s[n / 2 - 1] + s[n / 2]) / 2.0
}

ms_for = ->(engine, name) {
  d = data[engine][name]
  med = d ? median.call(d['bench']) : nil
  (med && med > 0) ? med * 1000.0 : nil
}

zjit_built = ENV['ZJIT_BUILD'] == 'ok'
# When the ruby HEAD build failed there are no zjit rows at all; name
# the build as the cause instead of the generic 'no result'.
zjit_fallback = zjit_built ? 'no result' : 'ruby HEAD build failed'

all_names = (data.values.flat_map(&:keys) + efail.values.flat_map(&:keys)).uniq.sort

benches = all_names.map do |name|
  mr_ms = ms_for.call('monoruby', name)
  yj_ms = ms_for.call('yjit', name)
  zj_ms = ms_for.call('zjit', name)
  {
    name: name,
    monoruby_ms: mr_ms,
    yjit_ms: yj_ms,
    zjit_ms: zj_ms,
    # YJIT (4.0.2) is the baseline: both ratios are "speed as a multiple
    # of YJIT's" — yjit time over the engine's own time, higher = faster
    # than YJIT, 1.0 = YJIT parity.
    ratio:      (mr_ms && yj_ms) ? yj_ms / mr_ms : nil,
    zjit_ratio: (zj_ms && yj_ms) ? yj_ms / zj_ms : nil,
    monoruby_failed: mr_ms.nil?,
    yjit_failed: yj_ms.nil?,
    zjit_failed: zj_ms.nil?,
    monoruby_reason: mr_ms.nil? ? (reason.call(efail['monoruby'][name], timeouts['monoruby']) || 'no result') : nil,
    yjit_reason:     yj_ms.nil? ? (reason.call(efail['yjit'][name], timeouts['yjit']) || 'no result') : nil,
    zjit_reason:     zj_ms.nil? ? (reason.call(efail['zjit'][name], timeouts['zjit']) || zjit_fallback) : nil,
  }
end

head_sha = ENV['RUBY_HEAD_SHA'].to_s
puts JSON.generate(
  timestamp: ENV['TIMESTAMP'],
  commit: ENV['SHORT_SHA'],
  ruby_head: (zjit_built && !head_sha.empty?) ? head_sha : nil,
  timeouts: timeouts,
  benchmarks: benches,
)
