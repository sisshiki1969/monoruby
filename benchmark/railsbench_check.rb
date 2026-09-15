# frozen_string_literal: true

# Verifies that a railsbench run is measuring correct work.
#
# benchmark.rb only asserts that each response status is 200. A JIT that
# miscompiles a template or an ActiveRecord query can satisfy that while
# returning the wrong page -- and it will look *faster* for doing less. This
# script serves every route benchmark.rb visits, checks the bodies against the
# database rows they are rendered from, and then re-serves the hot routes far
# past the JIT compilation thresholds to confirm the response does not change
# once compiled code takes over.
#
# Usage, from anywhere -- either copy of this file works, see RAILSBENCH_DIR
# below:
#
#     ruby        benchmarks/railsbench/check.rb
#     ruby --yjit benchmarks/railsbench/check.rb
#     monoruby    benchmark/railsbench_check.rb
#
# Exits 0 when every check passes and 1 otherwise, so it can gate a benchmark
# run. It boots through the same harness path as benchmark.rb (bundle install,
# db:migrate, db:seed), so a failure here is a real problem with the benchmark
# or with the Ruby running it, not with this script's setup.
#
# REPEAT=n sets how many times each hot route is re-served in the last check
# (default 300). monoruby compiles a method at 20 calls and a loop at 100
# iterations, and YJIT well before that, so the default leaves margin on both.

# Resolve the railsbench app rather than assuming this file sits in it, so the
# same script runs from the ruby-bench checkout and from monoruby's benchmark/
# directory, where it is kept under version control. RAILSBENCH_DIR overrides.
RAILSBENCH_DIR = [
  ENV['RAILSBENCH_DIR'],
  __dir__,
  File.expand_path('../../ruby-bench/benchmarks/railsbench', __dir__),
].compact.find { |dir| File.exist?(File.join(dir, 'config/environment.rb')) }

unless RAILSBENCH_DIR
  abort <<~MSG
    Cannot find the railsbench app. It is expected either alongside this file or
    at ../../ruby-bench/benchmarks/railsbench relative to it (the layout
    bin/ruby-bench-diff assumes). Set RAILSBENCH_DIR to point at it:

        git clone https://github.com/ruby/ruby-bench.git
        RAILSBENCH_DIR=ruby-bench/benchmarks/railsbench ruby #{$PROGRAM_NAME}
  MSG
end

require File.expand_path('../../harness/loader', RAILSBENCH_DIR)

ENV['RAILS_ENV'] ||= 'production'
Dir.chdir RAILSBENCH_DIR
use_gemfile extra_setup_cmd: "bin/rails db:migrate db:seed"

require File.join(RAILSBENCH_DIR, 'config/environment')
require 'json'
require 'digest'

APP = Rails.application
REPEAT = Integer(ENV.fetch('REPEAT', 300))

# The routes benchmark.rb visits, in the order it builds them.
INDEX_ROUTES = ['/posts', '/posts.json'].freeze
SHOW_IDS = (1..100).freeze
ALL_ROUTES = (INDEX_ROUTES + SHOW_IDS.map { |i| "/posts/#{i}" }).freeze

# db/seeds.rb writes 100 posts and marks the i-th (0-based) published unless
# i % 10 == 0, so ids 1, 11, ... 91 are the unpublished ones.
SEEDED_POSTS = 100
UNPUBLISHED_IDS = SHOW_IDS.select { |id| (id - 1) % 10 == 0 }.freeze

# ---------------------------------------------------------------- harness

$checks = 0
$failures = []

# A check block returns nil to pass, or a string describing what went wrong.
def check(name)
  $checks += 1
  detail = yield
  if detail.nil?
    puts "  ok    #{name}"
  else
    $failures << name
    puts "  FAIL  #{name}"
    puts "          #{detail}"
  end
rescue StandardError, ScriptError => e
  $failures << name
  puts "  FAIL  #{name}"
  puts "          #{e.class}: #{e.message}"
  puts "          #{e.backtrace.first(4).join("\n          ")}" if e.backtrace
end

# Serve one request the way benchmark.rb does: a fresh env every time, because
# the app mutates it, and close the body because it may be a Rack::BodyProxy.
def get(path)
  env = Rack::MockRequest.env_for("https://localhost#{path}")
  status, headers, body = APP.call(env)
  chunks = []
  body.each { |chunk| chunks << chunk }
  body.close if body.respond_to?(:close)
  [status, headers['Content-Type'] || headers['content-type'], chunks.join]
end

# Two parts of a correct response legitimately differ between requests or
# between runs, and nothing else may:
#
#   - the layout's csrf-token meta tag holds a freshly masked token per request
#   - created_at / updated_at are stamped by whichever db:seed last ran
#
# Normalizing both leaves a body that must match byte for byte across requests
# in one process AND across implementations, which is what makes the printed
# fingerprint comparable between `ruby` and `monoruby`.
def normalize(body)
  body
    .gsub(/(name="csrf-token" content=")[^"]*"/, '\1CSRF_TOKEN"')
    .gsub(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z/, 'TIMESTAMP')
end

def digest(str)
  Digest::SHA256.hexdigest(str)[0, 16]
end

def escape(str)
  ERB::Util.html_escape(str).to_s
end

# ----------------------------------------------------------------- checks

puts "railsbench check -- #{RUBY_DESCRIPTION}"
puts "RAILS_ENV=#{Rails.env} REPEAT=#{REPEAT}"
puts

puts "seed data"

check "the database holds the #{SEEDED_POSTS} seeded posts, ids #{SHOW_IDS.first}..#{SHOW_IDS.last}" do
  count = Post.count
  ids = Post.order(:id).pluck(:id)
  if count != SEEDED_POSTS
    "Post.count is #{count}, expected #{SEEDED_POSTS}. Did db:seed run? See README.md."
  elsif ids != SHOW_IDS.to_a
    "ids are not #{SHOW_IDS.first}..#{SHOW_IDS.last}: got #{ids.first(5).inspect}..#{ids.last(3).inspect}"
  end
end

check "the published flags match db/seeds.rb (#{UNPUBLISHED_IDS.size} unpublished)" do
  unpublished = Post.where(published: false).order(:id).pluck(:id)
  unless unpublished == UNPUBLISHED_IDS.to_a
    "unpublished ids are #{unpublished.inspect}, expected #{UNPUBLISHED_IDS.to_a.inspect}"
  end
end

check "every seeded post has a non-empty title and body" do
  empty = Post.order(:id).pluck(:id, :title, :body)
              .select { |_id, title, body| title.to_s.empty? || body.to_s.empty? }
              .map(&:first)
  "posts with a blank title or body: #{empty.inspect}" unless empty.empty?
end

puts
puts "the workload benchmark.rb measures"

check "its seeded route sampling is reproducible" do
  sample = lambda do
    rng = Random.new(0x1be52551fc152997)
    Array.new(2000) { ALL_ROUTES.sample(random: rng) }
  end
  first = sample.call
  if first != sample.call
    "Array#sample with a seeded Random gave two different route lists; " \
      "timings from separate runs are not comparable"
  elsif first.uniq.sort != ALL_ROUTES.to_a.sort
    "the sampled workload does not cover all #{ALL_ROUTES.size} routes: " \
      "#{(ALL_ROUTES.to_a - first.uniq).inspect} never visited"
  else
    puts "          route-list digest #{digest(first.join(' '))} " \
         "(equal across implementations means the same workload was served)"
    nil
  end
end

check "all #{ALL_ROUTES.size} routes answer 200 with a non-empty body" do
  bad = ALL_ROUTES.filter_map do |path|
    status, _ctype, body = get(path)
    "#{path} -> #{status}, #{body.bytesize} bytes" unless status == 200 && !body.empty?
  end
  "#{bad.size} route(s) wrong: #{bad.first(5).join('; ')}" unless bad.empty?
end

check "the html and json routes answer with the content type they claim" do
  _s, html_ctype, = get('/posts')
  _s, json_ctype, = get('/posts.json')
  _s, show_ctype, = get('/posts/1')
  wrong = []
  wrong << "/posts -> #{html_ctype.inspect}" unless html_ctype.to_s.start_with?('text/html')
  wrong << "/posts.json -> #{json_ctype.inspect}" unless json_ctype.to_s.start_with?('application/json')
  wrong << "/posts/1 -> #{show_ctype.inspect}" unless show_ctype.to_s.start_with?('text/html')
  wrong.join('; ') unless wrong.empty?
end

puts
puts "rendered bodies against the rows they come from"

check "the html index renders all #{SEEDED_POSTS} posts" do
  _status, _ctype, body = get('/posts')
  missing = Post.order(:id).pluck(:id, :title).reject { |_id, title| body.include?(escape(title)) }
  if missing.empty?
    rows = body.scan(%r{<td><a href="/posts/\d+">Show</a></td>}).size
    "the index has #{rows} Show links, expected #{SEEDED_POSTS}" unless rows == SEEDED_POSTS
  else
    "#{missing.size} post title(s) absent from the index, e.g. #{missing.first(3).map(&:last).inspect}"
  end
end

check "the json index matches the database row for row" do
  _status, _ctype, body = get('/posts.json')
  parsed = JSON.parse(body)
  rows = Post.order(:id).pluck(:id, :title, :body, :published)
  if !parsed.is_a?(Array)
    "the json index parsed as #{parsed.class}, expected Array"
  elsif parsed.size != SEEDED_POSTS
    "the json index has #{parsed.size} entries, expected #{SEEDED_POSTS}"
  else
    wrong = parsed.zip(rows).filter_map do |entry, (id, title, text, published)|
      expected = { 'id' => id, 'title' => title, 'body' => text, 'published' => published }
      got = expected.keys.to_h { |k| [k, entry[k]] }
      next if got == expected

      differing = expected.keys.reject { |k| got[k] == expected[k] }
      "post #{id}: #{differing.map { |k| "#{k}=#{got[k].inspect} want #{expected[k].inspect}" }.join(', ')}"
    end
    missing_keys = %w[id title body published created_at updated_at url] - parsed.first.keys
    if !wrong.empty?
      "#{wrong.size} entr(ies) wrong: #{wrong.first(3).join('; ')}"
    elsif !missing_keys.empty?
      "the json entries are missing #{missing_keys.inspect}"
    end
  end
end

check "each json show page matches its own row" do
  wrong = Post.order(:id).pluck(:id, :title, :body, :published).filter_map do |id, title, text, published|
    _status, _ctype, body = get("/posts/#{id}.json")
    entry = JSON.parse(body)
    expected = { 'id' => id, 'title' => title, 'body' => text, 'published' => published }
    got = expected.keys.to_h { |k| [k, entry[k]] }
    "post #{id}: #{got.inspect} want #{expected.inspect}" unless got == expected
  end
  "#{wrong.size} show page(s) wrong: #{wrong.first(3).join('; ')}" unless wrong.empty?
end

check "each html show page renders its own post and not another" do
  posts = Post.order(:id).pluck(:id, :title, :body)
  wrong = posts.filter_map do |id, title, text|
    _status, _ctype, body = get("/posts/#{id}")
    problems = []
    problems << 'title absent' unless body.include?(escape(title))
    problems << 'body absent' unless body.include?(escape(text))
    # A show page must carry exactly one post: the neighbouring row's title
    # must not appear on it.
    other_id, other_title = posts.find { |oid, otitle| oid != id && otitle != title }
    if other_title && body.include?(escape(other_title))
      problems << "also renders post #{other_id}"
    end
    "post #{id}: #{problems.join(', ')}" unless problems.empty?
  end
  "#{wrong.size} show page(s) wrong: #{wrong.first(3).join('; ')}" unless wrong.empty?
end

check "a missing post is 404, so the 200 assertion is not vacuous" do
  missing_id = Post.maximum(:id).to_i + 100_000
  status, _ctype, = get("/posts/#{missing_id}")
  if status != 404
    "/posts/#{missing_id} -> #{status}, expected 404. ActiveRecord::RecordNotFound " \
      'is not reaching the exception handler, so every route "passing" means nothing.'
  end
end

puts
puts "stability once the JIT has compiled the request path"

# The routes worth hammering: the html index (the heaviest template loop), the
# json index (a jbuilder partial per row) and one show page (the shortest path).
%w[/posts /posts.json /posts/7].each do |path|
  check "#{path} returns the same body #{REPEAT} times over" do
    _status, _ctype, first = get(path)
    baseline = normalize(first)
    raw_diffs = 0
    drift = nil

    (REPEAT - 1).times do |i|
      status, _ctype, body = get(path)
      raw_diffs += 1 unless body == first
      if status != 200
        drift ||= "request #{i + 2} answered #{status}"
      elsif normalize(body) != baseline
        drift ||= "request #{i + 2} differs at byte #{
          baseline.bytes.zip(normalize(body).bytes).index { |a, b| a != b } || baseline.bytesize
        } of #{baseline.bytesize}"
      end
    end

    if drift
      "#{drift}; compiled code renders something other than what the interpreter did"
    else
      # HTML carries a per-request csrf token, so raw bodies differ by design
      # there and must not differ anywhere else. JSON has no token, so it must
      # be byte-identical even before normalizing.
      expected_raw_diffs = path.end_with?('.json') ? 0 : REPEAT - 1
      if raw_diffs != expected_raw_diffs
        "#{raw_diffs} of #{REPEAT - 1} raw bodies differed, expected #{expected_raw_diffs} " \
          '(the csrf-token meta tag is the only part allowed to vary)'
      else
        puts "          body digest #{digest(baseline)} (#{baseline.bytesize} bytes, normalized)"
        nil
      end
    end
  end
end

check 'the whole route set renders identically on a second pass' do
  pass = lambda { ALL_ROUTES.map { |path| normalize(get(path).last) }.join }
  first = pass.call
  second = pass.call
  if first != second
    "the #{ALL_ROUTES.size} routes rendered differently the second time through"
  else
    puts "          workload digest #{digest(first)} " \
         '(equal across implementations means identical output)'
    nil
  end
end

# ---------------------------------------------------------------- summary

# benchmark.rb removes this for the same reason: the production log grows on
# every request served.
File.unlink(File.join(RAILSBENCH_DIR, "log/#{Rails.env}.log")) rescue nil

puts
if $failures.empty?
  puts "#{$checks} checks passed -- railsbench is serving correct responses on this Ruby."
  exit 0
else
  puts "#{$failures.size} of #{$checks} checks FAILED:"
  $failures.each { |name| puts "  - #{name}" }
  puts
  puts 'A railsbench measurement from this Ruby is not meaningful until these pass.'
  exit 1
end
