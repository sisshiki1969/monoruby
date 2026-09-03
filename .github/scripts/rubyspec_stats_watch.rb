#!/usr/bin/env ruby
# frozen_string_literal: true
#
# rubyspec-stats watch: open (and later close) issues in this repo when the
# monoruby results published by a rubyspec-stats instance come back empty.
#
# Sources: the fork (sisshiki1969/rubyspec-stats) and upstream
# (eregon/rubyspec-stats). Both run ruby/spec against monoruby daily and
# commit monoruby/<category>.yml to master. A category file with no
# `examples:` count (`--- {}`) means the mspec run for that category never
# finished: monoruby hung and was killed by the outer timeout(1), or it
# crashed. The site then shows 0% for the category. A monoruby CI job that
# did not succeed at all (e.g. hit its job timeout) publishes nothing and is
# reported as well.
#
# One issue per (source, category-or-job), labelled `rubyspec-stats`, keyed
# by an HTML comment in the body. The same finding on newer results adds a
# comment; a source that recovers closes its issue.
#
# Env: GITHUB_TOKEN (needed for writes; reads work without it within the
# anonymous rate limit), GITHUB_REPOSITORY (issue repo), DRY_RUN=1 (print
# what would be done, write nothing).

require 'json'
require 'net/http'
require 'uri'

SOURCES = {
  'sisshiki1969/rubyspec-stats' => 'https://sisshiki1969.github.io/rubyspec-stats/',
  'eregon/rubyspec-stats' => 'https://eregon.me/rubyspec-stats/',
}.freeze
CATEGORIES = %w[language core library security command_line].freeze
ISSUE_REPO = ENV.fetch('GITHUB_REPOSITORY', 'sisshiki1969/monoruby')
TOKEN = ENV['GITHUB_TOKEN'].to_s
DRY_RUN = ENV['DRY_RUN'] == '1' || ENV['DRY_RUN'] == 'true'
LABEL = 'rubyspec-stats'
KEY = 'rubyspec-stats-watch'
API = 'https://api.github.com'

def http(method, url, body = nil, auth: true)
  uri = URI(url)
  klass = { get: Net::HTTP::Get, post: Net::HTTP::Post, patch: Net::HTTP::Patch }.fetch(method)
  req = klass.new(uri)
  req['Accept'] = 'application/vnd.github+json'
  req['X-GitHub-Api-Version'] = '2022-11-28'
  req['Authorization'] = "Bearer #{TOKEN}" if auth && !TOKEN.empty?
  if body
    req['Content-Type'] = 'application/json'
    req.body = JSON.generate(body)
  end
  Net::HTTP.start(uri.host, uri.port, use_ssl: true, open_timeout: 20, read_timeout: 60) { |h| h.request(req) }
end

# Reads on the rubyspec-stats repos are public data, but the workflow's
# GITHUB_TOKEN is scoped to this repo and some endpoints (Actions runs, job
# logs) refuse it for a foreign repo; retry those anonymously.
def api(method, path, body = nil)
  res = http(method, "#{API}#{path}", body)
  if method == :get && !path.start_with?("/repos/#{ISSUE_REPO}/") && !TOKEN.empty? &&
     (res.is_a?(Net::HTTPForbidden) || res.is_a?(Net::HTTPNotFound))
    res = http(method, "#{API}#{path}", body, auth: false)
  end
  raise "#{method.to_s.upcase} #{path}: HTTP #{res.code} #{res.body.to_s[0, 200]}" unless res.is_a?(Net::HTTPSuccess)

  JSON.parse(res.body)
end

def raw_file(repo, path)
  res = http(:get, "https://raw.githubusercontent.com/#{repo}/master/#{path}", auth: false)
  return nil if res.is_a?(Net::HTTPNotFound)
  raise "raw #{repo}/#{path}: HTTP #{res.code}" unless res.is_a?(Net::HTTPSuccess)

  res.body
end

# The job log endpoint answers with a redirect to a signed blob URL.
def job_log(repo, job_id)
  res = http(:get, "#{API}/repos/#{repo}/actions/jobs/#{job_id}/logs")
  res = http(:get, "#{API}/repos/#{repo}/actions/jobs/#{job_id}/logs", auth: false) unless res.is_a?(Net::HTTPRedirection)
  res = http(:get, res['location'], auth: false) if res.is_a?(Net::HTTPRedirection)
  return nil unless res.is_a?(Net::HTTPSuccess)

  res.body
end

# Last output line of the `$MSPEC spec/ruby/<category>` step in a job log,
# plus the step's exit annotation. With the example trace from
# spec/default.mspec, the last line is the description of the example that
# was running when the process was killed. Returns nil when the step is not
# in the log.
def step_tail(log, category)
  lines = log.dup.force_encoding(Encoding::UTF_8).scrub.lines.map { |l| l.sub(/\A\S+Z ?/, '').chomp }
  start = lines.index { |l| l.start_with?('##[group]Run ') && l.end_with?(" spec/ruby/#{category}") }
  return nil unless start

  block = lines[(start + 1)..]
  block = block[0, block.index { |l| l.start_with?('##[group]') } || block.size]
  endgroup = block.index { |l| l.start_with?('##[endgroup]') }
  block = block[(endgroup + 1)..] if endgroup
  exit_line = block.find { |l| l.start_with?('##[error]Process completed') }&.sub('##[error]', '')
  output = block.reject { |l| l.empty? || l.start_with?('##[') || l.start_with?('Warning: failed to read library path') }
  # The first two output lines are mspec's command echo and monoruby's banner;
  # anything after that which is not a bare `--marker` dot run is the example
  # trace (or the specs' own output).
  traced = output.drop(2).any? { |l| !l.match?(/\A\.+\z/) }
  last = output.last.to_s
  last = if !traced
           '(no example trace in this log, only file markers; count the dots to find the file)'
         elsif last.match?(/\A\.+\z/)
           '(only a file marker after the last example: the process died while loading the next spec file)'
         else
           last.sub(/\A\.+/, '')
         end
  { last: last, exit: exit_line }
end

def examples_count(yml)
  yml[/^examples:\s*(\d+)/, 1]&.to_i
end

# --- collect findings --------------------------------------------------------

def latest_ci_run(repo)
  runs = api(:get, "/repos/#{repo}/actions/workflows/ci.yml/runs?branch=master&status=completed&per_page=1")['workflow_runs']
  run = runs&.first or return nil
  jobs = api(:get, "/repos/#{repo}/actions/runs/#{run['id']}/jobs?per_page=50")['jobs']
  job = jobs.find { |j| j['name'].start_with?('specs (monoruby') }
  { id: run['id'], url: run['html_url'], number: run['run_number'], created: run['created_at'],
    job_id: job && job['id'], job_url: job && job['html_url'], job_conclusion: job && job['conclusion'] }
end

def results_commit(repo, category)
  c = api(:get, "/repos/#{repo}/commits?path=monoruby/#{category}.yml&sha=master&per_page=1").first or return nil
  { sha: c['sha'], url: c['html_url'], date: c.dig('commit', 'committer', 'date') }
end

def check_source(repo, site)
  findings = []
  run = begin
    latest_ci_run(repo)
  rescue StandardError => e
    warn "#{repo}: could not read CI runs: #{e.message}"
    nil
  end

  if run && run[:job_id] && run[:job_conclusion] != 'success'
    findings << {
      repo: repo, kind: 'job', id: "run-#{run[:id]}",
      title: "rubyspec-stats: monoruby job did not succeed on #{repo} (no results published)",
      body: <<~BODY,
        The monoruby job of #{repo}'s CI run [##{run[:number]}](#{run[:url]}) (#{run[:created]}) finished with conclusion **#{run[:job_conclusion]}** ([job](#{run[:job_url]})), so no new results were committed and #{site} still shows the previous ones.

        Usually this means more than one category hung and the per-category timeouts added up to the job's `timeout-minutes`. Check the last description line of each `... specs` step in the job log; with the example trace from `spec/default.mspec` that is the example that was running when it was killed.
      BODY
    }
  end

  log = nil
  CATEGORIES.each do |category|
    yml = raw_file(repo, "monoruby/#{category}.yml")
    count = yml && examples_count(yml)
    next if count && count > 0

    commit = begin
      results_commit(repo, category)
    rescue StandardError => e
      warn "#{repo}: could not read commits for #{category}: #{e.message}"
      nil
    end
    tail = nil
    if run && run[:job_id]
      log ||= begin
        job_log(repo, run[:job_id]) || :unavailable
      rescue StandardError => e
        warn "#{repo}: could not read the job log: #{e.message}"
        :unavailable
      end
      tail = step_tail(log, category) unless log == :unavailable
    end

    state = yml.nil? ? 'is missing' : 'has no examples'
    lines = []
    lines << "- results commit: [`#{commit[:sha][0, 7]}`](#{commit[:url]}) (#{commit[:date]})" if commit
    lines << "- CI run: [##{run[:number]}](#{run[:url]}) (#{run[:created]}), monoruby job: #{run[:job_conclusion]} ([job](#{run[:job_url]}))" if run
    if tail
      lines << "- last output of the `#{category} specs` step: `#{tail[:last]}`"
      lines << "- step result: `#{tail[:exit]}`" if tail[:exit]
    elsif run && run[:job_url]
      lines << "- the job log could not be read from here; open the job and look at the last description line of the `#{category} specs` step (the example trace from `spec/default.mspec`)."
    end

    findings << {
      repo: repo, kind: category, id: commit ? commit[:sha] : "run-#{run && run[:id]}",
      title: "rubyspec-stats: monoruby #{category} is empty (0%) on #{repo}",
      body: <<~BODY,
        `monoruby/#{category}.yml` on #{repo} #{state}, so #{site} shows 0% for **#{category}**. An empty results file means the mspec run for the category did not finish: monoruby hung and was killed by the outer `timeout -k 5 1200` (exit 124/137), or it crashed.

        #{lines.join("\n")}

        To keep the category out of the way until the cause is fixed, tag the example (`spec/tags/#{category}/FILE_tags.txt`, `critical(hangs):DESCRIPTION`), see doc/ruby_spec_skip_tags.md. This issue closes itself once the next run publishes non-empty results.
      BODY
    }
  end
  findings
end

# --- issues ------------------------------------------------------------------

def key_of(repo, kind) = "<!-- #{KEY}: #{repo} #{kind} -->"
def id_of(id) = "<!-- #{KEY}-id: #{id} -->"

def ensure_label
  api(:get, "/repos/#{ISSUE_REPO}/labels/#{LABEL}")
rescue StandardError
  write(:post, "/repos/#{ISSUE_REPO}/labels",
        { name: LABEL, color: 'd93f0b', description: 'monoruby results on a rubyspec-stats site came back empty' },
        "create label #{LABEL}")
end

def open_issues
  api(:get, "/repos/#{ISSUE_REPO}/issues?state=open&labels=#{LABEL}&per_page=100").reject { |i| i['pull_request'] }
end

def already_reported?(issue, id)
  return true if issue['body'].to_s.include?(id_of(id))

  api(:get, "/repos/#{ISSUE_REPO}/issues/#{issue['number']}/comments?per_page=100").any? { |c| c['body'].to_s.include?(id_of(id)) }
end

def write(method, path, body, what)
  puts "#{DRY_RUN ? '[dry-run] would ' : ''}#{what}"
  return if DRY_RUN

  api(method, path, body)
end

def main
  findings = []
  checked = []
  SOURCES.each do |repo, site|
    findings.concat(check_source(repo, site))
    checked << repo
  rescue StandardError => e
    warn "#{repo}: skipped: #{e.message}"
  end

  ensure_label if findings.any?
  issues = open_issues
  summary = []

  findings.each do |f|
    key = key_of(f[:repo], f[:kind])
    body = "#{key}\n#{id_of(f[:id])}\n#{f[:body]}"
    if (issue = issues.find { |i| i['body'].to_s.include?(key) })
      if already_reported?(issue, f[:id])
        summary << "#{f[:repo]} #{f[:kind]}: still empty, already reported in ##{issue['number']}"
      else
        write(:post, "/repos/#{ISSUE_REPO}/issues/#{issue['number']}/comments", { body: "Still empty on newer results.\n\n#{body}" },
              "comment on ##{issue['number']} (#{f[:repo]} #{f[:kind]})")
        summary << "#{f[:repo]} #{f[:kind]}: still empty, commented on ##{issue['number']}"
      end
    else
      created = write(:post, "/repos/#{ISSUE_REPO}/issues", { title: f[:title], body: body, labels: [LABEL] },
                      "open issue: #{f[:title]}")
      summary << "#{f[:repo]} #{f[:kind]}: opened #{created ? "##{created['number']}" : '(dry run)'}"
    end
  end

  issues.each do |issue|
    m = issue['body'].to_s.match(/<!-- #{KEY}: (\S+) (\S+) -->/) or next
    repo, kind = m[1], m[2]
    next unless checked.include?(repo)
    next if findings.any? { |f| f[:repo] == repo && f[:kind] == kind }

    write(:post, "/repos/#{ISSUE_REPO}/issues/#{issue['number']}/comments",
          { body: "Recovered: #{repo} published non-empty #{kind} results again." }, "comment recovery on ##{issue['number']}")
    write(:patch, "/repos/#{ISSUE_REPO}/issues/#{issue['number']}", { state: 'closed', state_reason: 'completed' },
          "close ##{issue['number']} (#{repo} #{kind} recovered)")
    summary << "#{repo} #{kind}: recovered, closed ##{issue['number']}"
  end

  summary << 'no findings' if summary.empty?
  puts summary
  File.open(ENV['GITHUB_STEP_SUMMARY'], 'a') { |f| f.puts(summary.map { |s| "- #{s}" }) } if ENV['GITHUB_STEP_SUMMARY']
end

main if __FILE__ == $PROGRAM_NAME
