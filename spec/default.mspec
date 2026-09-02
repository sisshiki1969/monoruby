# mspec configuration for a ruby/spec workspace laid out by this repo's
# setup action (`rubyspec: "true"`, see action.yml):
#
#   spec/ruby/          ruby/spec
#   spec/mspec/         ruby/mspec
#   spec/tags/          this repo's spec/tags
#   spec/default.mspec  this file
#
# mspec loads `default.mspec` from the current directory or `spec/` by
# itself -- in the `mspec` driver and in the mspec-ci/mspec-run child that
# runs the examples -- so nothing on the command line needs to reference
# this file. (An engine-specific name, `monoruby.mspec`, would not be
# picked up: monoruby reports RUBY_ENGINE "ruby".)
#
# Example trace: print each example's description to stderr right before
# it runs. mspec's own `--timeout` is a watchdog thread inside the target
# process and cannot fire while monoruby's main thread blocks in the
# kernel (e.g. in waitpid), so a hung run is only ever killed from the
# outside by timeout(1), which leaves an empty results file and, until
# now, no hint of where it was. With this trace the last description in
# the log is the example that was running -- the one to tag.
class ExampleTrace
  def before(state)
    STDERR.puts state.description
    STDERR.flush
  end
end

MSpec.register :before, ExampleTrace.new
