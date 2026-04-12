# frozen_string_literal: true
# Stub for monoruby: replaces sqlite3's fork_safety.rb which requires
# weakref/delegate (uses `def !` syntax not supported by monoruby's parser).

module SQLite3
  module ForkSafety
    def self.hook!; end
    def self.track(db); end
    def self.discard; end
    def self.suppress_warnings!; end
  end
end
