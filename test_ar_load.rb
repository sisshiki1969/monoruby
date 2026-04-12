#!/usr/bin/env monoruby
# Test ActiveRecord loading by bypassing bundler entirely.

GEM_BASE = "/opt/rbenv/versions/3.3.6/lib/ruby/gems/3.3.0/gems"

# Monkey-patches for monoruby feature gaps encountered while loading ActiveRecord.

# BUG: Regexp#match? does not implicitly convert Symbol to String.
class Regexp
  alias_method :__monoruby_match_q, :match?
  def match?(obj, pos = 0)
    obj = obj.to_s if obj.is_a?(Symbol)
    __monoruby_match_q(obj, pos)
  end
end

# BUG: File#path (instance method) is missing. Logger 1.7.0 feature-detects it
# expecting IOError; monoruby raises NoMethodError, which escapes the rescue.
class File
  def path
    @__monoruby_path || ""
  end
end

# BUG: Module#singleton_class? is missing.
class Module
  def singleton_class?
    false
  end
end

# BUG: String#tr! is missing (non-destructive String#tr exists).
class String
  def tr!(from, to)
    r = tr(from, to)
    return nil if r == self
    replace(r)
  end
end

# BUG: ObjectSpace::WeakMap is missing. The stdlib weakref.rb uses it, so
# define a non-GC-aware stand-in backed by a plain Hash. This keeps strong
# references, which is semantically weaker than a real weakref but
# sufficient for descendants_tracker and connection_pool::reaper to
# function.
module ObjectSpace
  class WeakMap
    include Enumerable

    def initialize
      @store = {}
    end

    def []=(key, value)
      @store[key] = value
    end

    def [](key)
      @store[key]
    end

    def key?(key)
      @store.key?(key)
    end

    def include?(key)
      @store.key?(key)
    end

    def delete(key)
      @store.delete(key)
    end

    def keys
      @store.keys
    end

    def values
      @store.values
    end

    def each_pair(&block)
      @store.each_pair(&block)
      self
    end
    alias each each_pair

    def size
      @store.size
    end
    alias length size
  end
end unless defined?(ObjectSpace::WeakMap)

# BUG: ObjectSpace.define_finalizer / undefine_finalizer / _id2ref are missing.
# monoruby has no GC finalization hooks, so make them no-ops.
module ObjectSpace
  def self.define_finalizer(obj, aproc = nil, &block); obj; end
  def self.undefine_finalizer(obj); obj; end
end

[
  ["activerecord-8.1.1", "lib"],
  ["activemodel-8.1.1", "lib"],
  ["activesupport-8.1.1", "lib"],
  ["i18n-1.14.7", "lib"],
  ["tzinfo-2.0.6", "lib"],
  ["concurrent-ruby-1.3.5", "lib/concurrent-ruby"],
  ["minitest-5.26.2", "lib"],
  ["bigdecimal-3.3.1", "lib"],
  ["mutex_m-0.3.0", "lib"],
  ["base64-0.3.0", "lib"],
  ["benchmark-0.3.0", "lib"],
  ["connection_pool-3.0.2", "lib"],
  ["drb-2.2.3", "lib"],
  ["logger-1.7.0", "lib"],
  ["securerandom-0.4.1", "lib"],
  ["timeout-0.5.0", "lib"],
  ["uri-1.1.1", "lib"],
  ["sqlite3-2.7.3-x86_64-linux-gnu", "lib"],
].each do |gem, sub|
  path = "#{GEM_BASE}/#{gem}/#{sub}"
  $LOAD_PATH.unshift(path) if Dir.exist?(path)
end

# Load core_ext/module/delegation so we can patch it
require "active_support/core_ext/module/delegation"

# BUG WORKAROUND: trailing comma drops kwargs
class Module
  remove_method :delegate rescue nil
  def delegate(*methods, to: nil, prefix: nil, allow_nil: nil, private: nil)
    ::ActiveSupport::Delegation.generate(self, methods, location: caller_locations(1, 1).first, to: to, prefix: prefix, allow_nil: allow_nil, private: private)
  end
end

puts "Loading active_record..."
require "active_record"
puts "OK: active_record loaded"

ActiveRecord::Base.establish_connection adapter: "sqlite3", database: ":memory:"
puts "OK: connection established"

ActiveRecord::Schema.define do
  create_table :posts, force: true do |t|
    t.string :title, null: false
    t.string :body
    t.integer :upvotes, null: false
    t.timestamps
  end
end
puts "OK: schema defined"

class Post < ActiveRecord::Base
end

post = Post.create!(title: "hello", body: "world", upvotes: 10)
puts "OK: created post id=#{post.id}"

fetched = Post.find(post.id)
puts "OK: fetched post title=#{fetched.title}"
