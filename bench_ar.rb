#!/usr/bin/env monoruby
# ActiveRecord benchmark adapted from ruby-bench/benchmarks/activerecord/benchmark.rb
# Runs under monoruby with the same monkey-patches as test_ar_load.rb.

GEM_BASE = "/opt/rbenv/versions/3.3.6/lib/ruby/gems/3.3.0/gems"

# --- Monkey-patches for monoruby feature gaps ---

# BUG: Kernel#catch / Kernel#throw are not implemented.
# Provide a minimal implementation using exceptions.
class UncaughtThrowError < StandardError
  attr_reader :tag, :value
  def initialize(tag, value = nil)
    @tag = tag
    @value = value
    super("uncaught throw #{tag.inspect}")
  end
end

module Kernel
  def catch(tag = Object.new)
    yield tag
  rescue UncaughtThrowError => e
    raise unless e.tag == tag
    e.value
  end

  def throw(tag, value = nil)
    raise UncaughtThrowError.new(tag, value)
  end
end

class Regexp
  alias_method :__monoruby_match_q, :match?
  def match?(obj, pos = 0)
    obj = obj.to_s unless obj.is_a?(String)
    __monoruby_match_q(obj, pos)
  end
end

class File
  def path
    @__monoruby_path || ""
  end
end

class Module
  def singleton_class?
    false
  end
end

class String
  def tr!(from, to)
    r = tr(from, to)
    return nil if r == self
    replace(r)
  end
end

module ObjectSpace
  class WeakMap
    include Enumerable
    def initialize; @store = {}; end
    def []=(key, value); @store[key] = value; end
    def [](key); @store[key]; end
    def key?(key); @store.key?(key); end
    def include?(key); @store.key?(key); end
    def delete(key); @store.delete(key); end
    def keys; @store.keys; end
    def values; @store.values; end
    def each_pair(&block); @store.each_pair(&block); self; end
    alias each each_pair
    def size; @store.size; end
    alias length size
  end
end unless defined?(ObjectSpace::WeakMap)

module ObjectSpace
  def self.define_finalizer(obj, aproc = nil, &block); obj; end
  def self.undefine_finalizer(obj); obj; end
end

[
  ["activerecord-8.1.1", "lib"],
  ["activemodel-8.1.1", "lib"],
  ["activesupport-8.1.1", "lib"],
  ["i18n-1.14.8", "lib"],
  ["tzinfo-2.0.6", "lib"],
  ["concurrent-ruby-1.3.6", "lib/concurrent-ruby"],
  ["minitest-5.26.2", "lib"],
  ["bigdecimal-3.3.1", "lib"],
  ["mutex_m-0.3.0", "lib"],
  ["base64-0.3.0", "lib"],
  ["benchmark-0.3.0", "lib"],
  ["connection_pool-3.0.2", "lib"],
  ["drb-2.2.0", "lib"],
  ["logger-1.7.0", "lib"],
  ["securerandom-0.4.1", "lib"],
  ["timeout-0.5.0", "lib"],
  ["uri-1.1.1", "lib"],
  ["sqlite3-2.7.3-x86_64-linux-gnu", "lib"],
].each do |gem, sub|
  path = "#{GEM_BASE}/#{gem}/#{sub}"
  $LOAD_PATH.unshift(path) if Dir.exist?(path)
end

require "active_support/core_ext/module/delegation"
class Module
  remove_method :delegate rescue nil
  def delegate(*methods, to: nil, prefix: nil, allow_nil: nil, private: nil)
    ::ActiveSupport::Delegation.generate(self, methods, location: caller_locations(1, 1).first, to: to, prefix: prefix, allow_nil: allow_nil, private: private)
  end
end

puts "Loading active_record..."
require "active_record"
puts "OK: active_record loaded"

ActiveRecord::ConnectionAdapters::Deduplicable.module_eval do
  def deduplicate
    return self if frozen?
    self.class.registry[self] ||= deduplicated
  end
  alias :-@ :deduplicate
end

ActiveRecord::Type::TypeMap.class_eval do
  def lookup(lookup_key)
    fetch(lookup_key) { ActiveRecord::Type.default_value }
  end
end

ActiveRecord::Migration.class_eval do
  def say_with_time(message)
    say(message)
    result = yield
    say "%.4fs" % 0.0, :subitem
    say("#{result} rows", :subitem) if result.is_a?(Integer)
    result
  end
  def method_missing(method, *arguments, **kwargs, &block)
    say_with_time "#{method}(#{arguments.map(&:inspect).join(", ")})" do
      return super unless execution_strategy.respond_to?(method)
      execution_strategy.send(method, *arguments, **kwargs, &block)
    end
  end
end
ActiveRecord::Migration::DefaultStrategy.class_eval do
  private
  def method_missing(method, *args, **kwargs, &block)
    connection.send(method, *args, **kwargs, &block)
  end
end

ActiveRecord::Relation.class_eval do
  def where_clause
    @values.fetch(:where, ActiveRecord::Relation::WhereClause.empty)
  end
  def having_clause
    @values.fetch(:having, ActiveRecord::Relation::WhereClause.empty)
  end
  def from_clause
    @values.fetch(:from, ActiveRecord::Relation::FromClause.empty)
  end
end

# --- Setup database (matches ruby-bench activerecord/benchmark.rb) ---

ActiveRecord::Base.establish_connection adapter: "sqlite3", database: ":memory:"

ActiveRecord::Schema.define do
  create_table :posts, force: true do |t|
    t.string :title, null: false
    t.string :body
    t.string :type_name, null: false
    t.string :key, null: false
    t.integer :upvotes, null: false
    t.integer :author_id, null: false
    t.timestamps
  end

  create_table :comments, force: true do |t|
    t.integer :post_id
    t.integer :author_id
    t.text :body
    t.string :tags
    t.datetime :published_at
    t.timestamps
  end
end

class Post < ActiveRecord::Base
  has_many :comments, inverse_of: :post
end

class Comment < ActiveRecord::Base
  belongs_to :post, inverse_of: :comments
end

puts "Seeding data..."
srand(1337)

Post.transaction do
  100.times do
    Post.create!(
      title: "t" * 30,
      type_name: "n" * 10,
      key: "k" * 10,
      body: "b" * 100,
      upvotes: rand(30),
      author_id: rand(30),
    )
  end
end

Comment.transaction do
  Post.where(nil).each do |post|
    20.times do
      Comment.create!(
        post_id: post.id,
        author_id: rand(30),
        body: "c" * 30,
        tags: "g" * 30,
        published_at: Time.now
      )
    end
  end
end
puts "OK: seeded 100 posts with 2000 comments"

def run
  posts = Post.includes(:comments).order(id: :asc).limit(100)
  posts.each do |post|
    post.title
    post.title
    post.title
    post.body
    post.type_name
    post.upvotes
    post.updated_at
    post.comments.each do |comment|
      comment.body
      comment.published_at
    end
  end
end

# Warmup
puts "Warming up..."
run

# Benchmark
n_iters = 20
puts "Running #{n_iters} iterations (10 queries each)..."
times = []
n_iters.times do |i|
  t0 = Time.now
  10.times { run }
  elapsed = Time.now - t0
  ms = (elapsed * 1000).to_i
  puts "  ##{i + 1}: #{ms}ms"
  times << elapsed
end

avg_ms = (times.sum / times.size * 1000).to_i
puts "Average: #{avg_ms}ms"
