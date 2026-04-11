#!/usr/bin/env monoruby
# Test ActiveRecord loading by bypassing bundler entirely.

# Mark set.rb as already loaded to avoid loading system set.rb
# which conflicts with monoruby's built-in Set
$LOADED_FEATURES << "/opt/rbenv/versions/3.3.6/lib/ruby/3.3.0/set.rb"

# Patch missing Enumerable#to_set
module Enumerable
  def to_set(klass = Set, *args, &block)
    klass.new(self, *args, &block)
  end
end

GEM_BASE = "/opt/rbenv/versions/3.3.6/lib/ruby/gems/3.3.0/gems"

[
  "activerecord-8.1.1",
  "activemodel-8.1.1",
  "activesupport-8.1.1",
  "i18n-1.14.7",
  "tzinfo-2.0.6",
  "concurrent-ruby-1.3.5",
  "minitest-5.26.2",
  "bigdecimal-3.3.1",
  "mutex_m-0.3.0",
  "base64-0.3.0",
  "benchmark-0.3.0",
  "connection_pool-3.0.2",
  "drb-2.2.3",
  "logger-1.7.0",
  "securerandom-0.4.1",
  "timeout-0.5.0",
  "uri-1.1.1",
  "sqlite3-2.7.3-x86_64-linux-gnu",
].each do |gem|
  path = "#{GEM_BASE}/#{gem}/lib"
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
