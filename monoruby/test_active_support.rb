# Test ActiveSupport implementation for monoruby

passed = 0
failed = 0

def assert_equal(expected, actual, msg = "")
  if expected == actual
    puts "  PASS: #{msg}"
    return true
  else
    puts "  FAIL: #{msg} - expected #{expected.inspect}, got #{actual.inspect}"
    return false
  end
end

# === M1 Milestone: Basic require and string extensions ===
puts "=== M1 Milestone Test ==="
require 'active_support'
require 'active_support/core_ext/string'

result = "hello_world".camelize
if assert_equal("HelloWorld", result, '"hello_world".camelize')
  passed += 1
else
  failed += 1
end

result = "User".tableize
if assert_equal("users", result, '"User".tableize')
  passed += 1
else
  failed += 1
end

# === Inflector Tests ===
puts "\n=== Inflector Tests ==="

result = ActiveSupport::Inflector.pluralize("post")
if assert_equal("posts", result, 'pluralize("post")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.singularize("posts")
if assert_equal("post", result, 'singularize("posts")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.camelize("active_record")
if assert_equal("ActiveRecord", result, 'camelize("active_record")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.underscore("ActiveRecord")
if assert_equal("active_record", result, 'underscore("ActiveRecord")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.classify("posts")
if assert_equal("Post", result, 'classify("posts")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.foreign_key("User")
if assert_equal("user_id", result, 'foreign_key("User")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.demodulize("ActiveRecord::Base")
if assert_equal("Base", result, 'demodulize("ActiveRecord::Base")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.tableize("UserProfile")
if assert_equal("user_profiles", result, 'tableize("UserProfile")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.pluralize("person")
if assert_equal("people", result, 'pluralize("person")')
  passed += 1
else
  failed += 1
end

result = ActiveSupport::Inflector.singularize("people")
if assert_equal("person", result, 'singularize("people")')
  passed += 1
else
  failed += 1
end

# === String Extensions ===
puts "\n=== String Extensions ==="

result = "active_record/base".camelize
if assert_equal("ActiveRecord::Base", result, '"active_record/base".camelize')
  passed += 1
else
  failed += 1
end

result = "ActiveRecord::Base".underscore
if assert_equal("active_record/base", result, '"ActiveRecord::Base".underscore')
  passed += 1
else
  failed += 1
end

result = "post".pluralize
if assert_equal("posts", result, '"post".pluralize')
  passed += 1
else
  failed += 1
end

result = "posts".singularize
if assert_equal("post", result, '"posts".singularize')
  passed += 1
else
  failed += 1
end

result = "  hello   world  ".squish
if assert_equal("hello world", result, '"  hello   world  ".squish')
  passed += 1
else
  failed += 1
end

# === Object blank?/present? ===
puts "\n=== Blank/Present Tests ==="

if assert_equal(true, nil.blank?, 'nil.blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(true, "".blank?, '"".blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(false, "hello".blank?, '"hello".blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(true, [].blank?, '[].blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(false, [1].blank?, '[1].blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(true, {}.blank?, '{}.blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(false, 0.blank?, '0.blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(true, false.blank?, 'false.blank?')
  passed += 1
else
  failed += 1
end

if assert_equal(false, true.blank?, 'true.blank?')
  passed += 1
else
  failed += 1
end

result = "hello".presence
if assert_equal("hello", result, '"hello".presence')
  passed += 1
else
  failed += 1
end

result = "".presence
if assert_equal(nil, result, '"".presence')
  passed += 1
else
  failed += 1
end

# === Hash Extensions ===
puts "\n=== Hash Extensions ==="

h = {a: 1, b: 2}
result = h.stringify_keys
if assert_equal({"a" => 1, "b" => 2}, result, 'stringify_keys')
  passed += 1
else
  failed += 1
end

h = {"a" => 1, "b" => 2}
result = h.symbolize_keys
if assert_equal({a: 1, b: 2}, result, 'symbolize_keys')
  passed += 1
else
  failed += 1
end

h = {a: 1, b: 2, c: 3}
result = h.except(:b)
if assert_equal({a: 1, c: 3}, result, 'except(:b)')
  passed += 1
else
  failed += 1
end

h = {a: 1, b: 2, c: 3}
result = h.slice(:a, :c)
if assert_equal({a: 1, c: 3}, result, 'slice(:a, :c)')
  passed += 1
else
  failed += 1
end

h = {a: 1, b: 2}
result = h.reverse_merge(b: 3, c: 4)
if assert_equal({a: 1, b: 2, c: 4}, result, 'reverse_merge')
  passed += 1
else
  failed += 1
end

# HashWithIndifferentAccess
h = {a: 1, "b" => 2}.with_indifferent_access
if assert_equal(1, h[:a], 'HWIA[:a]')
  passed += 1
else
  failed += 1
end
if assert_equal(1, h["a"], 'HWIA["a"]')
  passed += 1
else
  failed += 1
end
if assert_equal(2, h[:b], 'HWIA[:b]')
  passed += 1
else
  failed += 1
end

# === Array Extensions ===
puts "\n=== Array Extensions ==="

result = [1, 2, 3, {a: 1}].extract_options!
if assert_equal({a: 1}, result, 'extract_options!')
  passed += 1
else
  failed += 1
end

result = Array.wrap(nil)
if assert_equal([], result, 'Array.wrap(nil)')
  passed += 1
else
  failed += 1
end

result = Array.wrap([1, 2])
if assert_equal([1, 2], result, 'Array.wrap([1,2])')
  passed += 1
else
  failed += 1
end

result = Array.wrap("hello")
if assert_equal(["hello"], result, 'Array.wrap("hello")')
  passed += 1
else
  failed += 1
end

# === Integer Extensions ===
puts "\n=== Integer Extensions ==="

result = 1.ordinalize
if assert_equal("1st", result, '1.ordinalize')
  passed += 1
else
  failed += 1
end

result = 2.ordinalize
if assert_equal("2nd", result, '2.ordinalize')
  passed += 1
else
  failed += 1
end

result = 3.ordinalize
if assert_equal("3rd", result, '3.ordinalize')
  passed += 1
else
  failed += 1
end

result = 11.ordinalize
if assert_equal("11th", result, '11.ordinalize')
  passed += 1
else
  failed += 1
end

result = 1.kilobytes
if assert_equal(1024, result, '1.kilobytes')
  passed += 1
else
  failed += 1
end

result = 1.megabytes
if assert_equal(1048576, result, '1.megabytes')
  passed += 1
else
  failed += 1
end

result = 1.hours
if assert_equal(3600, result, '1.hours')
  passed += 1
else
  failed += 1
end

result = 1.days
if assert_equal(86400, result, '1.days')
  passed += 1
else
  failed += 1
end

# === Concern ===
puts "\n=== Concern Tests ==="

module TestConcernA
  extend ActiveSupport::Concern

  included do
    @concern_included = true
  end

  class_methods do
    def concern_class_method
      "from concern"
    end
  end

  def concern_instance_method
    "instance method"
  end
end

class TestConcernClass
  include TestConcernA
end

obj = TestConcernClass.new
if assert_equal("instance method", obj.concern_instance_method, 'Concern instance method')
  passed += 1
else
  failed += 1
end

if assert_equal("from concern", TestConcernClass.concern_class_method, 'Concern class method')
  passed += 1
else
  failed += 1
end

# === Callbacks ===
puts "\n=== Callbacks Tests ==="

class TestCallbackClass
  include ActiveSupport::Callbacks

  define_callbacks :save

  set_callback :save, :before, :before_save_method
  set_callback :save, :after, :after_save_method

  attr_reader :log

  def initialize
    @log = []
  end

  def save
    run_callbacks :save do
      @log << :save
    end
  end

  def before_save_method
    @log << :before
  end

  def after_save_method
    @log << :after
  end
end

obj = TestCallbackClass.new
obj.save
if assert_equal([:before, :save, :after], obj.log, 'Callbacks before/after order')
  passed += 1
else
  failed += 1
end

# === Module delegate ===
puts "\n=== Module Delegate Tests ==="

class TestDelegator
  attr_reader :name

  def initialize
    @name = "delegated"
    @target = TestDelegateTarget.new
  end

  delegate :greet, to: :@target
end

class TestDelegateTarget
  def greet
    "hello"
  end
end

obj = TestDelegator.new
if assert_equal("hello", obj.greet, 'delegate :greet, to: :@target')
  passed += 1
else
  failed += 1
end

# === DescendantsTracker ===
puts "\n=== DescendantsTracker Tests ==="

class TrackBase
  extend ActiveSupport::DescendantsTracker
end

class TrackChild < TrackBase
end
TrackBase.register_descendant(TrackChild)

class TrackGrandChild < TrackChild
end
TrackChild.register_descendant(TrackGrandChild)

result = TrackBase.descendants
child_found = result.include?(TrackChild)
grandchild_found = result.include?(TrackGrandChild)
if assert_equal(true, child_found && grandchild_found, 'DescendantsTracker.descendants (manual)')
  passed += 1
else
  failed += 1
end

# === Summary ===
puts "\n========================================="
puts "Results: #{passed} passed, #{failed} failed"
puts "========================================="

if failed > 0
  puts "SOME TESTS FAILED"
else
  puts "ALL TESTS PASSED"
end
