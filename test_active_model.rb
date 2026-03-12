require 'active_model'

puts "=== ActiveModel Test Suite ==="
$pass = 0
$fail = 0

def assert(msg, condition)
  if condition
    puts "  PASS: #{msg}"
    $pass += 1
  else
    puts "  FAIL: #{msg}"
    $fail += 1
  end
end

def assert_equal(msg, expected, actual)
  if expected == actual
    puts "  PASS: #{msg}"
    $pass += 1
  else
    puts "  FAIL: #{msg} (expected: #{expected.inspect}, got: #{actual.inspect})"
    $fail += 1
  end
end

# ============================================================
# 1. ActiveModel::Errors
# ============================================================
puts "\n--- ActiveModel::Errors ---"

class ErrorTestModel
  def initialize
    @errors = ActiveModel::Errors.new(self)
  end
  def errors; @errors; end
  def name; "test"; end
end

m = ErrorTestModel.new
m.errors.add(:name, :blank)
assert("add error", m.errors.size == 1)
assert_equal("error message", "can't be blank", m.errors[:name].first)
assert_equal("full message", "Name can't be blank", m.errors.full_messages.first)
assert("include? attribute", m.errors.include?(:name))
assert("not include? other", !m.errors.include?(:age))

m.errors.add(:base, :invalid, message: "Something went wrong")
assert_equal("base error full message", "Something went wrong", m.errors.full_messages_for(:base).first)

m.errors.clear
assert("clear errors", m.errors.empty?)

# String-based error message (backward compat)
m.errors.add(:name, "is too short")
assert_equal("string message", "is too short", m.errors[:name].first)

# ============================================================
# 2. ActiveModel::Attributes
# ============================================================
puts "\n--- ActiveModel::Attributes ---"

class AttrModel
  include ActiveModel::Attributes
  attribute :name, :string
  attribute :age, :integer
  attribute :active, :boolean, default: true
  attribute :score, :float
end

obj = AttrModel.new
assert_equal("default value", true, obj.active)
assert_equal("nil before set", nil, obj.name)

obj.name = "Alice"
assert_equal("string attr set", "Alice", obj.name)

obj.age = "25"
assert_equal("integer casting from string", 25, obj.age)

obj.age = 30
assert_equal("integer attr", 30, obj.age)

obj.active = "false"
assert_equal("boolean cast from string", false, obj.active)

obj.active = 1
assert_equal("boolean cast from int", true, obj.active)

obj.score = "3.14"
assert_equal("float cast from string", 3.14, obj.score)

# Init with hash
obj2 = AttrModel.new(name: "Bob", age: 42)
assert_equal("init hash name", "Bob", obj2.name)
assert_equal("init hash age", 42, obj2.age)

# attributes method
attrs = obj2.attributes
assert_equal("attributes hash", "Bob", attrs["name"])

# attribute_names
assert("attribute_names includes name", obj2.attribute_names.include?("name"))

# ============================================================
# 3. ActiveModel::Validations - Presence
# ============================================================
puts "\n--- ActiveModel::Validations - Presence ---"

class PresModel
  include ActiveModel::Validations

  attr_accessor :name, :email

  validates :name, presence: true
  validates :email, presence: true
end

p1 = PresModel.new
p1.name = nil
p1.email = nil
assert("invalid when blank", !p1.valid?)
assert("has name error", p1.errors.include?(:name))
assert("has email error", p1.errors.include?(:email))

p1.name = "Alice"
p1.email = "alice@example.com"
assert("valid when present", p1.valid?)

# ============================================================
# 4. ActiveModel::Validations - Length
# ============================================================
puts "\n--- ActiveModel::Validations - Length ---"

class LenModel
  include ActiveModel::Validations

  attr_accessor :name, :bio

  validates :name, length: { minimum: 2, maximum: 50 }
  validates :bio, length: { in: 10..500 }
end

l = LenModel.new
l.name = "A"
l.bio = "short"
assert("invalid: name too short", !l.valid?)
assert("name too_short error", l.errors[:name].any? { |m| m.include?("too short") })
assert("bio too_short error", l.errors[:bio].any? { |m| m.include?("too short") })

l.name = "Alice"
l.bio = "This is a long enough bio for validation to pass."
assert("valid lengths", l.valid?)

# ============================================================
# 5. ActiveModel::Validations - Format
# ============================================================
puts "\n--- ActiveModel::Validations - Format ---"

class FmtModel
  include ActiveModel::Validations

  attr_accessor :email

  validates :email, format: { with: /\A[^@\s]+@[^@\s]+\z/ }
end

f = FmtModel.new
f.email = "bad"
assert("invalid format", !f.valid?)

f.email = "test@example.com"
assert("valid format", f.valid?)

# ============================================================
# 6. ActiveModel::Validations - Numericality
# ============================================================
puts "\n--- ActiveModel::Validations - Numericality ---"

class NumModel
  include ActiveModel::Validations

  attr_accessor :age, :score

  validates :age, numericality: { only_integer: true, greater_than: 0 }
  validates :score, numericality: { greater_than_or_equal_to: 0, less_than_or_equal_to: 100 }
end

n = NumModel.new
n.age = "abc"
n.score = 50
assert("invalid: not a number", !n.valid?)
assert("not_a_number error", n.errors[:age].any? { |m| m.include?("not a number") })

n.age = 25
n.score = 150
assert("invalid: score too high", !n.valid?)
assert("score less_than error", n.errors[:score].any? { |m| m.include?("less than or equal to") })

n.age = 25
n.score = 85
assert("valid numericality", n.valid?)

# ============================================================
# 7. ActiveModel::Validations - Inclusion
# ============================================================
puts "\n--- ActiveModel::Validations - Inclusion ---"

class InclModel
  include ActiveModel::Validations

  attr_accessor :role

  validates :role, inclusion: { in: ["admin", "user", "guest"] }
end

i = InclModel.new
i.role = "superadmin"
assert("invalid: not in list", !i.valid?)

i.role = "admin"
assert("valid: in list", i.valid?)

# ============================================================
# 8. ActiveModel::Validations - Custom validate
# ============================================================
puts "\n--- ActiveModel::Validations - Custom ---"

class CustModel
  include ActiveModel::Validations

  attr_accessor :start_date, :end_date

  validate :end_after_start

  def end_after_start
    if start_date && end_date && end_date < start_date
      errors.add(:end_date, "must be after start date")
    end
  end
end

c = CustModel.new
c.start_date = 10
c.end_date = 5
assert("invalid: custom validation", !c.valid?)
assert("custom error message", c.errors[:end_date].first == "must be after start date")

c.end_date = 15
assert("valid: custom passes", c.valid?)

# ============================================================
# 9. ActiveModel::Callbacks
# ============================================================
puts "\n--- ActiveModel::Callbacks ---"

class CbModel
  include ActiveModel::Callbacks

  define_model_callbacks :save

  attr_accessor :log

  before_save :log_before
  after_save :log_after

  def initialize
    @log = []
  end

  def save
    run_callbacks(:save) do
      @log << :save
    end
  end

  def log_before
    @log << :before_save
  end

  def log_after
    @log << :after_save
  end
end

cb = CbModel.new
cb.save
assert_equal("callbacks order", [:before_save, :save, :after_save], cb.log)

# ============================================================
# 10. ActiveModel::Dirty
# ============================================================
puts "\n--- ActiveModel::Dirty ---"

class DirtyModel
  include ActiveModel::Dirty

  define_attribute_methods :name, :age

  def name
    @name
  end

  def name=(val)
    _track_attribute_change(:name, @name, val)
    @name = val
  end

  def age
    @age
  end

  def age=(val)
    _track_attribute_change(:age, @age, val)
    @age = val
  end
end

d = DirtyModel.new
assert("not changed initially", !d.changed?)

d.name = "Alice"
assert("changed after set", d.changed?)
assert("name_changed?", d.name_changed?)
assert("name_was nil", d.name_was.nil?)
assert_equal("name_change", [nil, "Alice"], d.name_change)

assert("age not changed", !d.age_changed?)

d.changes_applied
assert("not changed after applied", !d.changed?)
assert("previously_changed?", d.previously_changed?)
assert("name_previously_changed?", d.name_previously_changed?)
assert_equal("previous name change", [nil, "Alice"], d.name_previous_change)

# Reset tracking
d.name = "Bob"
assert("changed after second set", d.changed?)
assert_equal("name change from Alice to Bob", ["Alice", "Bob"], d.name_change)

# Restore
d.restore_attributes
assert("not changed after restore", !d.changed?)
assert_equal("name restored", "Alice", d.name)

# ============================================================
# 11. ActiveModel::Naming
# ============================================================
puts "\n--- ActiveModel::Naming ---"

class User
  include ActiveModel::Naming
end

mn = User.model_name
assert_equal("model name", "User", mn.to_s)
assert_equal("singular", "user", mn.singular)
assert_equal("plural", "users", mn.plural)
assert_equal("element", "user", mn.element)
assert_equal("human", "User", mn.human)
assert_equal("param_key", "user", mn.param_key)

# ============================================================
# 12. ActiveModel::Conversion
# ============================================================
puts "\n--- ActiveModel::Conversion ---"

class ConvModel
  include ActiveModel::Conversion

  attr_accessor :id

  def initialize(id = nil)
    @id = id
  end
end

cv = ConvModel.new
assert("to_model is self", cv.to_model == cv)
assert("to_key nil without id", cv.to_key.nil?)
assert("to_param nil without id", cv.to_param.nil?)

cv.id = 42
assert_equal("to_key with id", [42], cv.to_key)
assert_equal("to_param with id", "42", cv.to_param)

# ============================================================
# 13. ActiveModel::Model (combined)
# ============================================================
puts "\n--- ActiveModel::Model ---"

class Person
  include ActiveModel::Model

  attr_accessor :name, :email, :age

  validates :name, presence: true
  validates :email, presence: true, format: { with: /\A[^@\s]+@[^@\s]+\z/ }
end

p = Person.new(name: "Alice", email: "alice@example.com", age: 30)
assert_equal("model init name", "Alice", p.name)
assert_equal("model init email", "alice@example.com", p.email)
assert("model valid", p.valid?)
assert("model not persisted", !p.persisted?)
assert("model new_record", p.new_record?)

p2 = Person.new(name: "", email: "bad")
assert("model invalid", !p2.valid?)
assert("model has name error", p2.errors.include?(:name))
assert("model has email error", p2.errors.include?(:email))

# model_name via Naming
assert_equal("person model_name", "Person", Person.model_name.to_s)
assert_equal("person plural", "persons", Person.model_name.plural)

# ============================================================
# Summary
# ============================================================
puts "\n=== Results: #{$pass} passed, #{$fail} failed ==="
if $fail > 0
  puts "SOME TESTS FAILED"
  exit 1
else
  puts "ALL TESTS PASSED"
end
