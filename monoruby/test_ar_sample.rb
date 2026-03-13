require 'active_record'

puts "=== ActiveRecord on monoruby ==="
puts

# M3: Basic CRUD
puts "--- M3: establish_connection ---"
ActiveRecord::Base.establish_connection(adapter: 'sqlite3', database: ':memory:')
puts "OK: Connected to SQLite3 in-memory database"

puts
puts "--- M3: Schema.define ---"
ActiveRecord::Schema.define do
  create_table :users do |t|
    t.string :name
    t.integer :age
    t.string :email
  end

  create_table :posts do |t|
    t.string :title
    t.string :body
    t.integer :user_id
  end
end
puts "OK: Tables created"

puts
puts "--- M3: Define models ---"
class User < ActiveRecord::Base
  has_many :posts
end

class Post < ActiveRecord::Base
  belongs_to :user
end
puts "OK: Models defined"

puts
puts "--- M3: Create ---"
alice = User.create(name: "Alice", age: 30, email: "alice@example.com")
puts "Created: #{alice.name} (id=#{alice.id}, age=#{alice.age})"

bob = User.create(name: "Bob", age: 25, email: "bob@example.com")
puts "Created: #{bob.name} (id=#{bob.id}, age=#{bob.age})"

charlie = User.create(name: "Charlie", age: 35, email: "charlie@example.com")
puts "Created: #{charlie.name} (id=#{charlie.id}, age=#{charlie.age})"

puts
puts "--- M3: Find ---"
found = User.find(alice.id)
puts "Find(#{alice.id}): #{found.name}"

puts
puts "--- M3: Find_by ---"
found2 = User.find_by(name: "Bob")
puts "Find_by(name: Bob): #{found2.name}, age=#{found2.age}"

puts
puts "--- M3: Where ---"
young = User.where("age < 35")
puts "Where(age < 35): #{young.map { |u| u.name }}"

puts
puts "--- M3: Order + Limit ---"
ordered = User.order("age DESC").limit(2)
puts "Order by age DESC, limit 2: #{ordered.map { |u| "#{u.name}(#{u.age})" }}"

puts
puts "--- M3: All / Count ---"
puts "All users: #{User.all.map { |u| u.name }}"
puts "Count: #{User.count}"

puts
puts "--- M3: First / Last ---"
puts "First: #{User.first.name}"
puts "Last: #{User.last.name}"

puts
puts "--- M3: Update ---"
alice.update(age: 31)
puts "Updated Alice age: #{User.find(alice.id).age}"

puts
puts "--- M3: Destroy ---"
charlie.destroy
puts "Destroyed Charlie. Count: #{User.count}"

puts
puts "=== M3 Complete ==="

puts
puts "--- M4: Associations ---"
post1 = alice.posts.create(title: "Hello World", body: "My first post")
puts "Created post: #{post1.title} (id=#{post1.id}, user_id=#{post1.user_id})"

post2 = alice.posts.create(title: "Ruby is fun", body: "I love Ruby")
puts "Created post: #{post2.title} (id=#{post2.id}, user_id=#{post2.user_id})"

post3 = bob.posts.create(title: "Bob's post", body: "Hello from Bob")
puts "Created post: #{post3.title} (id=#{post3.id}, user_id=#{post3.user_id})"

puts
puts "--- M4: has_many ---"
puts "Alice's posts: #{alice.posts.map { |p| p.title }}"
puts "Alice's post count: #{alice.posts.count}"
puts "Bob's posts: #{bob.posts.map { |p| p.title }}"

puts
puts "--- M4: belongs_to ---"
puts "Post '#{post1.title}' belongs to: #{post1.user.name}"

puts
puts "=== M4 Complete ==="
puts
puts "All tests passed!"
