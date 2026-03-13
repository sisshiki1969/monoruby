# Test ActiveRecord implementation for monoruby
# Covers both M3 and M4 milestones

require 'active_record'

puts "=== M3 Milestone: Basic ActiveRecord ==="
puts ""

# 1. Establish connection
puts "1. Establishing connection..."
ActiveRecord::Base.establish_connection(adapter: 'sqlite3', database: ':memory:')
puts "   OK - Connected to in-memory SQLite3"

# 2. Define schema
puts "2. Defining schema..."
ActiveRecord::Schema.define do
  create_table :users do |t|
    t.string :name
    t.integer :age
  end
end
puts "   OK - Created users table"

# 3. Define model
class User < ActiveRecord::Base
end

puts "3. Model defined: User"
puts "   table_name: #{User.table_name}"
puts "   column_names: #{User.column_names.inspect}"
puts "   primary_key: #{User.primary_key}"

# 4. Create
puts "4. Creating user..."
user = User.create(name: "Alice", age: 30)
puts "   Created: #{user.inspect}"
puts "   id=#{user.id}, name=#{user.name}, age=#{user.age}"
puts "   persisted?=#{user.persisted?}, new_record?=#{user.new_record?}"

# 5. Find
puts "5. Finding user by id..."
found = User.find(user.id)
puts "   Found: #{found.inspect}"

# 6. Find_by
puts "6. Find by name..."
found2 = User.find_by(name: "Alice")
puts "   Found: #{found2.inspect}"

# 7. Where
puts "7. Where query..."
results = User.where(name: "Alice").first
puts "   Where result: #{results.inspect}"

# 8. Update
puts "8. Updating user..."
user.update(age: 31)
puts "   Updated: #{user.inspect}"
reloaded = User.find(user.id)
puts "   Reloaded: #{reloaded.inspect}"
puts "   age=#{reloaded.age}"

# 9. Create more users
puts "9. Creating more users..."
User.create(name: "Bob", age: 25)
User.create(name: "Charlie", age: 35)

# 10. All
puts "10. All users..."
all_users = User.all.to_a
puts "   Count: #{all_users.length}"
all_users.each { |u| puts "   - #{u.name} (age: #{u.age})" }

# 11. Count
puts "11. Count: #{User.count}"

# 12. First / Last
puts "12. First: #{User.first.inspect}"
puts "    Last: #{User.last.inspect}"

# 13. Order
puts "13. Order by age DESC..."
ordered = User.order(age: :desc).to_a
ordered.each { |u| puts "   - #{u.name} (age: #{u.age})" }

# 14. Limit
puts "14. Limit 2..."
limited = User.limit(2).to_a
puts "   Got #{limited.length} records"

# 15. Destroy
puts "15. Destroying first user..."
user.destroy
puts "   destroyed?=#{user.destroyed?}"
puts "   User count after destroy: #{User.count}"

# 16. Pluck
puts "16. Pluck names: #{User.pluck(:name).inspect}"

puts ""
puts "=== M3 Milestone PASSED ==="
puts ""

# ===================================================
# M4 Milestone: Associations
# ===================================================

puts "=== M4 Milestone: Associations ==="
puts ""

# Create posts table
ActiveRecord::Schema.define do
  create_table :posts do |t|
    t.string :title
    t.text :body
    t.integer :user_id
  end

  create_table :profiles do |t|
    t.string :bio
    t.integer :user_id
  end
end
puts "1. Created posts and profiles tables"

# Redefine User with associations, and define Post and Profile
User.reset_column_information

class User < ActiveRecord::Base
  has_many :posts
  has_one :profile
end

class Post < ActiveRecord::Base
  belongs_to :user
end

class Profile < ActiveRecord::Base
  belongs_to :user
end

puts "2. Defined models with associations"

# Create user
puts "3. Creating user with posts..."
author = User.create(name: "Diana", age: 28)
puts "   Created author: #{author.inspect}"

# Create posts directly (not via association proxy first)
puts "   Creating posts directly..."
post1 = Post.create(title: "Hello World", body: "My first post", user_id: author.id)
puts "   Created post1: #{post1.inspect}"
post2 = Post.create(title: "Second Post", body: "Another post", user_id: author.id)
puts "   Created post2: #{post2.inspect}"

# Read posts via association
puts "4. Reading posts via association..."
author_posts = author.posts
puts "   author.posts count = #{author_posts.count}"
author_posts.each { |p| puts "   - #{p.title} (user_id: #{p.user_id})" }

# Create post via association proxy
puts "4b. Creating post via association..."
post3 = author.posts.create(title: "Third Post", body: "Via association")
puts "   Created post3: #{post3.inspect}"
puts "   Total posts: #{author.posts.count}"

# belongs_to
puts "5. Testing belongs_to..."
loaded_post = Post.find(post1.id)
post_author = loaded_post.user
puts "   post.user = #{post_author.inspect}"

# has_one
puts "6. Testing has_one..."
profile = Profile.create(bio: "I love coding", user_id: author.id)
puts "   Created profile: #{profile.inspect}"
loaded_author = User.find(author.id)
author_profile = loaded_author.profile
puts "   author.profile = #{author_profile.inspect}"

# Eager loading with includes
puts "7. Testing includes (eager loading)..."
users_with_posts = User.includes(:posts).all.to_a
users_with_posts.each do |u|
  post_list = u.posts.to_a
  puts "   #{u.name} has #{post_list.length} posts"
end

# Scopes
puts "8. Testing scopes..."
class User < ActiveRecord::Base
  scope :adults, -> { where("\"age\" >= ?", 18) }
  scope :named, ->(n) { where(name: n) }
end

adults = User.adults.to_a
puts "   Adults: #{adults.length}"
diana = User.named("Diana").first
puts "   Named Diana: #{diana.inspect}"

# Query chaining
puts "9. Testing query chaining..."
result = User.where("\"age\" > ?", 20).order(name: :asc).limit(5).to_a
puts "   Users age > 20 (ordered by name, limit 5):"
result.each { |u| puts "   - #{u.name} (#{u.age})" }

puts ""
puts "=== M4 Milestone PASSED ==="
puts ""
puts "All ActiveRecord tests completed successfully!"
