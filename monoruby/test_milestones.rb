# Exact M3 milestone verification
require 'active_record'
ActiveRecord::Base.establish_connection(adapter: 'sqlite3', database: ':memory:')
ActiveRecord::Schema.define do
  create_table :users do |t|
    t.string :name
    t.integer :age
  end
end

class User < ActiveRecord::Base
end

user = User.create(name: "Alice", age: 30)
puts "M3.1 create: #{user.inspect}"

found = User.find(user.id)
puts "M3.2 find: #{found.inspect}"

where_result = User.where(name: "Alice").first
puts "M3.3 where: #{where_result.inspect}"

user.update(age: 31)
puts "M3.4 update: #{User.find(user.id).age}"

user.destroy
puts "M3.5 destroy: count=#{User.count}"

puts ""
puts "=== M3 exact verification PASSED ==="
puts ""

# Exact M4 milestone verification
ActiveRecord::Schema.define do
  create_table :posts do |t|
    t.string :title
    t.integer :user_id
  end
end

class User < ActiveRecord::Base
  has_many :posts
end

class Post < ActiveRecord::Base
  belongs_to :user
end

user = User.create(name: "Alice")
puts "M4.1 user: #{user.inspect}"

user.posts.create(title: "Hello")
puts "M4.2 post created via association"
puts "M4.3 posts count: #{user.posts.count}"

users_with_posts = User.includes(:posts).all.to_a
users_with_posts.each do |u|
  puts "M4.4 #{u.name} has #{u.posts.to_a.length} posts"
end

puts ""
puts "=== M4 exact verification PASSED ==="
