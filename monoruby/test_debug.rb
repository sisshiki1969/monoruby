require 'active_record'

ActiveRecord::Base.establish_connection(adapter: 'sqlite3', database: ':memory:')

ActiveRecord::Schema.define do
  create_table :users do |t|
    t.string :name
  end
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

author = User.create(name: "Diana")
post1 = Post.create(title: "Hello World", user_id: author.id)

puts "Step 1: get proxy..."
proxy = author.posts
puts "proxy class: #{proxy.class}"

puts "Step 2: count via _build_relation..."
c = Post.where(user_id: author.id).count
puts "count = #{c}"

puts "Step 3: load_target..."
proxy.load_target
puts "loaded: #{proxy.instance_variable_get(:@loaded)}"
puts "records: #{proxy.instance_variable_get(:@records).length}"

puts "Step 4: each on records..."
records = proxy.instance_variable_get(:@records)
records.each { |p| puts "  - #{p.title}" }
puts "Step 4 done"

puts "Step 5: each on proxy..."
proxy.each { |p| puts "  - #{p.title}" }
puts "Step 5 done"
