# ActiveRecord benchmark runner for monoruby
# Bypasses Bundler.setup (which requires socket.so) and sets up gem paths manually.

# Add gem lib paths directly
gem_libs = %w[
  activemodel-8.1.1
  activerecord-8.1.1
  activesupport-8.1.1
  base64-0.3.0
  bigdecimal-3.3.1
  concurrent-ruby-1.3.5
  connection_pool-3.0.2
  drb-2.2.3
  i18n-1.14.7
  json-2.17.1
  logger-1.7.0
  minitest-5.26.2
  mutex_m-0.3.0
  securerandom-0.4.1
  sqlite3-2.7.3-x86_64-linux-gnu
  timeout-0.5.0
  tzinfo-2.0.6
  uri-1.1.1
]

gem_libs.each do |gem_name|
  lib_path = "/opt/rbenv/versions/3.3.6/lib/ruby/gems/3.3.0/gems/#{gem_name}/lib"
  $LOAD_PATH.unshift(lib_path) if File.directory?(lib_path)
end

require "active_record"

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

Random.srand(1337)

Post.transaction do
  100.times do
    post = Post.create!(
      title: Random.alphanumeric(30),
      type_name: Random.alphanumeric(10),
      key: Random.alphanumeric(10),
      body: Random.alphanumeric(100),
      upvotes: rand(30),
      author_id: rand(30),
    )
    20.times do
      post.comments.create!(
        author_id: rand(30),
        body: Random.alphanumeric(30),
        tags: Random.alphanumeric(30),
        published_at: Time.now
      )
    end
  end
end

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

run # heat any caches

# Benchmark: 20 iterations x 10 runs
times = []
20.times do |i|
  t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  10.times { run }
  elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0
  ms = (elapsed * 1000).to_i
  times << elapsed
  puts "itr ##{i+1}: #{ms}ms"
end

avg_ms = ((times.sum / times.size) * 1000).to_i
puts
puts "Average: #{avg_ms}ms (#{times.size} iterations)"
