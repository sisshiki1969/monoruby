require 'sqlite3'

puts "=== SQLite3 M2 Milestone Test ==="

# Test 1: M2 Milestone - exact example from plan
db = SQLite3::Database.new(':memory:')
db.execute("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
db.execute("INSERT INTO users (name) VALUES (?)", ["Alice"])
result = db.execute("SELECT * FROM users")
raise "M2 FAILED" unless result == [[1, "Alice"]]
puts "1. M2 milestone example: OK"

# Test 2: Multiple inserts and selects
db.execute("INSERT INTO users (name) VALUES (?)", ["Bob"])
db.execute("INSERT INTO users (name) VALUES (?)", ["Charlie"])
rows = db.execute("SELECT * FROM users")
raise "Multi-insert FAILED" unless rows == [[1, "Alice"], [2, "Bob"], [3, "Charlie"]]
puts "2. Multiple inserts: OK"

# Test 3: Parameter binding with multiple params
db.execute("CREATE TABLE scores (name TEXT, score INTEGER)")
db.execute("INSERT INTO scores VALUES (?, ?)", ["Alice", 95])
db.execute("INSERT INTO scores VALUES (?, ?)", ["Bob", 87])
rows = db.execute("SELECT * FROM scores WHERE score > ?", [90])
raise "Param binding FAILED" unless rows == [["Alice", 95]]
puts "3. Multiple parameter binding: OK"

# Test 4: Column types (integer, float, text, null)
db.execute("CREATE TABLE types (i INTEGER, f REAL, t TEXT, n TEXT)")
db.execute("INSERT INTO types VALUES (?, ?, ?, ?)", [42, 3.14, "hello", nil])
row = db.execute("SELECT * FROM types").first
raise "Types FAILED: #{row.inspect}" unless row[0] == 42 && row[1] == 3.14 && row[2] == "hello" && row[3].nil?
puts "4. Column type handling: OK"

# Test 5: changes and last_insert_row_id
db.execute("UPDATE users SET name = 'David' WHERE id = 2")
raise "Changes FAILED" unless db.changes == 1
puts "5. changes: OK"

db.execute("INSERT INTO users (name) VALUES (?)", ["Eve"])
raise "last_insert_row_id FAILED" unless db.last_insert_row_id == 4
puts "6. last_insert_row_id: OK"

# Test 7: get_first_value
count = db.get_first_value("SELECT COUNT(*) FROM users")
raise "get_first_value FAILED: #{count}" unless count == 4
puts "7. get_first_value: OK"

# Test 8: get_first_row
row = db.get_first_row("SELECT * FROM users WHERE id = ?", [1])
raise "get_first_row FAILED" unless row == [1, "Alice"]
puts "8. get_first_row: OK"

# Test 9: Prepared statement with ResultSet
stmt = db.prepare("SELECT name FROM users ORDER BY id")
rs = stmt.execute
names = rs.to_a
raise "ResultSet FAILED: #{names.inspect}" unless names == [["Alice"], ["David"], ["Charlie"], ["Eve"]]
stmt.close
puts "9. Prepared statement + ResultSet: OK"

# Test 10: Transaction
db.transaction do |tx|
  tx.execute("INSERT INTO users (name) VALUES (?)", ["Frank"])
  tx.execute("INSERT INTO users (name) VALUES (?)", ["Grace"])
end
count = db.get_first_value("SELECT COUNT(*) FROM users")
raise "Transaction FAILED" unless count == 6
puts "10. Transaction: OK"

# Test 11: execute_batch
db.execute_batch("CREATE TABLE batch_test (id INTEGER); INSERT INTO batch_test VALUES (1); INSERT INTO batch_test VALUES (2)")
rows = db.execute("SELECT * FROM batch_test")
raise "execute_batch FAILED: #{rows.inspect}" unless rows == [[1], [2]]
puts "11. execute_batch: OK"

# Test 12: Boolean binding
db.execute("CREATE TABLE bools (flag INTEGER)")
db.execute("INSERT INTO bools VALUES (?)", [true])
db.execute("INSERT INTO bools VALUES (?)", [false])
rows = db.execute("SELECT * FROM bools")
raise "Boolean FAILED: #{rows.inspect}" unless rows == [[1], [0]]
puts "12. Boolean binding: OK"

# Test 13: Empty result set
rows = db.execute("SELECT * FROM users WHERE id = 999")
raise "Empty result FAILED" unless rows == []
puts "13. Empty result set: OK"

# Test 14: results_as_hash
db.results_as_hash = true
rows = db.execute("SELECT id, name FROM users WHERE id = 1")
raise "results_as_hash FAILED: #{rows.inspect}" unless rows.first.is_a?(Hash)
raise "results_as_hash content FAILED" unless rows.first["id"] == 1 && rows.first["name"] == "Alice"
db.results_as_hash = false
puts "14. results_as_hash: OK"

# Test 15: Statement columns
stmt = db.prepare("SELECT id, name FROM users")
cols = stmt.columns
raise "columns FAILED: #{cols.inspect}" unless cols == ["id", "name"]
stmt.close
puts "15. Statement#columns: OK"

# Test 16: Close database
db.close
raise "close FAILED" unless db.closed?
puts "16. Database close: OK"

# Test 17: Exception on closed database
begin
  db.execute("SELECT 1")
  raise "Should have raised"
rescue SQLite3::SQLException => e
  puts "17. SQLException on closed db: OK"
end

puts "\n=== All #{17} tests passed! ==="
