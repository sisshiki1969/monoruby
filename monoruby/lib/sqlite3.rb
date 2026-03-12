# sqlite3.rb - SQLite3 database adapter for monoruby
#
# This provides an API compatible with the CRuby sqlite3 gem,
# implemented using monoruby's Fiddle module to call libsqlite3.
#
# Usage:
#   require 'sqlite3'
#   db = SQLite3::Database.new(':memory:')
#   db.execute("CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)")
#   db.execute("INSERT INTO users (name) VALUES (?)", ["Alice"])
#   rows = db.execute("SELECT * FROM users")  #=> [[1, "Alice"]]
#   db.close

require 'sqlite3/version'
require 'sqlite3/exceptions'
require 'sqlite3/ffi_bindings'
require 'sqlite3/statement'
require 'sqlite3/result_set'
require 'sqlite3/database'

module SQLite3
  # Convenience constant aliases matching the sqlite3 gem
  SQLITE_OK         = FFIBindings::SQLITE_OK
  SQLITE_ERROR      = FFIBindings::SQLITE_ERROR
  SQLITE_ROW        = FFIBindings::SQLITE_ROW
  SQLITE_DONE       = FFIBindings::SQLITE_DONE
end
