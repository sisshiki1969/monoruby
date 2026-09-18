# sqlite3_native.rb — monoruby's stand-in for sqlite3_native.so
#
# When monoruby resolves `require "sqlite3/X.Y/sqlite3_native"` (a .so), it
# redirects here. Everything the C extension provides is implemented in
# Rust over the bundled SQLite (`libsqlite3-src`, 3.48.0), as the
# dynamically loaded extension `ext/sqlite3` (libsqlite3_native.so): its
# `Init_sqlite3_native` builds `SQLite3::Database` and `SQLite3::Statement`
# and registers their native methods (doc/native_extension_loading.md).
#
# This file therefore holds only what the C extension defines but is
# naturally Ruby: the `Blob` class BLOB columns come back as, the constants
# the gem's own files read at load time, and the `Statement#initialize` that
# sqlite3 1.7.x expects from the extension (2.x defines its own in
# statement.rb, loaded later, which wins).

require "sqlite3_native.so"

module SQLite3
  # BLOB columns come back as this, and a String bound as one is written as
  # a BLOB (the gem's `String#to_blob` makes them).
  class Blob < String
  end

  # `version_info.rb` reads these at load time. SQLite is built into
  # monoruby, so compiled and loaded are the same version, and neither the
  # gem's packaged nor its precompiled libraries are in play.
  SQLITE_VERSION = libversion_string
  SQLITE_LOADED_VERSION = SQLITE_VERSION
  SQLITE_PACKAGED_LIBRARIES = false
  SQLITE_PRECOMPILED_LIBRARIES = false

  def self.threadsafe?
    threadsafe > 0
  end

  # `sqlite3_open_v2` flags. The gem's `constants.rb` does not define these
  # — the C extension does, from the sqlite3.h macros.
  module Constants
    module Open
      READONLY = 0x00000001
      READWRITE = 0x00000002
      CREATE = 0x00000004
      DELETEONCLOSE = 0x00000008
      EXCLUSIVE = 0x00000010
      AUTOPROXY = 0x00000020
      URI = 0x00000040
      MEMORY = 0x00000080
      MAIN_DB = 0x00000100
      TEMP_DB = 0x00000200
      TRANSIENT_DB = 0x00000400
      MAIN_JOURNAL = 0x00000800
      TEMP_JOURNAL = 0x00001000
      SUBJOURNAL = 0x00002000
      SUPER_JOURNAL = 0x00004000
      NOMUTEX = 0x00008000
      FULLMUTEX = 0x00010000
      SHAREDCACHE = 0x00020000
      PRIVATECACHE = 0x00040000
      WAL = 0x00080000
      NOFOLLOW = 0x01000000
      EXRESCODE = 0x02000000
    end
  end

  class Statement
    # sqlite3 1.7.x has `initialize` in the C extension; 2.x defines it in
    # statement.rb and that definition wins, being loaded later.
    def initialize(db, sql)
      raise ArgumentError, "prepare called on a closed database" if db.closed?

      sql = sql.encode("UTF-8") if sql && sql.encoding.to_s != "UTF-8"
      @connection = db
      @columns = nil
      @types = nil
      @remainder = prepare db, sql
    end
  end
end
