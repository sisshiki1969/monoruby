# Compatibility shim for sqlite3 resultset.rb
# For sqlite3 >= 2.0, load the original directly.
# For sqlite3 1.x, provide a fixed version that avoids `[*splat]` syntax.

# Determine which sqlite3 gem version is being used
_sqlite3_gem_dir = $LOADED_FEATURES.detect { |f| f.include?("/sqlite3") }&.then { |f|
  f[%r{.*/gems/sqlite3-[^/]+}]
}

if _sqlite3_gem_dir && _sqlite3_gem_dir.include?("sqlite3-1.")
  # sqlite3 1.x - provide fixed version
  require 'sqlite3/constants'
  require 'sqlite3/errors'

  module SQLite3
    class ResultSet
      include Enumerable

      class ArrayWithTypes < Array
        attr_accessor :types
      end

      class ArrayWithTypesAndFields < Array
        attr_writer :types
        attr_writer :fields
        def types; @types; end
        def fields; @fields; end
      end

      class HashWithTypesAndFields < Hash
        attr_writer :types
        attr_writer :fields
        def types; @types; end
        def fields; @fields; end
        def [] key
          key = fields[key] if key.is_a? Numeric
          super key
        end
      end

      def initialize db, stmt
        @db   = db
        @stmt = stmt
      end

      def reset( *bind_params )
        @stmt.reset!
        @stmt.bind_params( *bind_params )
        @eof = false
      end

      def eof?
        @stmt.done?
      end

      def next
        if @db.results_as_hash
          return next_hash
        end
        row = @stmt.step
        return nil if @stmt.done?
        row = @db.translate_from_db @stmt.types, row
        if row.respond_to?(:fields)
          row = ArrayWithTypes.new(row)
        else
          row = ArrayWithTypesAndFields.new(row)
        end
        row.fields = @stmt.columns
        row.types = @stmt.types
        row
      end

      def each
        while node = self.next
          yield node
        end
      end

      def each_hash
        while node = next_hash
          yield node
        end
      end

      def close
        @stmt.close
      end

      def closed?
        @stmt.closed?
      end

      def types
        @stmt.types
      end

      def columns
        @stmt.columns
      end

      def next_hash
        row = @stmt.step
        return nil if @stmt.done?
        row = @db.translate_from_db @stmt.types, row
        h = HashWithTypesAndFields.new
        @stmt.columns.zip(row).each do |k, v|
          h[k] = v
        end
        row = h
        row.fields = @stmt.columns
        row.types = @stmt.types
        row
      end
    end
  end
else
  # sqlite3 2.x - load the original from the gem
  # Find and load the gem's resultset.rb, skipping our shim
  _orig = $LOAD_PATH.map { |p| File.join(p, "sqlite3", "resultset.rb") }
    .select { |f| File.exist?(f) && !f.include?(".monoruby") }
    .first
  if _orig
    load _orig
  end
end
