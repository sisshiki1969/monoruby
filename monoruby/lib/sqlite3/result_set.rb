# sqlite3/result_set.rb - SQLite3::ResultSet implementation
#
# Provides iteration over statement results.
# API compatible with the CRuby sqlite3 gem's ResultSet class.

module SQLite3
  class ResultSet
    include Enumerable

    def initialize(stmt)
      @stmt = stmt
      @eof = false
    end

    # Fetch the next row as an Array, or nil when done.
    def next
      return nil if @eof
      row = @stmt.step
      if row.nil?
        @eof = true
        nil
      else
        row
      end
    end

    # Iterate over all rows.
    def each
      while (row = self.next)
        yield row
      end
    end

    # Collect all remaining rows into an Array.
    def to_a
      rows = []
      each { |row| rows << row }
      rows
    end

    # Return column names from the underlying statement.
    def columns
      @stmt.columns
    end

    def eof?
      @eof
    end

    # Reset the result set to re-iterate.
    def reset
      @stmt.reset!
      @eof = false
    end

    def close
      @stmt.close
    end
  end
end
