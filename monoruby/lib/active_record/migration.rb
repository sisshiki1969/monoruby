# frozen_string_literal: true
#
# ActiveRecord::Migration - Basic migration DSL.

module ActiveRecord
  class Migration
    def self.migrate(direction = :up)
      instance = new
      instance.send(direction)
    end

    def up
      # Override in subclass
    end

    def down
      # Override in subclass
    end

    def create_table(table_name, options = {}, &block)
      connection.create_table(table_name.to_s, options, &block)
    end

    def drop_table(table_name, options = {})
      connection.drop_table(table_name.to_s)
    end

    def add_column(table_name, column_name, type, options = {})
      connection.add_column(table_name.to_s, column_name.to_s, type, options)
    end

    def remove_column(table_name, column_name)
      connection.remove_column(table_name.to_s, column_name.to_s)
    end

    def add_index(table_name, column_names, options = {})
      connection.add_index(table_name.to_s, column_names, options)
    end

    private

    def connection
      ActiveRecord::Base.connection
    end
  end
end
