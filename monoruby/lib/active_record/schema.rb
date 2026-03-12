# frozen_string_literal: true
#
# ActiveRecord::Schema - Schema definition DSL.

module ActiveRecord
  class Schema
    def self.define(info = {}, &block)
      schema = new
      schema.instance_eval(&block)
      schema
    end

    def create_table(table_name, options = {}, &block)
      connection.create_table(table_name.to_s, options, &block)
    end

    def add_column(table_name, column_name, type, options = {})
      connection.add_column(table_name.to_s, column_name.to_s, type, options)
    end

    def add_index(table_name, column_names, options = {})
      connection.add_index(table_name.to_s, column_names, options)
    end

    def drop_table(table_name, options = {})
      connection.drop_table(table_name.to_s)
    end

    def remove_column(table_name, column_name)
      connection.remove_column(table_name.to_s, column_name.to_s)
    end

    private

    def connection
      ActiveRecord::Base.connection
    end
  end
end
