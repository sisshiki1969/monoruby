# frozen_string_literal: true
#
# ActiveRecord for monoruby.
# A minimal but functional implementation of Rails' ActiveRecord ORM.
#
# Provides:
# - Connection management with SQLite3 adapter
# - Schema/Migration DSL
# - Basic CRUD (create, read, update, delete)
# - Query builder (Relation) with chaining
# - Associations (has_many, belongs_to, has_one)
# - Scopes
# - Type casting from database columns

require 'active_support'
require 'active_model'
require 'sqlite3'

module ActiveRecord
  class RecordNotFound < StandardError; end
  class RecordNotSaved < StandardError; end
  class RecordInvalid < StandardError
    attr_reader :record
    def initialize(record = nil)
      @record = record
      if record
        super("Validation failed: %s" % [record.errors.full_messages.join(', ')])
      else
        super("Validation failed")
      end
    end
  end
  class StatementInvalid < StandardError; end
  class ConnectionNotEstablished < StandardError; end
  class RecordNotDestroyed < StandardError; end
end

require 'active_record/type'
require 'active_record/connection_adapters'
require 'active_record/schema'
require 'active_record/migration'
require 'active_record/relation'
require 'active_record/associations'
require 'active_record/base'
