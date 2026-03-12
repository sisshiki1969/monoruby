# frozen_string_literal: true
#
# ActiveSupport::Callbacks for monoruby.
# Provides define_callbacks, set_callback, and run_callbacks.
#

module ActiveSupport
  module Callbacks
    class CallbackChain
      include Enumerable

      attr_reader :name, :config

      def initialize(name, config = {})
        @name = name
        @config = config
        @chain = []
      end

      def each(&block)
        @chain.each(&block)
      end

      def append(callback)
        @chain.push(callback)
      end

      def prepend(callback)
        @chain.unshift(callback)
      end

      def delete(callback)
        @chain.delete(callback)
      end

      def empty?
        @chain.empty?
      end
    end

    class Callback
      attr_reader :kind, :filter, :name, :options

      def initialize(name, filter, kind, options = {})
        @name = name
        @filter = filter
        @kind = kind
        @options = options
      end

      def matches?(name, filter)
        @name == name && @filter == filter
      end

      def apply(target, value = nil, &block)
        case @filter
        when Symbol
          target.send(@filter, &block)
        when Proc
          if @filter.arity == 0
            @filter.call(&block)
          else
            @filter.call(target, &block)
          end
        when String
          # Evaluate string as code (limited support)
          target.instance_eval(@filter)
        else
          if @filter.respond_to?(:call)
            @filter.call(target, &block)
          end
        end
      end

      # Check if callback should run based on :if / :unless conditions
      def should_run?(target)
        if_conditions = Array(@options[:if])
        unless_conditions = Array(@options[:unless])

        if_result = if_conditions.all? do |cond|
          case cond
          when Symbol then target.send(cond)
          when Proc then cond.call(target)
          when String then target.instance_eval(cond)
          else true
          end
        end

        unless_result = unless_conditions.none? do |cond|
          case cond
          when Symbol then target.send(cond)
          when Proc then cond.call(target)
          when String then target.instance_eval(cond)
          else false
          end
        end

        if_result && unless_result
      end
    end

    def self.included(base)
      base.extend(ClassMethods)
      base.instance_variable_set(:@_callbacks, {})
    end

    module ClassMethods
      def define_callbacks(*names)
        options = names.last.is_a?(Hash) ? names.pop : {}
        names.each do |name|
          _callbacks[name.to_sym] = CallbackChain.new(name, options)
        end
      end

      def set_callback(name, *filter_and_or_options)
        # Parse arguments: set_callback(:save, :before, :method_name, options)
        # or: set_callback(:save, :before, options) with :if/:unless
        kind = filter_and_or_options.shift  # :before, :after, :around
        options = filter_and_or_options.last.is_a?(Hash) ? filter_and_or_options.pop : {}
        filter = filter_and_or_options.first

        # If no explicit filter but a block-like usage, try options
        filter ||= options.delete(:filter)

        callback = Callback.new(name, filter, kind, options)

        chain = _callbacks[name.to_sym]
        unless chain
          raise ArgumentError, "#{name} is not a callback defined on #{self}"
        end

        if options[:prepend]
          chain.prepend(callback)
        else
          chain.append(callback)
        end
      end

      def skip_callback(name, *filter_and_or_options)
        kind = filter_and_or_options.shift
        filter = filter_and_or_options.first

        chain = _callbacks[name.to_sym]
        return unless chain

        chain.delete_if { |cb| cb.kind == kind && cb.filter == filter } if chain.respond_to?(:delete_if)
      end

      def reset_callbacks(name)
        chain = _callbacks[name.to_sym]
        if chain
          _callbacks[name.to_sym] = CallbackChain.new(name, chain.config)
        end
      end

      def _callbacks
        @_callbacks ||= {}
        # Inherit from superclass
        if superclass.respond_to?(:_callbacks)
          # Merge parent callbacks (lazy copy)
          superclass._callbacks.each do |name, chain|
            unless @_callbacks.key?(name)
              new_chain = CallbackChain.new(name, chain.config)
              chain.each { |cb| new_chain.append(cb) }
              @_callbacks[name] = new_chain
            end
          end
        end
        @_callbacks
      end
    end

    def run_callbacks(name)
      callbacks = self.class._callbacks[name.to_sym]
      unless callbacks
        return block_given? ? yield : nil
      end

      befores = []
      afters = []
      arounds = []

      callbacks.each do |cb|
        next unless cb.should_run?(self)
        case cb.kind
        when :before
          befores << cb
        when :after
          afters << cb
        when :around
          arounds << cb
        end
      end

      # Run before callbacks
      befores.each do |cb|
        result = cb.apply(self)
        if result == false && callbacks.config[:terminator]
          return false
        end
      end

      # Run around + yield + after
      if arounds.empty?
        result = yield if block_given?
      else
        # Build the around chain
        result = _run_around_callbacks(arounds, 0) { yield if block_given? }
      end

      # Run after callbacks (in reverse order)
      afters.reverse_each do |cb|
        cb.apply(self)
      end

      result
    end

    private

    def _run_around_callbacks(arounds, index, &block)
      if index >= arounds.size
        return block.call if block
        return
      end

      cb = arounds[index]
      cb.apply(self) do
        _run_around_callbacks(arounds, index + 1, &block)
      end
    end
  end
end
