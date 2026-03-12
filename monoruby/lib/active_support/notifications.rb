# frozen_string_literal: true
#
# ActiveSupport::Notifications (minimal) for monoruby.
# Provides a simple pub/sub notification system.
#

module ActiveSupport
  module Notifications
    class Event
      attr_reader :name, :time, :end, :transaction_id, :payload
      attr_accessor :children

      def initialize(name, start, ending, transaction_id, payload)
        @name = name
        @time = start
        @end = ending
        @transaction_id = transaction_id
        @payload = payload
        @children = []
      end

      def duration
        if @end && @time
          (@end - @time) * 1000.0
        else
          0.0
        end
      end
    end

    class Fanout
      def initialize
        @subscribers = []
        @listeners_for = {}
      end

      def subscribe(pattern = nil, block = nil, &blk)
        subscriber = Subscriber.new(pattern, block || blk)
        @subscribers << subscriber
        @listeners_for.clear
        subscriber
      end

      def unsubscribe(subscriber_or_name)
        case subscriber_or_name
        when String
          @subscribers.reject! { |s| s.pattern == subscriber_or_name }
        else
          @subscribers.delete(subscriber_or_name)
        end
        @listeners_for.clear
      end

      def publish(name, *args)
        listeners_for(name).each do |s|
          s.call(name, *args)
        end
      end

      def listeners_for(name)
        @listeners_for[name] ||= @subscribers.select { |s| s.subscribed_to?(name) }
      end

      def listening?(name)
        listeners_for(name).any?
      end
    end

    class Subscriber
      attr_reader :pattern, :block

      def initialize(pattern, block)
        @pattern = pattern
        @block = block
      end

      def subscribed_to?(name)
        return true if @pattern.nil?
        case @pattern
        when Regexp
          @pattern === name
        when String
          @pattern == name
        else
          @pattern === name
        end
      end

      def call(*args)
        @block.call(*args)
      end
    end

    @notifier = Fanout.new

    class << self
      attr_accessor :notifier

      def subscribe(pattern = nil, block = nil, &blk)
        notifier.subscribe(pattern, block || blk)
      end

      def unsubscribe(subscriber_or_name)
        notifier.unsubscribe(subscriber_or_name)
      end

      def instrument(name, payload = {})
        start_time = Time.now
        begin
          result = yield(payload) if block_given?
        ensure
          end_time = Time.now
          notifier.publish(name, start_time, end_time, nil, payload)
        end
        result
      end

      def publish(name, *args)
        notifier.publish(name, *args)
      end

      def subscribed(callback, pattern = nil, &block)
        subscriber = subscribe(pattern, callback)
        block.call
      ensure
        unsubscribe(subscriber)
      end
    end
  end
end
