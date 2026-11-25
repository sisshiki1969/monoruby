# frozen_string_literal: true

# Loaded by monoruby right after `error_highlight/base.rb`
# (`Executor::run_load_hooks`), whichever copy of the gem that was: the
# vendored one or a newer one a bundle brought.
#
# error_highlight finds the node a backtrace location was executing
# through `RubyVM::AbstractSyntaxTree`: `.of(location)`, which on a
# prism-compiled CRuby raises a RuntimeError naming prism, and then
# `.node_id_for_backtrace_location(location)`, whose id it looks up in
# `Prism.parse_file`'s tree. monoruby compiles with prism too, so it
# answers the same two calls.
#
# They are not a top-level `RubyVM`. Libraries take `defined?(RubyVM)`
# to mean CRuby's VM is there to be asked for instruction sequences and
# statistics, and monoruby has neither. A constant of `ErrorHighlight`
# itself is found first from the gem's code (every reference there is
# inside `module ErrorHighlight`), and a private one is invisible from
# anywhere else.
module ErrorHighlight
  module RubyVM
    module AbstractSyntaxTree
      # What `ErrorHighlight.spot(node)` tests its argument against;
      # nothing is an instance of it.
      class Node
      end

      def self.of(*, **)
        raise RuntimeError, "cannot get AST for ISEQ compiled by prism"
      end

      def self.node_id_for_backtrace_location(location)
        unless Thread::Backtrace::Location === location
          raise TypeError, "Thread::Backtrace::Location object expected"
        end
        location.__node_id
      end
    end
  end
  private_constant :RubyVM
end
