# :stopdoc:
# monoruby-compatible implementation of Forwardable internals.
# This replaces the CRuby version that depends on RubyVM::InstructionSequence.
module Forwardable
  def self._valid_method?(method)
    method = method.to_s
    method.match?(/\A[_a-zA-Z]\w*[?!=]?\z/)
  end

  def self._compile_method(src, file, line)
    eval(src, nil, file, line)
  end
end
