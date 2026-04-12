# :stopdoc:
module Forwardable
  def self._valid_method?(method)
    method = method.to_s
    # Simple validation: check if the method name looks like a valid Ruby method name
    return true if /\A[a-zA-Z_]\w*[?!=]?\z/ =~ method
    # Also allow operator methods
    return true if %w[+ - * / % ** == != < > <= >= <=> === & | ^ ~ << >> [] []= +@ -@].include?(method)
    false
  end

  def self._compile_method(src, file, line)
    eval(src, nil, file, line)
  end
end
