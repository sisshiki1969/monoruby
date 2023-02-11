require 'benchmark'
def fib n
  if n < 3
    1
  else
    fib(n-1) + fib(n-2)
  end
end

Benchmark.bm do |x|
  x.report { fib 35 }
end