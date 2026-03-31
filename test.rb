	def foo(&block)
  ["a","b"].each { |e| block.call(e) }
  #["a","b"].each { |e| yield e }  # => OK
	end
	a = []
	foo { |x| a.push(x) }  # => NoMethodError: undefined method 'push' for 0
	# 'a' が外部スコープの配列ではなく 0 (Integer) として見える
	puts a.inspect  # => []