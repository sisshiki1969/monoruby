class S
  def f
  end
end

class C < S
  def f
    defined?(super)
  end
end

100.times {
p C.new.f
p S.new.f
}