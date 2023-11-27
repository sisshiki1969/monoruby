class C
  def f
    for i in 0..9
      puts i
    end
  end
end

class D < C
end

C.new.f
D.new.f