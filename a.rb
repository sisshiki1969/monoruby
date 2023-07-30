$res = 0

class S
    def f
        $res += 100
    end
end

class C < S
    def f
        3.times do
            super
        end
    end
end

C.new.f