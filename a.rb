class CPU
    def add_mappings(addr)
        addr.each do |x|
            puts x
        end
    end
end

@cpu = CPU.new
@cpu.add_mappings(0x2000.step(0x3fff, 8))