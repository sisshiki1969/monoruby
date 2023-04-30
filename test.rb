puts RubyVM::InstructionSequence.new(DATA).disassemble

__END__

a = 0
while a < 20
  begin
    1/0
  rescue 100+10
    puts 100
  end
  a += 1
end