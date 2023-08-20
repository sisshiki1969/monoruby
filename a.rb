def f
  sum = 100
  if sum < 0
    sum += 1
    begin
    end while false
  end
end

20.times { f }

puts f

#+:00000 init_method reg:2 arg:0 req:0 opt:0 rest:false block:None stack_offset:5
# :00001 %1 = 100: i32
# :00002 _%2 = %1 < 0: i16                    [<INVALID>][<INVALID>]
# :00003 condnotbr _%2 =>:00011
#+:00004 %1 = %1 + 1: i16                     [<INVALID>][<INVALID>]
#+:00005 loop_start counter=0 jit-addr=0000000000000000
# :00006 %2 = literal[false]
# :00007 condbr %2 =>:00005
#+:00008 %2 = nil
# :00009 loop_end
#+:00010 ret %2
#+:00011 %2 = nil
# :00012 ret %2