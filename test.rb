50.times do |x|
  if x == 25
    class Integer
      def *(other)
        42
      end
    end
  end
  puts "#{x}: #{100 * 100}"
end
