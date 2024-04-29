50.times do |x|
  puts "#{x}: #{100 * 100}"
  if x == 5
    class Integer
      def *(other)
        42
      end
    end
  end
end
