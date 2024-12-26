require "reline"

prompt = 'prompt> '
use_history = true

begin
  while true
    text = Reline.readmultiline(prompt, use_history) do |multiline_input|
      # Accept the input until `end` is entered
      multiline_input.split.last == "end"
    end

    puts 'You entered:'
    puts text
  end
# If you want to exit, type Ctrl-C
rescue Interrupt
  puts '^C'
  exit 0
end