if File::ALT_SEPARATOR
  /[#{SEPARATOR_LIST}]/
else
  /#{Regexp.quote File::SEPARATOR}/
end