pat = /\A
  \d+ # 整数部 #{RUBY_VERSION}
  (\. # 小数点
    \d+ # 小数部
  )?  # 小数点 + 小数部 はなくともよい
\z/x

puts pat