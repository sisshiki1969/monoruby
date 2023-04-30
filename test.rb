 def f
 	5.times do |x|
 		next    # ブロックを抜ける
 		break   # ブロックを呼んだメソッド（times）を抜ける
 		return  # ブロックが所属するメソッド（f）を抜ける
 	end
 end