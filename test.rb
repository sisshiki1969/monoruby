 def f
 	eval("a = 42", $b)
 end
 
 $b = binding
 
 a = 100
 f
 puts a