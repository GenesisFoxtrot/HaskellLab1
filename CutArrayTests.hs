import CSV 

arr = [[1,3,4],[5,6,7]]

assert a b name = if a == b then  "Test " ++ name ++ " passed" 
	                        else  "Test"  ++ name ++ " failed"

main = do 
   print (assert (cutArray arr True False False) [[5,6,7]]     "Cut only row test")
   print (assert (cutArray arr False True False) [[3,4],[6,7]] "Cut only last column test")
   print (assert (cutArray arr False False True) [[1,3],[5,6]] "Cut only first last column test") 
   print (assert (cutArray arr True True False)  [[6,7]]       "Cut row, first column test")
   print (assert (cutArray arr False True True)  [[3],[6]]     "Cut last column, first column test")
   print (assert (cutArray arr True False True)  [[5,6]]       "Cut row, last column test")   
   print (assert (cutArray arr True True True)   [[6]]         "Cut row, first column, last column test")   


