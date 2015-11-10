import CSV 

arr = [[1,3,4],[5,6,7]]

assert a b name = if a == b then  "Test " ++ name ++ " passed" 
	                        else  "Test"  ++ name ++ " failed"

main = do 
   print (assert (maybeRead "3"::Maybe Int) (Just 3) "Just Int test")
   print (assert (maybeRead "3.14"::Maybe Int) (Nothing)  "Float String to Int test")
   print (assert (maybeRead "gsfgfsg"::Maybe Int) (Nothing)  "Letters to Int test")
   print (assert (maybeRead "3.14"::Maybe Float) (Just 3.14)  "Just Float test")
   print (assert (maybeRead "gsfgfsg"::Maybe Float) (Nothing)  "Letters to Float test")
 