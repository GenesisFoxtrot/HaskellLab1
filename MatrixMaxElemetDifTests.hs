import FCM

arr = [[1,3,4],[5,6,7]]
brr = [[1,2,3],[5,6,-7]]
crr = [[-1,-2,-3],[-5,-6,-7]]
drr = [[1.3,1.2,1.1],[1.2,1.3,1.5]]

assert a b name = if a == b then  "Test " ++ name ++ " passed" 
	                        else  "Test"  ++ name ++ " failed"

main = do 
   print (assert (matrixMaxElemDif arr brr::Float) (14)   "Negative numer test")
   print (assert (matrixMaxElemDif arr arr::Float) (0)    "Same lists test")
   print (assert (matrixMaxElemDif crr brr::Float) (12)   "Negative arr test")
   print (assert (matrixMaxElemDif arr drr::Float) (5.5)  "Float arr test")