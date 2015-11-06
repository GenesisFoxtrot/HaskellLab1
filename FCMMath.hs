module FCMMath
(
	
) where 


 
twoToOneArr :: Num a =>  ([a]->[a]->a)->[[a]] -> [[a]] -> [a]
    
twoToOneArr _ [] [] = []
twoToOneArr f  (x:xs) (y:ys)  =  f x y : twoToOneArr f xs ys

hammingDistance::  Num a =>  [a]->[a]->a

hammingDistance vectori vectorj = distance 0 vectori vectorj
    where 
    	distance n [] [] = n
    	distance n (xi:vectrit) (xj:vectrjt) = distance (n + xi - xj) vectrit vectrjt


evclideDistance vectori vectorj = sqrt . fromIntegral . hammingDistance vectori $ vectorj  

hd = twoToOneArr hammingDistance  