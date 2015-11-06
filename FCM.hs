module FCM
(randomUMatrix 
) where

import System.Random
import Data.List

data  RandomSet a  = RandomSet a StdGen
data  Vector a = Vector a deriving (Show)

getSecond (_,b) = b
getFirst (a, _) =  a

--randByMod :: (Num a) => StdGen -> (a , StdGen)
--randByMod seed = (((`mod` 10000) . getFirst . random $ seed) , (getSecond . random $ seed))

randomList:: Int->StdGen->(StdGen->(a,StdGen))->([a],StdGen)
randomList n gen randf = randomL [] n gen randf 
    where randomL acm 0 gen _ = (acm , gen) 
          randomL acm n gen randf= randomL ((getFirst (randf gen)) : acm) (n-1) (getSecond(randf gen)) randf

randomVector :: Int -> StdGen -> ([Float], StdGen)
randomVector n gen = 
	let rl = randomList n gen  (random :: StdGen -> (Int, StdGen))
	    dl = map (`mod` 10000) (getFirst rl) 
        in ((map ((/(fromIntegral (sum dl))) . fromIntegral) dl), getSecond rl) 
--addErrorOfCalc seed = (Just (random 100 seed) ) --mod (length a) 

randomUMatrixGen r c gen = randomList r gen (randomVector c)
randomUMatrix r c gen= getFirst (randomUMatrixGen r c gen)


vectSum a b =  zipWith (+) a b

--vectSumF:: (Fractional b) =>[[b]] ->[b] 
vectSumF a = foldl vectSum (take (length (head a)) [0,0..]) a

--vL:: (Fractional b) =>[[b]] ->[b] ->[b]
vL xObjects uColumn = (vectSumF (zipWith (\ a b-> map (*(b*b)) a) xObjects uColumn)) 
 --map (/ sum ( uColumn))
--zipWith (\a b -> map  (*(a*a)) b) uColumn 

--vCenters:: (Fractional b) =>[[b]] ->[[b]] ->[[b]]
vCenters uMatrix xObjects = map (vL xObjects) (transpose uMatrix) 
gl uColumn xObject = zipWith (\ a b-> map (*b) a) xObject uColumn


zipMatrixWith f a b = zipWith (zipWith f) a b

--aarr2 = [[4, 2, 1, 5],[4, 2, 1, 8]]

barr = [[2.0, 1.0, 0.0, 5.0], [2.0, 1.0, 0.0, 8.0] , [23.0,0.0,3.0,8.0],[2.0, 1.0, 9.0, 5.0], [2.0, 3.0, 0.0, 8.0] , [23.0,0.0,4.0,52.0]]
uarr = [[1,0,0],[2,1,1],[3,1,0]]
ularr = [1.0,2.0,3.0]
varr = [[3.0, 3.0, 4.0, 5.0], [2.0, 4.0, 5.0, 8.0] , [23.0,67.0,6.0,8.0]]

--test xObjects uColumn = (zipWith (\ a b-> map (*b) a) xObjects uColumn)



nextM dist vMatrix xObject kVector =  map (nV dist xObject kVector) vMatrix
   where nV dist xObject kVector vVector =  ((dist xObject vVector) / (dist xObject kVector))^2 

nextU :: (Fractional a )=>([a]->[a]->a)->[[a]]->[[a]]->[[a]]
nextU  dist vMatrix xObjects = map (nextUm dist vMatrix)  xObjects
nextUm dist vMatrix xObject = map (bbb) vMatrix
    where bbb a = 1/(sum (map ( \ b -> ((dist xObject a) / b)^2 ) (map (\ c-> (dist xObject c)) vMatrix)))

hammingDistance a b= abs (sum (zipWith (-) a b))

test =  nextM hammingDistance (vCenters [[0.4,0.5,0.6],[0.6,0.5,0.4]] barr) [4,5,3] [5,6,2] 

test2 = zipWith hammingDistance varr barr

--test3 = aaa hammingDistance [1.0,2.0,3.0,3.2] varr
test4 v 0= nextU hammingDistance barr v
test4 v n = test4  (vCenters (nextU hammingDistance barr v) barr) (n-1)


main = do
    print (vCenters uarr barr)