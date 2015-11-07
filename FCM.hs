module FCM
(clusteringFCM,
 clusteringFCMwithVStart,
 hammingDistance,
 evclideDistance
) where

import System.Random
import Data.List

getSecond (_,b) = b
getFirst  (a,_) = a

randomList:: Int->StdGen->(StdGen->(a,StdGen))->([a],StdGen)
randomList n gen randf = randomL [] n gen randf 
  where randomL acm 0 gen _ = (acm , gen) 
        randomL acm n gen randf= randomL ((getFirst (randf gen)) : acm) (n-1) (getSecond(randf gen)) randf

randomVector :: Int -> StdGen -> ([Float], StdGen) -- vector with row sum = 1 
randomVector n gen =   ((map ((/(fromIntegral (sum (getFirst rl)))) . fromIntegral) (getFirst rl)), (getSecond rl)) 
  where rl = randomList n gen  (randomR (0,10000) :: StdGen -> (Int, StdGen))  

randomUMatrixGen r c gen = randomList r gen (randomVector c)
randomUMatrix r c gen= getFirst (randomUMatrixGen r c gen)

randomVMatrix:: Int -> [[Float]]->StdGen->[[Float]]
randomVMatrix n xObjects gen = randomVM [] n  xObjects gen
  where 
    randomVM acm n [] gen = acm 
    randomVM acm n (x:xs) gen = randomVM ((getFirst randomVColumn) : acm) n xs (getSecond randomVColumn)
      where randomVColumn  = randomList n gen (randomR (maximum x, maximum x)) 

vectSum a b =  zipWith (+) a b
vectSumF a = foldl vectSum (take (length (head a)) [0,0..]) a

vL xObjects uColumn = (vectSumF (zipWith (\ a b-> map (*(b*b)) a) xObjects uColumn)) 
vCenters uMatrix xObjects = map (vL xObjects) (transpose uMatrix) 

zipMatrixWith f a b = zipWith (zipWith f) a b
matrixMaxElemDif a b =maximum (map maximum (zipMatrixWith (\a b -> abs (a-b)) a b))

nextU :: (Fractional a )=>([a]->[a]->a)->[[a]]->[[a]]->[[a]]
nextU  dist vMatrix xObjects = map (nextUm dist vMatrix)  xObjects
nextUm dist vMatrix xObject = map (bbb) vMatrix
  where bbb a = 1/(sum (map ( \ b -> ((dist xObject a) / b)^2 ) (map (\ c-> (dist xObject c)) vMatrix)))

hammingDistance a b= abs . sum  $ zipWith (-) a b
evclideDistance a b= sqrt $ hammingDistance  a b

clusteringFCM xObjects n e gen dist= clutering (randomUMatrix n (length xObjects) gen) xObjects dist e
    
clusteringFCMwithVStart xObjects n e gen dist = clutering (nextU dist (randomVMatrix n xObjects gen) xObjects) xObjects dist e

clutering u xObjects dist e = if matrixMaxElemDif u iteration < e then iteration else clutering iteration xObjects dist e 
  where iteration = nextU dist (vCenters u xObjects) xObjects 
