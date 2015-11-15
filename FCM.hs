module FCM
(clusteringFCM,
 clusteringFCMwithVStart,
 hammingDistance,
 evclideDistance,
 randomUMatrix,
 randomVMatrix,
 vCenters,
 nextU,
 matrixMaxElemDif
) where

import System.Random
import Data.List

--Random section---------------------------------------------------------------------
randomList:: Int->StdGen->(StdGen->(a,StdGen))->([a],StdGen)
randomList n gen randf = randomL [] n gen randf 
  where randomL acm 0 gen _ = (acm , gen) 
        randomL acm n gen randf= randomL ((fst (randf gen)) : acm) (n-1) (snd (randf gen)) randf

randomVector :: Int -> StdGen -> ([Float], StdGen) -- vector with row sum = 1 
randomVector n gen =   ((map ((/(fromIntegral (sum (fst rl)))) . fromIntegral) (fst rl)), (snd rl)) 
  where rl = randomList n gen  (randomR (0,10000) :: StdGen -> (Int, StdGen))  

randomUMatrixGen r c gen = randomList r gen (randomVector c)
randomUMatrix r c gen= fst (randomUMatrixGen r c gen)

randomVMatrix::  [[Float]]->Int ->StdGen->[[Float]]
randomVMatrix xObjects n gen = randomVM [] n xObjects gen
  where 
    randomVM acm n [] gen = acm 
    randomVM acm n (x:xs) gen = randomVM ((fst randomVColumn) : acm) n xs (snd randomVColumn)
      where randomVColumn  = randomList n gen (randomR (maximum x, maximum x)) 
-----------------------------------------------------------------------------------
vCenters uMatrix xObjects = map (vL xObjects) (transpose uMatrix) 
 where 
  vL xObjects uColumn = map (/(sum uColumn)) (vectSumF (zipWith (\ a b-> map (*(b*b)) a) xObjects uColumn))  
  vectSumF a = foldl1 (zipWith (+))  a

nextU :: (Fractional a )=>([a]->[a]->a)->[[a]]->[[a]]->[[a]]
nextU  dist vMatrix xObjects = map (nextUm)  xObjects
 where 
  nextUm xObject = map (mCalc) vMatrix 
   where
    mCalc a = 1/(sum (map ( \ b -> ((dist xObject a) / b)^2 ) (map (dist xObject) vMatrix)))

zipMatrixWith f a b = zipWith (zipWith f) a b
matrixMaxElemDif a b =maximum (map maximum (zipMatrixWith (\a b -> abs (a-b)) a b))

hammingDistance a b= abs . sum  $ zipWith (-) a b
evclideDistance a b= sqrt $ hammingDistance  a b

clusteringFCM xObjects n e gen dist= validation n e xObjects (clutering (randomUMatrix (length xObjects) n gen) xObjects dist e)
clusteringFCMwithVStart xObjects n e gen dist = validation n e xObjects (clutering (nextU dist (randomVMatrix xObjects n gen) xObjects) xObjects dist e)

clutering u xObjects dist e = 
  if (matrixMaxElemDif u iteration :: Float) < e 
    then iteration 
    else clutering iteration xObjects dist e 
  where iteration = nextU dist (vCenters u xObjects) xObjects 
 
validation n e xObjects f = if n <= 0 && e<=0 then error "ERROR:e and n must be bigger than 0"
                                              else if n > length xObjects then error  "ERROR:n must be less than objects count"
                       	                                                  else f