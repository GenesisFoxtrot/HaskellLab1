module CSV
(maybeCSVParse,
 maybeCSVParseWithCut,
 cutArray
)where 

import Data.Maybe
import Data.List.Split.Internals

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing
    
maybeP c s =(map (maybeRead) (splitOn c s))

maybeArr :: [Maybe a] -> Maybe [a]
maybeArr arr = mA [] arr
   where mA acm [] = Just (reverse acm)
         mA acm ((Just a):xs) = mA (a:acm) xs
         mA acm (Nothing:xs) = Nothing

maybeCSVParseWithCut c s wr wfc wlc= maybeArr (map (\a ->maybeArr (map maybeRead a)) maybeOblects)
   where maybeOblects = cutArray (splitBy c s) wr wfc wlc

splitBy c s = (map (splitOn c) (splitOn "\n" s))

cutArray xObjects r fc lc = cut lc (map init) (cut fc (map tail)  (cut r tail xObjects))
  where cut b f xObjects = if b then f xObjects else xObjects


maybeParse :: (Read a) => String ->  String  -> Maybe [a]
maybeParse c s = maybeArr (map (maybeRead) (splitOn c s))          -- Not used

maybeCSVParse:: (Read a) => String->String->Maybe[[a]]
maybeCSVParse c s = maybeArr (map (maybeParse c) (splitOn "\n" s)) -- Not used