module CSV
(maybeCSVParse
)where 

import Data.Maybe
import Data.List.Split.Internals

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing

maybeParse :: (Read a) => String ->  String  -> Maybe [a]
maybeParse c s = maybeArr (map (maybeRead) (splitOn c s))
    
maybeP c s =(map (maybeRead) (splitOn c s))

maybeArr :: [Maybe a] -> Maybe [a]
maybeArr arr = mA [] arr
   where mA acm [] = Just acm
         mA acm ((Just a):xs) = mA (a:acm) xs
         mA acm (Nothing:xs) = Nothing

maybeCSVParse:: (Read a) => String->String->Maybe[[a]]
maybeCSVParse c s = maybeArr (map (maybeParse ",") (splitOn c s))