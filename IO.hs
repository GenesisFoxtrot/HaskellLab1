import Data.Maybe
import Data.List.Split.Internals
import System.IO

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


showCSV c s = sCSV (maybeCSVParse c s::Maybe[[Int]])
   where sCSV (Nothing) = "File incorrect"
         sCSV (Just a) = show a 
--getListFromString :: (Read a, Show [a]) => String -> Maybe [a]
--getListFromString str = maybeRead $ "[" ++ str ++ "]"

main :: IO ()
main = do
  handle <- openFile "111.txt" ReadMode
  input <- hGetContents handle
  print (showCSV "\n" input)
  --let maybeList = getListFromString input in
    --  case maybeList of
      --    Just l  -> print (l)
        --  Nothing -> error "Неверный формат строки. Прощайте."