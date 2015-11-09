module ARGS
(argsParse,
 containsInDict,
 argsValidate,
 argsDicionary,
 argsAny,
 argsByKey,
 readArgsByKey,
 readE
)
where

import Data.List.Split.Internals

argsParse args = map (splitOn ":") args

containsInDict a dictionary = any (\b -> b == a) dictionary

argsValidate args dictionary =foldl (&&) True (map (validateArgument) args)
  where 
   validateArgument (x:xs) = length xs == 1 && containsInDict x dictionary 
   validateArgument _ = False 

argsDicionary:: [[String]]->String-> String
argsDicionary dict key = last (head (filter (\a -> key == head a) dict))  

argsAny dict key = any (\a -> key == head a) dict

argsByKey dict key Nothing    = argsByKeyF dict key (error ("Has no "++key++" argumet"))
argsByKey dict key (Just def) = argsByKeyF dict key def
argsByKeyF dict key f= if argsAny dict key then  argsDicionary dict key else f 

readArgsByKey:: (Read a)=> [[String]]->String->Maybe String->a
readArgsByKey dict key def = readE (argsByKey dict key def) key 

readE s key = case reads s of
                  [(x,"")]    ->  x
                  _           ->  error (key++" has wrong format")