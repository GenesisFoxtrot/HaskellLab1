module ARGS
(containsInDict,
 argsValidate,
 argsDicionary,
 argsAny,
 argsByKey
 )
where

containsInDict a dictionary = any (\b -> b == a) dictionary

argsValidate args dictionary =foldl (&&) True (map (validateArgument) args)
  where 
   validateArgument (x:xs) = length xs == 1 && containsInDict x dictionary 
   validateArgument _ = False 

argsDicionary:: [[String]]->String-> String
argsDicionary dict key = last (head (filter (\a -> key == head a) dict))  

argsAny dict key = any (\a -> key == head a) dict

argsByKey dict key def = 
  if argsAny dict key
    then  argsDicionary dict key
    else  def
