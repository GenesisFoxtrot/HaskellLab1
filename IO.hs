import Prelude hiding (catch) 
import System.IO.Error hiding(try, catch, ioError)
import System.IO
import System.Environment
import System.Random
import Control.Exception
import Data.Maybe
import Data.List.Split.Internals
import FCM
import CSV

getCSV c s = sCSV (maybeCSVParse c s::Maybe[[Float]])
   where sCSV (Nothing) = error "Wrong CSV File Format"
         sCSV (Just a) = a

handlerArgs e = putStrLn "Wrong format of varebles"

handler e 
  | isDoesNotExistError e = putStrLn "File is not exist" 
  | otherwise = ioError e

argsParse:: [String]->[[String]]
argsParse args = map (splitOn ":") args


dictionary = ["input", "output", "classNum", "accuracy", "startWith", "columSeparator"]
containsInDict a = any (\b -> b == a) dictionary

argsValidate args =foldl (&&) True (map (validateArgument) args)
  where 
   validateArgument (x:xs) = if length xs == 1 && containsInDict x then True else False
   validateArgument _ = False 

argsDicionary:: [[String]]->String-> String
argsDicionary dict key = last (head (filter (\a -> key == head a) dict))  

argsAny dict key = any (\a -> key == head a) dict

mainAction :: IO ()
mainAction = do
  args <- getArgs
  c<-if (argsValidate (argsParse args)) then print "Yes" else error "Wrong arguments format!" -- throw IOException 
  csvFile<-readFile (argsDicionary (argsParse args) "input")
  a<-if argsAny (argsParse args) "output" 
   then writeFile (argsDicionary (argsParse args) "output") (show (getCSV "\n" csvFile))  
   else print (show (getCSV "\n" csvFile))
  gen<-newStdGen
  la<-print (map sum (clasteringFCM (getCSV "\n" csvFile) 3 gen))
  print "End Main Action"

    


main = do
  mainAction `catch` handler --) :: IO (Either IOException String)
  
  --let maybeList = getListFromString input in
    --  case maybeList of
      --    Just l  -> print (l)
        --  Nothing -> error "Неверный формат строки. Прощайте."