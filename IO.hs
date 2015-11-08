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
import ARGS 

dictionary = ["input","output","classNum","accuracy","startWithRandomCenters","columSeparator", 
              "distance", "withoutFirstRow", "withoutFirstColumn", "withoutLastColumn"]

getCSV c s = sCSV (maybeCSVParse c s::Maybe[[Float]])
   where sCSV (Nothing) = error "Wrong CSV File Format"
         sCSV (Just a) = a

handlerArgs e = putStrLn "Wrong format of varebles"

handler e 
  | isDoesNotExistError e = putStrLn "File is not exist" 
  | otherwise = ioError e

argsParse:: [String]->[[String]]
argsParse args = map (splitOn ":") args

cutXObjects xObjects r fc lc = cut lc (map init) (cut fc (map tail)  (cut r tail xObjects))
  where cut b f xObjects = if b then f xObjects else xObjects

getDitance "hammingDistance" = hammingDistance
getDitance "evclideDistance" = evclideDistance
getDitance _ = error "Such distance function does not exist" 

mainAction :: IO ()
mainAction = do
  args <- getArgs
  argsP<-return (argsParse args)
  c<-if argsValidate argsP dictionary then print "Yes" else error "Wrong arguments format!" -- throw IOException 
  csvFile<-readFile (argsDicionary argsP "input")
  columSeparator <-         return (argsByKey argsP "columSeparator" "\n")
  classNum <-               return (read (argsByKey argsP "classNum" "-")::Int) --`catch` handlerArgs
  accuracy <-               return (read (argsByKey argsP "accuracy" "-")::Float)--catch
  withoutFirstRow<-         return (read (argsByKey argsP "accuracy" "False")::Bool)
  withoutFirstColumn<-      return (read (argsByKey argsP "withoutFirstColumn" "False")::Bool)
  startWithRandomCenters <- return (read (argsByKey argsP "startWithRandomCenters" "False")::Bool)
  dict <-             return (getDitance (argsByKey argsP "distance" "hammingDistance"))
  csvP <- return (getCSV columSeparator csvFile)
  xObjects <- return (cutXObjects csvP withoutFirstRow withoutFirstColumn withoutLastColumn)
  fClastering <- return (if startWithRandomCenters then clusteringFCMwithVStart else clusteringFCM)
  gen<-newStdGen 
  clusters<-return (fClastering xObjects classNum accuracy gen dict)
  output<-if argsAny argsP "output" 
    then writeFile (argsDicionary argsP "output") (show clusters) 
    else print (show clusters)
  print "End Main Action"

main = do
  mainAction `catch` handler --) :: IO (Either IOException String)
