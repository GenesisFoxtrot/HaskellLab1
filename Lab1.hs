import Prelude hiding (catch) 
import System.IO.Error hiding(try, catch, ioError)
import System.IO
import System.Environment
import System.Random
import Control.Exception
import Data.Maybe
import FCM
import CSV
import ARGS 

dictionary = ["input","output","classNum","accuracy","startWithRandomCenters","columSeparator", 
              "distance", "withoutFirstRow", "withoutFirstColumn", "withoutLastColumn"]

getCSV c s woFRow woFColumn woLColumn= sCSV (maybeCSVParseWithCut c s woFRow woFColumn woLColumn::Maybe[[Float]])
   where sCSV (Nothing) = error "ERROR: Wrong CSV File Format"
         sCSV (Just a) = a
handlerFiles s e  
  | isDoesNotExistError e = error ("ERROR: "++s++" does not exist") 
  | isAlreadyInUseError e = error ("ERROR: "++s++"file is already used ")
  | isPermissionError e = error   ("ERROR: Permission"++ s ++"file ") 
  | otherwise = ioError e 

handlerOthers:: SomeException -> IO a
handlerOthers e
  | otherwise = error ( "ERROR:" ++ (show e))

getDitance "hammingDistance" = hammingDistance
getDitance "evclideDistance" = evclideDistance
getDitance dist = error ("ERROR:"++ dist ++ " distance function does not exist")

mainAction :: IO ()
mainAction = do
  args <- getArgs
  argsP<-return (argsParse args)
  c<-if argsValidate argsP dictionary then return() else error "Error::Some wrong argument key!" 
  -------------------------------------------------------------------------------------------------
  csvFile<-   readFile (argsByKey argsP "input"                       Nothing           ) `catch` handlerFiles "input"
  cSeparator<-return   (argsByKey argsP "columSeparator"              (Just ",")        ) 
  classNum<-  return   (readArgsByKey argsP "classNum"                Nothing      ::Int)   
  accuracy<-  return   (readArgsByKey argsP "accuracy"                Nothing    ::Float) 
  woFRow<-    return   (readArgsByKey argsP "withoutFirstRow"        (Just"False")::Bool) 
  woFColumn<- return   (readArgsByKey argsP "withoutFirstColumn"     (Just"False")::Bool) 
  woLColumn<- return   (readArgsByKey argsP "withoutLastColumn"      (Just"False")::Bool) 
  startWV<-   return   (readArgsByKey argsP "startWithRandomCenters" (Just"False")::Bool) 
  dict<-      return   (getDitance (argsByKey argsP "distance"(Just"hammingDistance"  )))
  -------------------------------------------------------------------------------------------------
  xObjects <-    return (getCSV cSeparator csvFile woFRow woFColumn woLColumn)
  dddddddd<-     print xObjects
  fClastering <- return (if startWV then clusteringFCMwithVStart else clusteringFCM)
  gen<-          newStdGen 
  clusters<-     return (fClastering xObjects classNum accuracy gen dict) 
  output<-       if argsAny argsP "output" 
    then writeFile (argsDicionary argsP "output") (show clusters)`catch` handlerFiles "output" 
    else print (show clusters)
  print "End Main Action" 

main = do
  mainAction `catch` handlerOthers