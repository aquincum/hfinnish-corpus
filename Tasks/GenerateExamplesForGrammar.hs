module Tasks.GenerateExamplesForGrammar (task) where

import           Data.List (intersperse)
import           Data.Monoid
import qualified Data.Text.IO as TIO
import qualified Hanalyze.Token as T
import           Hanalyze.ToUCLAP
import           Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "examplesforgrammar"
         "Reads in a UCLA PL output grammar.txt and generates all combinations of violating Finnish segments."

doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  txt <- TIO.readFile infn
  let uclagr = readUCLAPLGrammar txt infn
  let patts = map generateExamples uclagr
      pattsStr = map (\p -> mconcat $ intersperse (T.pack ",") p) patts
  mapM_ (\(uc, wds) ->
          putStrLn (show uc ++ "\t" ++ (T.unpack wds))) (zip uclagr pattsStr)
