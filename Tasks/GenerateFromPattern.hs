module Tasks.GenerateFromPattern (task) where

import           Control.Monad.Writer
import qualified Data.Text as Txt
import qualified Data.Text.IO as TIO
import           Hanalyze.FreqDist
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import           Hanalyze.ToUCLAP
import qualified Hanalyze.Token as T
import           System.IO
import           Tasks.Task

task :: Task
task = Task
         doTask
         (Just (FPattern . readFPattern))
         "generatefrompatt"
         "Generates words based on a pattern. The pattern must be a dot pattern. The pattern can be specified with the -p, the --pattern flags, or it can be just plainly written in place of the file name. Outputs to the stdout."

doTask :: [Flag] -> IO ()
doTask flags = do
  let mpatt = getFlag flags FPattern
      patt = case mpatt of
        Nothing -> error "No pattern given."
        Just (FPattern []) -> error "Illegal pattern."
        Just (FPattern p) -> p
  when (length patt == 0) (error "Illegal pattern.")
  when (not (isDotPattern patt)) (error "Not a dot pattern!")
  let phs = case generatePattern finnishInventory patt of
        Nothing -> []
        Just phons -> phons
      words = map spellout phs
  case getFlag flags UCLAOutput of
    Just (UCLAOutput True) -> do
      let fd = tFromList (zip words (replicate (length words) 1))
          (corp, probs) = runWriter $ convertCorpus finnishInventory fd
      TIO.putStrLn corp
      when (not (probs == Txt.empty)) (putStrLn "Problems:\n" >> TIO.putStrLn probs)
    _ -> mapM_ (T.hPutStrLn stdout) words

