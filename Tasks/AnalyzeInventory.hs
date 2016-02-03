module Tasks.AnalyzeInventory (task) where

import           Hanalyze.Phoneme
import           Tasks.Task

task :: Task
task = Task
         doTask
         (Just (MaxN . readIntParam))
         "analyzeinventory"
         "Analyzes the inventory of Finnish. Selects relevant bundles of maxn length. Default argument is maxn."


doTask :: [Flag] -> IO ()
doTask flags = do
  let maxn = case getFlag flags MaxN of
        Nothing -> 2 -- default is 2
        Just (MaxN (IntParam n)) -> n
  let relb = selectRelevantBundles finnishInventory maxn
      phonemes = map (pickByFeature finnishInventory) relb
      phnames = (map . map) phonemeName phonemes
      outputzip = zip relb phnames
  mapM_ (\zline -> putStr (show $ fst zline) >>
                   putStr ": " >>
                   mapM_ (\ph -> putStr (ph ++ " ")) (snd zline) >>
                   putStrLn "") outputzip
