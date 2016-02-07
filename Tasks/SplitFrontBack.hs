module Tasks.SplitFrontBack (taskfb, taskcut) where

import           Hanalyze.FreqDist
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import           Hanalyze.Vowels
import           Tasks.Task

taskfb :: Task
taskfb = Task
         (doTask False)
         (Just FileName)
         "splitfrontback"
         "Splits a freqdist into a front-neutral and a back-neutral file. The files will be saved with the same name, with a _front and a _back suffix"

taskcut :: Task
taskcut = Task
         (doTask True)
         (Just FileName)
         "splitcut"
         "Splits a freqdist into a front-neutral and a back-neutral file, and also cuts down the freqdist into disyllables only. The files will be saved with the same name, with a _front_cut and a _back_cut suffix"

-- First 'Bool' argument: cut to disyllables or not?
doTask :: Bool -> [Flag] -> IO ()
doTask cut flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  fd <- readFreqDist infn
  let front = filterTable (\t -> harmonicity t == FrontNeutral) fd
      back = filterTable (\t -> harmonicity t == BackNeutral) fd
      (front', back') = if cut then cutFDs front back flags else (front, back)
  saveTable front' (infn ++ "_front" ++ if cut then "_cut" else "")
  saveTable back' (infn ++ "_back" ++ if cut then "_cut" else "")

cutFDs :: FreqDist -> FreqDist -> [Flag] -> (FreqDist, FreqDist)
cutFDs front back flags =
  let patt = case readPattern (theInventory flags) (T.pack "{+consonantal}*{-consonantal}.{-consonantal}*{+consonantal}*") of
        Nothing -> error "readpattern"
        Just p -> p
      takeUntil2ndV tok = case segment (theInventory flags) tok of
        Nothing -> T.pack ""
        Just seg -> case matchWord seg patt of
          Nothing -> T.pack "no match"
          Just matched -> spellout matched
      front' = tMap takeUntil2ndV front 
      back' = tMap takeUntil2ndV back
  in
   (front', back')
       
