{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tasks.Task where

import           Data.Monoid
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import           System.Exit


-- |A task that can be run by hanalyze.
data Task = Task {
  runTask :: [Flag] -> IO (),  -- ^The IO action that takes a list of 'Flag's
  parseUnflaggedOption :: Maybe (String -> Flag), -- ^An optional function that converts an argument that is without a flag to the CLI to a flag. So if we take a file, this will return a 'FileName', or if we take a pattern it will return a 'FPattern'. It can be 'Nothing', in which case extraneous arguments are ignored.
  taskFlagString :: String, -- ^The command given to the CLI to run this task
  descriptionString :: String -- ^A description of this task
  }

instance Show Task where
  show t = "Task " ++ taskFlagString t
instance Eq Task where
  t1 == t2 = taskFlagString t1 == taskFlagString t2

-- |For my reflection trick to work!
instance Monoid Task where
  mempty = noTask
  mappend t1 t2 = Task
                    (\flags -> (runTask t1) flags >> (runTask) t2 flags)
                    Nothing -- ?
                    (taskFlagString t1 ++ ", " ++ taskFlagString t2)
                    "Concatenation of tasks."

-- |A hack so that my params are monoids.
newtype IntParam = IntParam Int deriving (Show, Eq)
instance Monoid IntParam where
  mempty = IntParam 1
  mappend (IntParam i1) (IntParam i2) = IntParam $ i1+i2

readIntParam :: String -> IntParam
readIntParam s = let i = read s :: Int in IntParam i

-- |A hack so that my params are monoids.
newtype BoolParam = BoolParam Bool deriving (Show, Eq)
instance Monoid BoolParam where
  mempty = BoolParam False
  mappend (BoolParam b1) (BoolParam b2) = BoolParam $ b1 && b2


data FFileType = FFreqDist | FSummaryTable deriving (Show, Eq)
instance Monoid FFileType where
  mempty = FFreqDist
  mappend _ _ = FFreqDist

data Flag = TaskFlag Task
          | MaxN IntParam
          | Capabilities IntParam
          | SampleNone IntParam
          | SamplePatt IntParam
          | UCLAOutput BoolParam
          | FileName FilePath
          | FPattern [Pattern]
          | FileType FFileType
          | FlagNoop
            deriving (Show, Eq)

noTask :: Task
noTask = Task (\_ -> putStrLn "No task run.") Nothing "none" "This task does not do anything!"

-- |Ugly hack, can't Typeable yet.
--
-- >>> showConstructorName (MaxN 5)
-- "MaxN"
showConstructorName :: Show a => a -> String
showConstructorName = head . words . show

-- |Ugly hack too, but works.
--
-- >>> showConstructorNamePlain MaxN
-- "MaxN"
showConstructorNamePlain :: (Show a, Monoid a) => (a -> Flag) -> String
showConstructorNamePlain c = showConstructorName $ c mempty 

-- |Get any flag based on a constructor '(a -> Flag)', thanks
-- to my ugly 'showConstructorNamePlain' trick.
--
-- >>> getFlag flags MaxN
-- Just (MaxN 5)
getFlag :: (Show a, Monoid a) => [Flag] -> (a -> Flag) -> Maybe Flag
getFlag [] _ = Nothing
getFlag (f:fs) constr = if showConstructorName f == showConstructorNamePlain constr
                        then Just f
                        else getFlag fs constr

-- |Get all the arguments that are constructed by the  constructor
--
-- >>> getAllFlags flags FileName
-- ["this.txt", "that.txt"]
getAllFlags :: (Show a, Monoid a) => [Flag] -> (a -> Flag) -> [Flag]
getAllFlags [] _ = []
getAllFlags (f:fs) constr = if showConstructorName f == showConstructorNamePlain constr
                        then f:(getAllFlags fs constr)
                        else getAllFlags fs constr


dieIfNoFN :: Maybe Flag -> IO FilePath
dieIfNoFN Nothing = putStrLn "No filename given!" >> exitFailure
dieIfNoFN (Just (FileName fp)) = return fp
dieIfNoFN _ =  putStrLn "Error in dieIfNoFN." >> exitFailure


-- |Wrapper around 'Hanalyze.Pattern.readPattern', converting a 'Maybe' to a list,
-- so if the parse fails, it returns []
readFPattern :: String -> [Pattern]
readFPattern s = case readPattern finnishInventory (T.pack s) of
  Nothing -> []
  Just p -> p

-- |Reader for file type. Anything but @summarytable@ will result in 'FFreqDist'.
readFFileType :: String -> FFileType
readFFileType s = case s of
  "summarytable" -> FSummaryTable
  _ -> FFreqDist
