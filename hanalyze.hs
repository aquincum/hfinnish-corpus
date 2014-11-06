import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import Control.Applicative

data HarmonyV = Front | Neutral | Back deriving (Show, Eq)
data HarmonyW = Anything | AllFront | AllBack | BackNeutral | FrontNeutral | FrontBack | AllNeutral deriving (Show, Eq)
data Suffixing = BackSuffixes | FrontSuffixes deriving (Show, Eq)
type Token = String
type FreqDist = Map.Map Token Integer

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]

fdEmpty :: FreqDist
fdEmpty = Map.empty

loadfile :: FilePath -> IO [Token]
loadfile fn = do
  contents <- readFile fn
  let tokens = words contents in
    return tokens

readCountFreqs :: FilePath -> IO FreqDist
readCountFreqs fn = fmap countFreqs (loadfile fn)

multiReadCountFreqs :: [FilePath] -> IO FreqDist
multiReadCountFreqs fns = foldl (\x y -> Map.unionWith (+) <$> x <*> y) (pure Map.empty) (fmap readCountFreqs fns)

-- the whaaat!?
-- first, we read in the files to separate FreqDists. This is (fmap readCountFreqs fns).
-- this is a [FreqDist]. Now we make a fold over this. The initial accumulator is pure
-- Map.empty :: IO FreqDist, and the folding function merges the accumulator with the 
-- new FreqDist with unionWith: if a key is found in both maps, they are summed over.
-- Inside the lambda: Map.unionWith (+) takes 2 maps, therefore it is used in an applicative
-- manner, as x and y are IO monads. Vagy mi.

writeCountFreqs :: FreqDist -> FilePath -> Maybe Handle -> IO ()
writeCountFreqs fd _ _ 
  | fd == fdEmpty = return ()
writeCountFreqs fd fn Nothing = do
  handle <- openFile fn WriteMode
  writeCountFreqs fd fn $ Just handle
  hClose handle
  return ()
writeCountFreqs fd fn (Just handle) = let ((mkey,mval),fd2) = Map.deleteFindMax fd in do
    hPutStrLn handle (mkey ++ "\t" ++ show mval)
    writeCountFreqs fd2 fn $ Just handle

doWriteCountFreqs :: FilePath -> IO ()
doWriteCountFreqs fn = readCountFreqs fn >>= (\fd -> writeCountFreqs fd "out.txt" Nothing)

saveCountFreqs :: (IO FreqDist) -> FilePath -> IO ()
saveCountFreqs fd fn = fd >>= (\x -> writeCountFreqs x fn Nothing)


  
addPlusToken :: FreqDist -> Token -> FreqDist
addPlusToken fd tok = Map.insertWith (+) tok 1 fd
  
  
countFreqs :: [Token] -> FreqDist
countFreqs = foldl addPlusToken fdEmpty


  
harmonyV :: Char -> Maybe HarmonyV
harmonyV c 
  | c `elem` "aou" = Just Back
  | c `elem` "ei" = Just Neutral
  | c `elem` "äöü" = Just Front
  | otherwise = Nothing

fullHarmonic :: Token -> HarmonyV -> Bool
fullHarmonic str harm = foldl (\acc x -> if harmonyV x `elem` [Just harm, Nothing] then (acc && True) else False) True str

harmonicity :: Token -> HarmonyW
harmonicity "" = Anything
harmonicity (x:xs)
  | harmonyV x == Just Back = case sofar of AllFront -> FrontBack
                                            BackNeutral -> BackNeutral
                                            FrontNeutral -> FrontBack
                                            AllNeutral -> BackNeutral
                                            _ -> AllBack
  | harmonyV x == Just Front = case sofar of AllBack -> FrontBack
                                             BackNeutral -> FrontBack
                                             FrontNeutral -> FrontNeutral
                                             AllNeutral -> FrontNeutral
                                             _ -> AllFront
  | harmonyV x == Just Neutral = case sofar of AllBack -> BackNeutral
                                               BackNeutral -> BackNeutral
                                               FrontNeutral -> FrontNeutral
                                               AllFront -> AllFront
                                               _ -> AllNeutral
  | otherwise = sofar
  where sofar = harmonicity xs

suffixIt :: HarmonyW -> Suffixing
suffixIt w = if w `elem` [AllFront, FrontNeutral, AllNeutral] then FrontSuffixes else BackSuffixes
