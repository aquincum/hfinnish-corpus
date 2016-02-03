module Main where

import System.Environment
import Tasks.Tasks

main :: IO ()
main = do
  args <- getArgs
  flags <- compileOptions args
  case getFlag flags TaskFlag of
    Nothing -> runTask helpTask flags
    Just (TaskFlag t) -> runTask t flags
