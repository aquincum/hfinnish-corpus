{-# LANGUAGE NullaryTypeClasses #-}

module Tasks.Tasks where

import Tasks.Options

data Task = Task {
  runTask :: [Flag] -> IO (),
  taskFlagString :: String
  }

tasks :: [Task]
tasks = undefined
--  [ testTask ]







