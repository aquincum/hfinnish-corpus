module Hanalyze.Process (
  Result, Process(..),
  startProcess, endProcess, waitProcess,
  initProcess, copyProcess
  ) where

import Control.Concurrent
import Control.Monad

-- |counting the number of running processes
type Counter = MVar Int
-- |Using to block or release threads
type Blocker = MVar ()
-- |Using to store the result of a process
type Result = MVar Int

-- |A data structure with 3 MVars to represent the counter, the blocker
-- and the result of a given process
data Process = Process {getCounter :: Counter, getBlocker :: Blocker, getResult :: Result}

-- |The number of processes to run at the same time. If a new process is run and
-- the number of processes would exceed 'maxProcesses'
maxProcesses :: Int
maxProcesses = 5

debug :: String -> IO ()
debug = putStrLn


-- |Initializes a new process with new MVars all around
initProcess :: IO Process
initProcess = do
  debug "initProcess"
  cnt <- newMVar 0
  blk <- newMVar ()
  res <- newMVar 0
  return $ Process cnt blk res

-- |Copies the counter and blocker MVars to a new process, but initializes
-- a new result
copyProcess :: Process -> IO Process
copyProcess (Process c b r) = do
  debug "copyProcess"
  newres <- newMVar 0
  let pr2 = Process c b newres
  return pr2

-- |Sets the result within a process
setResult :: Int -> Process -> IO Process
setResult i pr = do
  debug "setResult"
  let res = getResult pr
  putMVar res i
  return pr

-- |Increments the counter (adding a new process)
incCounter :: Process -> IO Process
incCounter pr@(Process c _ _) = do
  debug "incCounter"
  cntval <- takeMVar c
  putMVar c (cntval + 1)
  return pr

-- |Deals with determining blockage, with incrementing the counter too
loadProcess :: Process -> IO Process
loadProcess pr@(Process c b r) = do
  debug "loadProcess"
  blocker <- takeMVar b -- WILL BE BLOCKED UNTIL GO
  cntval <- takeMVar c
  _ <- takeMVar r -- let's take out result so that we can wait for that
  let newcnt = cntval + 1
  putMVar c newcnt
  unless (newcnt >= maxProcesses) (putMVar b blocker) -- put blocker back if we're still good
  return pr

-- |Clears up counter, lets blockage go
unloadProcess :: Process -> IO Process
unloadProcess pr@(Process c b r) = do
  -- we have to put blocker if things good
  debug "unloadProcess"
  let _ = tryPutMVar b () -- put blocker back if it is not there.
  cntval <- takeMVar c
  putMVar c (cntval - 1)
  return pr


-- |Starts a process to run and registers that in the process system
startProcess :: IO  () -> Process -> IO Process
startProcess io pr = do
  debug "startProcess"
  loadProcess pr -- WILL BE BLOCKED UNTIL GO
  forkFinally io (\_ -> do
                        let _ = endProcess pr
                        return ()
                 )
  return pr

-- |Waits until result is given
waitProcess :: Process -> IO Process
waitProcess pr@(Process _ _ r) = do
  debug "waitProcess"
  res <- takeMVar r
  return pr

-- |Finishes running a process and deals with all the side effects
endProcess :: Process -> IO Process
endProcess pr = do
  debug "endProcess"
  setResult 1 pr >>= unloadProcess
