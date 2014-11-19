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

-- |Initializes a new process with new MVars all around
initProcess :: IO Process
initProcess = do
  cnt <- newMVar 0
  blk <- newMVar ()
  res <- newMVar 0
  return $ Process cnt blk res

-- |Copies the counter and blocker MVars to a new process, but initializes
-- a new result
copyProcess :: Process -> IO Process
copyProcess pr = do
  newres <- newMVar 0
  let pr2 = Process (getCounter pr) (getBlocker pr) newres
  return pr2

-- |Sets the result within a process
setResult :: Process -> Int -> IO Process
setResult pr i = do
  let res = getResult pr
  putMVar res i
  return pr

-- |Increments the counter (adding a new process)
incCounter :: Process -> IO Process
incCounter pr = do
  cntval <- takeMVar $ getCounter pr
  putMVar (getCounter pr) (cntval + 1)
  return pr

-- |Deals with determining blockage, with incrementing the counter too
loadProcess :: Process -> IO Process
loadProcess pr = do
  blocker <- takeMVar $ getBlocker pr -- WILL BE BLOCKED UNTIL GO
  cntval <- takeMVar $ getCounter pr
  let newcnt = cntval + 1
  putMVar (getCounter pr) newcnt
  unless (newcnt >= maxProcesses) (putMVar (getBlocker pr) blocker) -- put blocker back if we're still good
  return pr

-- |Clears up counter, lets blockage go
unLoadProcess :: Process -> IO Process
unLoadProcess pr = do
  -- we have to put blocker if things good
  let _ = tryPutMVar (getBlocker pr) () -- put blocker back if it is not there.
  cntval <- takeMVar $ getCounter pr
  putMVar (getCounter pr) (cntval - 1)
  return pr


-- |Starts a process to run and registers that in the process system
startProcess :: Process -> IO () -> IO Process
startProcess pr io = do
  loadProcess pr -- WILL BE BLOCKED UNTIL GO
  forkFinally io (\_ -> do
                        let _ = endProcess pr
                        return ()
                 )
  return pr

-- |Finishes running a process and deals with all the side effects
endProcess pr = do
  setResult pr 1
--  decCounter pr

-- |Outside API: runs a new process
runProcess blocker counter io = do
  return ()
