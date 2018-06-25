import Control.Concurrent.STM

type Semaphore = TVar Bool

p :: Semaphore -> STM ()
p sem = do {
    b <- readTVar sem;
    if b
    then writeTVar sem False
    else retry
}

v :: Semaphore -> STM ()
v sem = writeTVar sem True