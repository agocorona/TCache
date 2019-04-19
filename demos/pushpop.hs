import Data.Persistent.Collection
import Control.Concurrent
import Data.TCache

main :: IO ()
main = do
    let q = getQRef "hi"

    -- display if and what we have in the queue
    -- will be empty on first run but afterwards
    -- contains "e"
    pickAll q >>= print

    -- make sure there is no data left
    flush q

    -- pop from empty would deadlock, so don't do that
    -- pop q >>= print

    push q "a" -- push before starting asyncs

    -- async pops (first does not need to wait)
    _ <- forkIO $ pop q >>= print
    _ <- forkIO $ pop q >>= print

    putStrLn "Waiting a bit (should print \"a\")"
    threadDelay 1000000
    putStrLn "By mpt \"a\" should be printed"

    -- push more
    push q "b" -- this will be printed asap

    push q "c"
    push q "d" -- will be sync popped
    push q "e" -- will be left over at the end

    -- let the second fork finish
    threadDelay 1000000
    putStrLn "By now \"b\" should be already printed"

    -- another async fork (printing "c")
    _ <- forkIO $ pop q >>= print

    -- sync current state to file and print it
    -- this usually still has "c" included
    threadDelay 1000000
    syncCache
    readFile ".tcachedata/Queue#hi" >>= putStrLn

    -- sync pop (usually prints "d")
    pop q >>= print

    -- another sync / wait / print
    syncCache
    threadDelay 1000000
    -- here "e" will be left in the queue on Disk
    readFile ".tcachedata/Queue#hi" >>= putStrLn
    -- and stays there for the next run
