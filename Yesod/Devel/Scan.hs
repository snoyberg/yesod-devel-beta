module Yesod.Devel.Scan
    ( scan
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Yesod.Devel.Builder (NeedBuild)
import Control.Monad (forever, void)

scan :: TVar NeedBuild -> IO ()
scan needBuild = void $ forkIO $ forever $ do
    putStrLn "Press enter to trigger a new build"
    _ <- getLine
    atomically $ writeTVar needBuild True
