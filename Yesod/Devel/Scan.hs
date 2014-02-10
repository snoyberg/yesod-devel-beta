module Yesod.Devel.Scan
    ( scan
    ) where

import Control.Concurrent.STM
import Yesod.Devel.Builder (NeedBuild)
import Control.Monad (forever)

scan :: TVar NeedBuild -> IO ()
scan needBuild = forever $ do
    putStrLn "Press enter to trigger a new build"
    _ <- getLine
    atomically $ writeTVar needBuild True
