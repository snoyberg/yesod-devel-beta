{-# LANGUAGE OverloadedStrings #-}
module Yesod.Devel.Runner
    ( runner
    ) where

import Yesod.Devel.Daemon (daemon)
import Yesod.Devel.Builder (LastCompile)
import Yesod.Devel.Process (RunProc, EarlyTermination (..))
import Yesod.Devel.PortManager (PortManager, getPort, recyclePort)
import Control.Monad (join)
import Control.Concurrent (threadDelay)
import Filesystem (createTree, removeFile)
import Filesystem.Path.CurrentOS (FilePath)
import Control.Concurrent.STM
import Control.Concurrent.Async (async, cancelWith)
import Control.Exception (finally, try, IOException)
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Prelude hiding (FilePath)

runner :: (Text -> IO ())
       -> PortManager
       -> Int -> TVar (Maybe Int) -> RunProc -> TVar LastCompile -> IO ()
runner log portManager mainPort reversedPort runProc lastCompile = do
    killUserProcess <- newTVarIO $ return ()
    daemon "runner" (loop killUserProcess Nothing) log
  where
    loop killUserProcess mtimestamp = do
        newTimestamp <- atomically $ do
            lc <- readTVar lastCompile
            case (mtimestamp, lc) of
                (_, Nothing) -> retry
                (Nothing, Just x) -> return x
                (Just old, Just new)
                    | old == new -> retry
                    | otherwise -> return new
        log "New compilation available, running"

        createTree "yesod-devel"
        writeFile "yesod-devel/devel-terminate" ""

        join $ atomically $ do
            x <- readTVar $ killUserProcess
            writeTVar (killUserProcess) (return ())
            writeTVar reversedPort Nothing
            return x

        threadDelay 100000
        removeFileIfExists "yesod-devel/devel-terminate"
        removeFileIfExists "dist/devel-terminate" -- compatibility
        port <- getPort portManager
        log $ "Got new listening port of " <> pack (show port)
        up <- async $ runProc "runghc"
            -- FIXME [ "-package-dbdist/package.conf.inplace"
            [ "-package-confdist/package.conf.inplace"
            , "devel.hs"
            ]
            [ ("PORT", show port)
            , ("APPROOT", "http://localhost:" ++ show mainPort)
            , ("DISPLAY_PORT", show mainPort)
            ] `finally` recyclePort portManager port

        atomically $ do
            writeTVar (killUserProcess) (cancelWith up EarlyTermination)
            writeTVar reversedPort (Just port)

        loop killUserProcess $ Just newTimestamp

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = do
    ex <- try $ removeFile fp
    case ex :: Either IOException () of
        Left _ -> return ()
        Right () -> return ()
