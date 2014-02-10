{-# LANGUAGE OverloadedStrings #-}
module Yesod.Devel.Builder
    ( builder
    , NeedBuild
    , LastCompile
    ) where

import Yesod.Devel.Daemon
import Yesod.Devel.Process
import Data.Text (Text)
import Control.Monad (unless)
import Data.Time (getCurrentTime, UTCTime)
import Control.Concurrent.STM

type NeedBuild = Bool
type LastCompile = Maybe UTCTime

builder :: RunProc
        -> (Text -> IO ())
        -> IO (TVar NeedBuild, TVar LastCompile)
builder runProc log' = do
    isConfigured <- newTVarIO False
    lastCompile <- newTVarIO Nothing
    needBuild <- newTVarIO True

    let builder' = do
            configed <- atomically $ do
                nb <- readTVar needBuild
                unless nb retry
                writeTVar needBuild False -- so that if an error occurs below, we don't loop
                readTVar isConfigured
            unless configed $ do
                runProc "cabal" ["configure", "-flibrary-only"] []
                atomically $ writeTVar isConfigured True
            runProc "cabal" ["build"] []
            now <- getCurrentTime
            atomically $ writeTVar lastCompile (Just now)

    daemon "builder" builder' log'
    return (needBuild, lastCompile)
