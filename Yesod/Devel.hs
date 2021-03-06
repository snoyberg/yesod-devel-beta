{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Devel where

import Yesod.Devel.PortManager
import Yesod.Devel.ReverseProxy
import Yesod.Devel.Process
import Yesod.Devel.Builder
import Yesod.Devel.Runner
import Yesod.Devel.Capture
import Yesod.Devel.Scan
import Yesod.Devel.Status
import Control.Concurrent.MVar

yesodDevel :: IO ()
yesodDevel = do
    let mainPort = 4000
        statusPort = 4001

    mexitMsg <- newEmptyMVar

    capture <- startCapture
    portManager <- newPortManager
    runProc <- getRunProc (logMessage capture) (outputChunk capture)

    (needBuild, lastCompile) <- builder runProc (logMessage capture)
    reversedPort <- reverseProxy mainPort statusPort mexitMsg
    runner (logMessage capture) portManager mainPort reversedPort runProc lastCompile
    status statusPort mexitMsg App
        { appCapture = capture
        , appReversePort = mainPort
        }

    putStrLn $ "http://localhost:" ++ show statusPort

    scan needBuild

    takeMVar mexitMsg >>= putStrLn
