{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Devel.PortManager
    ( newPortManager
    , PortManager
    , getPort
    , recyclePort
    ) where

import Control.Concurrent.STM
import Data.Conduit.Network (bindPort)
import Control.Exception (try, IOException)
import Network.Socket (sClose)

data PortManager = PortManager
    { getPort :: IO Int
    , recyclePort :: Int -> IO ()
    }

newPortManager :: IO PortManager
newPortManager = do
    available <- newTVarIO $ concat
        [ [43124..44320]
        , [28120..29166]
        , [45967..46997]
        , [28241..29117]
        , [40001..40840]
        , [29170..29998]
        , [38866..39680]
        , [43442..44122]
        , [41122..41793]
        , [35358..36000]
        ]
    used <- newTVarIO []
    return $ PortManager
        (getPort' available used)
        (recyclePort' used)

getPort' :: TVar [Int] -> TVar [Int] -> IO Int
getPort' availablePorts usedPorts = do
    port <- atomically $ do
        ps <- readTVar availablePorts
        case ps of
            p:ps' -> do
                writeTVar availablePorts ps'
                return p
            [] -> do
                ups <- readTVar usedPorts
                case reverse ups of
                    [] -> error "We've run out of ports, that's rather unusual"
                    up:ups' -> do
                        writeTVar availablePorts ups'
                        writeTVar usedPorts []
                        return up
    es <- try $ bindPort port "*"
    case es of
        Left (_ :: IOException) -> getPort' availablePorts usedPorts
        Right s -> do
            sClose s
            return port

recyclePort' :: TVar [Int] -> Int -> IO ()
recyclePort' usedPorts p = atomically $ modifyTVar usedPorts (p:)
