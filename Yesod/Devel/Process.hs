{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Devel.Process
    ( RunProc
    , getRunProc
    , EarlyTermination (..)
    ) where

import Prelude hiding (log)
import Data.Monoid ((<>))
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.ByteString (isPrefixOf)
import Control.Exception (Exception, throwIO, try)
import Data.Typeable (Typeable)
import System.Process (terminateProcess, waitForProcess, interruptProcessGroupOf, createProcess, StdStream (CreatePipe), proc, std_in, std_out, std_err, create_group, env)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import System.Environment (getEnvironment)

type RunProc = String -- ^ command
            -> [String] -- ^ args
            -> [(String, String)] -- ^ extra env
            -> IO ()

getRunProc :: (Text -> IO ()) -- ^ log
           -> (ByteString -> IO ()) -- ^ capture
           -> IO RunProc
getRunProc log' capture = do
    environment <- getEnvironment
    return $ runProc' environment log' capture

runProc' :: [(String, String)] -- ^ default environment
         -> (Text -> IO ()) -- ^ log
         -> (ByteString -> IO ()) -- ^ capture
         -> String -- ^ command
         -> [String] -- ^ args
         -> [(String, String)] -- ^ extra env
         -> IO ()
runProc' environment log capture cmd args extraEnv = do
    let cp = (proc cmd args)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            , env = Just $ extraEnv ++ environment
            }
    let desc = pack $ show $ cmd : args
    log $ "Running " <> desc
    (Just stdin, Just stdout, Just stderr, ph) <- createProcess cp
    hClose stdin
    connect stdout
    connect stderr
    eec <- try $ waitForProcess ph
    case eec of
        Left EarlyTermination -> do
            log $ "Performing early termination on: " <> desc
            interruptProcessGroupOf ph
            terminateProcess ph
            ec <- waitForProcess ph
            log $ "Early termination complete for " <> desc
        Right ExitSuccess -> log $ "Process exited successfully: " <> desc
        Right (ExitFailure i) -> do
            log $ "Process failed: " <> desc
            throwIO $ RunException cmd args i
  where
    connect handle = CB.sourceHandle handle $$ removeLoading =$ CL.mapM_ capture
    removeLoading = CB.lines
                =$= CL.filter (not . ("Loading package " `isPrefixOf`))
                =$= CL.map (<> "\n")

data RunException = RunException String [String] Int
    deriving Typeable
instance Exception RunException
instance Show RunException where
    show (RunException cmd args ec) = concat
        [ "Error running command "
        , cmd
        , " with arguments: "
        , show args
        , ". Exit code was: "
        , show ec
        ]

data EarlyTermination = EarlyTermination
    deriving (Show, Typeable)
instance Exception EarlyTermination
