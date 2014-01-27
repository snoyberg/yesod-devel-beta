{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding (log, reverse)
import Data.Typeable (Typeable)
import System.IO (hClose)
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import System.Process
import Control.Concurrent (threadDelay)
import Filesystem (createTree, removeFile)
import Control.Concurrent.Async
import Control.Exception (Exception, throwIO, try, IOException, finally)
import Network.Socket (sClose)
import Data.Conduit.Network (bindPort)
import qualified Data.ByteString as S
import System.Exit (ExitCode (..))
import Control.Concurrent.STM
import Data.MonoTraversable
import Data.Sequences
import Data.Semigroup (Semigroup (..))
import Data.Time
import Data.Text (Text)
import Control.Monad
import Control.Applicative
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnvironment)
import Network.HTTP.Conduit (Manager, newManager, conduitManagerSettings)
import Network.HTTP.ReverseProxy
import Data.FileEmbed (embedFile)
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)

data Status = Status
    { isConfigured :: !(TVar Bool)
    , logMessages :: !(TChan (UTCTime, Text))
    , needBuild :: !(TVar Bool)
    , standardOutput :: !(TChan S.ByteString)
    , standardError :: !(TChan S.ByteString)
    , lastCompile :: !(TVar (Maybe UTCTime))
    , killUserProcess :: !(TVar (IO ()))
    , availablePorts :: !(TVar [Int])
    , usedPorts :: !(TVar [Int])
    , environment :: ![(String, String)]
    , mainPort :: !Int
    , reversedPort :: !(TVar (Maybe Int))
    , manager :: !Manager
    }

newStatus :: IO Status
newStatus = Status
    <$> newTVarIO False
    <*> atomically newTChan
    <*> newTVarIO True
    <*> atomically newTChan
    <*> atomically newTChan
    <*> newTVarIO Nothing
    <*> newTVarIO (return ())
    <*> newTVarIO (concat
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
            ])
    <*> newTVarIO []
    <*> getEnvironment
    <*> return 3000
    <*> newTVarIO Nothing
    <*> newManager conduitManagerSettings

getPort :: Status -> IO Int
getPort s = do
    port <- atomically $ do
        ps <- readTVar $ availablePorts s
        case ps of
            p:ps' -> do
                writeTVar (availablePorts s) ps'
                return p
            [] -> do
                ups <- readTVar $ usedPorts s
                case reverse ups of
                    [] -> error "We've run out of ports, that's rather unusual"
                    up:ups' -> do
                        writeTVar (availablePorts s) ups'
                        writeTVar (usedPorts s) []
                        return up
    es <- try $ bindPort port "*"
    case es of
        Left (_ :: IOException) -> getPort s
        Right s -> do
            sClose s
            return port

recyclePort :: Status -> Int -> IO ()
recyclePort s p = atomically $ modifyTVar (usedPorts s) (p:)

log :: Status -> Text -> IO ()
log s msg = do
    now <- getCurrentTime
    atomically $ writeTChan (logMessages s) (now, msg)

main :: IO ()
main = do
    status <- newStatus

    forkRunRestart "builder" builder status
    forkRunRestart "runner" runner status
    forkRunRestart "reverse proxy" reverseProxy status

    _ <- async $ forever $ atomically (readTChan $ logMessages status) >>= print
    _ <- async $ forever $ atomically (readTChan $ standardOutput status) >>= S.putStr
    _ <- async $ forever $ atomically (readTChan $ standardError status) >>= S.putStr
    forever $ do
        putStrLn "Press enter to trigger a new build"
        _ <- getLine
        atomically $ writeTVar (needBuild status) True

forkRunRestart
    :: Text -- ^ process description
    -> (Status -> IO ())
    -> Status
    -> IO ()
forkRunRestart desc action s = do
    x <- async $ forever $ do
        res <- withAsync (action s) waitCatch
        case res of
            Left e -> log s $ desc <> " exited early due to exception: " <> fromList (show e)
            Right () -> return ()
    link x

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

runProc :: Status
        -> String -- ^ command
        -> [String] -- ^ args
        -> [(String, String)] -- ^ extra env
        -> IO ()
runProc status cmd args extraEnv = do
    let cp = (proc cmd args)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            , env = Just $ extraEnv ++ environment status
            }
    let desc = fromList $ show $ cmd : args
    log status $ "Running " <> desc
    (Just stdin, Just stdout, Just stderr, ph) <- createProcess cp
    hClose stdin
    connect stdout (standardOutput status)
    connect stderr (standardError status)
    eec <- try $ waitForProcess ph
    case eec of
        Left EarlyTermination -> do
            log status $ "Performing early termination on: " <> desc
            interruptProcessGroupOf ph
            terminateProcess ph
            ec <- waitForProcess ph
            log status $ "Early termination complete for " <> desc
        Right ExitSuccess -> log status $ "Process exited successfully: " <> desc
        Right (ExitFailure i) -> do
            log status $ "Process failed: " <> desc
            throwIO $ RunException cmd args i
  where
    connect handle chan = CB.sourceHandle handle $$ removeLoading =$ sinkChan chan
    sinkChan chan = CL.mapM_ $ atomically . writeTChan chan
    removeLoading = CB.lines
                =$= CL.filter (not . ("Loading package " `isPrefixOf`))
                =$= CL.map (<> "\n")

builder :: Status -> IO ()
builder status = do
    configed <- atomically $ do
        nb <- readTVar $ needBuild status
        unless nb retry
        writeTVar (needBuild status) False -- so that if an error occurs below, we don't loop
        readTVar $ isConfigured status
    unless configed $ do
        runProc status "cabal" ["configure", "-flibrary-only"] []
        atomically $ writeTVar (isConfigured status) True
    runProc status "cabal" ["build"] []
    now <- getCurrentTime
    atomically $ writeTVar (lastCompile status) (Just now)

runner :: Status -> IO ()
runner status =
    loop Nothing
  where
    loop mtimestamp = do
        newTimestamp <- atomically $ do
            lc <- readTVar $ lastCompile status
            case (mtimestamp, lc) of
                (_, Nothing) -> retry
                (Nothing, Just x) -> return x
                (Just old, Just new)
                    | old == new -> retry
                    | otherwise -> return new
        log status "New compilation available, running"

        createTree "yesod-devel"
        writeFile "yesod-devel/devel-terminate" ""

        join $ atomically $ do
            x <- readTVar $ killUserProcess status
            writeTVar (killUserProcess status) (return ())
            writeTVar (reversedPort status) Nothing
            return x

        threadDelay 100000
        removeFile "yesod-devel/devel-terminate"
        port <- getPort status
        log status $ "Got new listening port of " <> fromList (show port)
        up <- async $ runProc status "runghc"
            -- FIXME [ "-package-dbdist/package.conf.inplace"
            [ "-package-confdist/package.conf.inplace"
            , "devel.hs"
            ]
            [ ("PORT", show port)
            , ("APPROOT", "http://localhost:" ++ show (mainPort status))
            , ("DISPLAY_PORT", show $ mainPort status)
            ] `finally` recyclePort status port

        atomically $ do
            writeTVar (killUserProcess status) (cancelWith up EarlyTermination)
            writeTVar (reversedPort status) (Just port)

        loop $ Just newTimestamp

reverseProxy :: Status -> IO ()
reverseProxy status = do
    run (mainPort status) $ waiProxyToSettings getWPR settings (manager status)
  where
    getWPR _ = do
        mrp <- atomically $ readTVar $ reversedPort status
        return $ case mrp of
            Nothing -> WPRResponse defResponse
            Just port -> WPRProxyDest $ ProxyDest "localhost" port

    settings = def
        { wpsOnExc = onExc
        }

    refreshHtml = L.fromChunks [$(embedFile "refreshing.html")]
    onExc _ _ = return defResponse
    defResponse = responseLBS
        status200
        [ ("content-type", "text/html; charset=utf-8")
        , ("Refresh", "1")
        ]
        refreshHtml
