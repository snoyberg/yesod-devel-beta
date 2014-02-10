module Yesod.Devel.Capture
    ( Capture
    , startCapture
    , logMessage
    , outputChunk
    , waitCaptured
    ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Concurrent.STM
import Data.Time (UTCTime, getCurrentTime)
import Control.Applicative ((<$>), (<|>))

data Capture = Capture
    { logMessage :: Text -> IO ()
    , outputChunk :: ByteString -> IO ()
    , waitCaptured :: STM (Either (UTCTime, Text) ByteString)
    }

startCapture :: IO Capture
startCapture = do
    logMessages <- atomically newTChan
    chunks <- atomically newTChan
    return Capture
        { logMessage = \msg -> do
            now <- getCurrentTime
            atomically $ writeTChan logMessages (now, msg)
        , outputChunk = atomically . writeTChan chunks
        , waitCaptured = (Left <$> readTChan logMessages) <|>
                         (Right <$> readTChan chunks)
        }
