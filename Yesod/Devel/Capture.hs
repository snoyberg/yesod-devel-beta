{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString as S

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
        , outputChunk = atomically
                      . writeTChan chunks
                      . stripColorCodes
        , waitCaptured = (Left <$> readTChan logMessages) <|>
                         (Right <$> readTChan chunks)
        }
  where
    wdel = 27
    wm = 109
    stripColorCodes bs =
        case S.breakByte wdel bs of
            (_, "") -> bs
            (x, S.drop 1 . snd . S.breakByte wm -> y)
                | S.null y -> bs
                | otherwise -> S.append x $ stripColorCodes y
