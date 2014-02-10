{-# LANGUAGE OverloadedStrings #-}
module Yesod.Devel.Daemon
    ( daemon
    ) where

import Control.Concurrent.Async (withAsync, waitCatch, async, link)
import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Monad (forever)
import Data.Text (pack)

daemon
    :: Text -- ^ process description
    -> IO () -- ^ action
    -> (Text -> IO ()) -- ^ log errors
    -> IO ()
daemon desc action log' = do
    x <- async $ forever $ do
        res <- withAsync action waitCatch
        case res of
            Left e -> log' $ desc <> " exited early due to exception: " <> pack (show e)
            Right () -> return ()
    link x
