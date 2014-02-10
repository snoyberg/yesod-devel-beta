{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Devel.ReverseProxy
    ( reverseProxy
    ) where

import Yesod.Devel.Daemon
import Control.Concurrent.STM
import Network.HTTP.Client (withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Network.Wai (responseBuilder)
import Network.HTTP.ReverseProxy
import Data.Text (Text)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

reverseProxy :: Int -- ^ listening port
             -> Int -- ^ status port
             -> (Text -> IO ()) -- ^ log
             -> IO (TVar (Maybe Int))
reverseProxy listeningPort statusPort log' = do
    iport <- newTVarIO Nothing
    let start = withManager tlsManagerSettings
              $ run listeningPort
              . waiProxyToSettings (getWPR iport) settings
    daemon "reverse proxy" start log'
    return iport
  where
    getWPR iport _ = do
        mrp <- readTVarIO iport
        return $ case mrp of
            Nothing -> WPRResponse defResponse
            Just port -> WPRProxyDest $ ProxyDest "localhost" port

    settings = def
        { wpsOnExc = onExc
        }

    refreshHtml = renderHtmlBuilder $(shamletFile "refreshing.hamlet")
    onExc _ _ = return defResponse
    defResponse = responseBuilder
        status200
        [ ("content-type", "text/html; charset=utf-8")
        , ("Refresh", "1")
        ]
        refreshHtml
