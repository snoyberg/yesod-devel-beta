{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Yesod.Devel.Status
    ( status
    , App (..)
    , Widget
    , resourcesApp
    ) where

import Yesod.Devel.Daemon
import Yesod.Devel.Capture
import Yesod.Core
import Yesod.EventSource
import Control.Monad (forever)
import Control.Concurrent.STM (atomically)
import Data.Conduit (yield)
import Network.Wai.EventSource.EventStream (ServerEvent (ServerEvent, CommentEvent))
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Blaze.ByteString.Builder (fromLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow, fromText)
import Data.Monoid ((<>))
import Data.Aeson (encode)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (void)

data App = App
    { appCapture :: !Capture
    , appReversePort :: !Int
    }
instance Yesod App where
    defaultLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        giveUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle p}
                    <link rel=stylesheet href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css">
                    ^{pageHead p}
                <body>
                    <div class="container">
                        <h1>#{pageTitle p}
                        $maybe msg <- mmsg
                            <div class="alert alert-info alert-dismissable">
                                <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;
                                \#{msg}
                        ^{pageBody p}
                        <div .row>
                            <div .span5>
                                <h2>Log messages
                                <ul #log>
                            <div .span7>
                                <h2>Output
                                <pre #output>
                |]

mkYesod "App" [parseRoutes|
/ HomeR GET
/event EventR GET
|]

getHomeR :: Handler Html
getHomeR = do
    App {..} <- getYesod
    let reverseUrl = "http://localhost:" ++ show appReversePort
    defaultLayout $ do
        setTitle "Yesod Devel: Status page"
        addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"
        toWidget
            [julius|
                $(function(){
                    var src = new EventSource("@{EventR}");
                    var $log = $("#log");
                    var $output = $("#output");

                    src.addEventListener("log", function(e) {
                        var li = $("<li/>");
                        li.text(e.data);
                        $log.prepend(li);
                    });
                    src.addEventListener("chunk", function(e) {
                        $output.text($output.text() + JSON.parse(e.data));
                    });
                });
            |]
        [whamlet|
            <p>
                Your app can be accessed from
                <a target=_blank href=#{reverseUrl}>#{reverseUrl}#
                .
        |]

getEventR :: Handler TypedContent
getEventR = repEventSource $ \_ -> do
    App {..} <- lift getYesod
    yield $ CommentEvent $ fromText "Welcome to the event source"
    forever $ do
        emsg <- liftIO $ atomically $ waitCaptured appCapture
        let (name, content) =
                case emsg of
                    Left (when, msg) -> ("log", fromShow when <> fromText ": " <> fromText msg)
                    Right bs -> ("chunk", fromLazyByteString $ encode $ decodeUtf8With lenientDecode bs)
        yield $ ServerEvent (Just $ fromText name) Nothing [content]

status :: Int -- ^ app port
       -> MVar String
       -> App
       -> IO ()
status port mexitMsg app = do
    appl <- toWaiAppPlain app
    void $ forkIO $ run port appl `finally`
                    putMVar mexitMsg "Status app exited"
