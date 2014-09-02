{-# LANGUAGE OverloadedStrings #-}
module Debate.Server 
( runServer
, Config(..)
, httpApplication
, wsApplication
, receiveData
, sendTextData
, SockJSMessage(..)
)
where

import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (forkIO, threadDelay)
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe)

import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types          as H
import qualified Network.HTTP.Types.Header   as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Blaze.ByteString.Builder (fromLazyByteString)

import qualified Data.Text          as T
import qualified Data.ByteString    as B

import Data.Aeson

-- TODO add prefix to config
data Config = Config { port :: Int,
                       prefix :: T.Text }

data SockJSMessage = SockJSMessage [T.Text]

runServer configuration application = do
    let settings = Warp.setPort (port configuration) Warp.defaultSettings
    Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApplication application) (httpApplication configuration application)

-- TODO: add routing for '/info', '/greeting', and all xhr polling
-- the websocket or xhr-polling transport should go over 
-- /prefix/server id/session id unique to session
-- add '/websocket' for websocket transport
-- add '/xhr' for xhr-polling
-- todo routing on prefix too (threading)
httpApplication :: Config -> (WS.Connection -> IO ()) -> Wai.Application
httpApplication configuration application req respond = do
                                 let pathPrefix = prefix configuration
                                 case Wai.pathInfo req of
                                   [pathPrefix, "info"] -> responseInfo req respond
                                   [pathPrefix, serverId, sessionId, "xhr"] -> openXHR req respond
                                   [pathPrefix, serverId, sessionId, "xhr_send"] -> receiveXHR req respond
                                   path -> do print path
                                              respond $ Wai.responseLBS H.status404 [("Content-Type", "text/plain")] "Not found"


responseInfo req respond = do
                ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
                respond $ Wai.responseBuilder H.status200 
                    (concat [headerJSON, headerNotCached, headerCORS "*" req])
                    $ fromLazyByteString $ encode         [ "websocket"     .= True
                                                        , "cookie_needed" .= False
                                                        , "origins"       .= ["*:*" :: T.Text]
                                                        , "entropy"       .= ent
                                                        ]

openXHR req respond = respond $ Wai.responseLBS H.status200 [] "o\n"

receiveXHR req respond = respond $ Wai.responseLBS H.status204 [] ""

-- protocol framing see http://sockjs.github.io/sockjs-protocol/sockjs-protocol-0.3.html
wsApplication application pending = do
    connection <- WS.acceptRequest pending
    WS.sendTextData connection (T.pack "o") -- sockjs client expects an "o" message to open socket
    _ <- forkIO $ heartbeat connection
    application connection

heartbeat connection = do
    WS.sendTextData connection (T.pack "h")
    threadDelay 25000 -- default in sockjs js implementation
    heartbeat connection

-- utils
headerJSON :: H.ResponseHeaders
headerJSON = [("Content-Type", "application/json; charset=UTF-8")]

headerNotCached :: H.ResponseHeaders
headerNotCached = [("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")]

headerCORS :: B.ByteString -> Wai.Request -> H.ResponseHeaders
headerCORS def req = allowHeaders ++ allowOrigin ++ allowCredentials
    where allowCredentials = [("Access-Control-Allow-Credentials", "true")]
          allowHeaders =
              case lookup "Access-Control-Request-Headers" $ Wai.requestHeaders req of
                   Just "" -> []
                   Just ah -> [("Access-Control-Allow-Headers", ah)]
                   Nothing -> []
          allowOrigin =
              case origin of
                   ""     -> []
                   "null" -> [("Access-Control-Allow-Origin", def)]
                   _      -> [("Access-Control-Allow-Origin", origin)]
          origin = fromMaybe def . lookup "Origin" $ Wai.requestHeaders req

-- remove square brackets and quotes ["msg"]
receiveData connection = WS.receiveData connection :: IO T.Text
-- add square brackets and quotes
sendTextData connection msg =
    WS.sendTextData connection $ T.concat ["a", msg] -- msg already in an encoded array at reception
