{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Debate.Server 
( runServer
, Config(..)
, ServerState (..)
, httpApplication
, wsApplication
, receiveData
, sendTextData
)
where

import           Control.Monad.Trans (lift)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           GHC.Conc.Sync (atomically)
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe)

import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types          as H
import qualified Network.HTTP.Types.Header   as H
import qualified Network.Wai as Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Text          as T
import qualified Data.ByteString    as B

import Data.Aeson
import qualified Data.Map.Strict as Map

data Config = Config { port :: Int,
                       prefix :: T.Text }

type SessionId = T.Text
data Client = Client
  { sessionId :: SessionId,
    pendingMessages :: [T.Text]
  }
data ServerState = ServerState {
                     clients :: TVar (Map.Map SessionId Client)
                   }

runServer configuration application = do
    let settings = Warp.setPort (port configuration) Warp.defaultSettings
    newVar <- atomically $ newTVar Map.empty
    let state    = ServerState {clients = newVar}
    Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApplication application state) (httpApplication configuration application state)

-- TODO: add routing for '/info', '/greeting', and all xhr polling
-- the websocket or xhr-polling transport should go over 
-- /prefix/server id/session id unique to session
-- add '/websocket' for websocket transport
-- add '/xhr' for xhr-polling
-- todo routing on prefix too (threading)
httpApplication :: Config -> (WS.Connection -> IO ()) -> ServerState -> Wai.Application
httpApplication configuration application state req respond = do
                                 let pathPrefix = prefix configuration
                                 print $ Wai.pathInfo req
                                 case Wai.pathInfo req of
                                   [pathPrefix, "info"] -> responseInfo req respond
                                   [pathPrefix, serverId, sessionId, "xhr_send"] -> processXHR sessionId state req respond
                                   [pathPrefix, serverId, sessionId, "xhr"] -> openXHR req respond
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

processXHR sessionId state req respond = do (params, _) <- parseRequestBody lbsBackEnd req
                                            let msg = maybe "" (msgFromFrame . decodeUtf8) (lookup "body" params) -- todo error handling on empty body
                                            atomically $ savePendingMsg state sessionId msg 
                                            respond $ Wai.responseLBS H.status204 [] ""

savePendingMsg ServerState{..} sessionId msg = do
                   clientMap <- readTVar clients
                   let session = Map.lookup sessionId clientMap
                       newMap = case session of
                            Nothing -> Map.insert sessionId (newClient sessionId msg) clientMap
                            Just client -> Map.adjust (addPendingMessage msg) sessionId clientMap
                   writeTVar clients $ newMap

newClient sessionId msg = Client {sessionId = sessionId, pendingMessages = [msg]}

addPendingMessage msg client = Client { sessionId = sessionId client, pendingMessages = (pendingMessages client) ++ [msg] }

-- protocol framing see http://sockjs.github.io/sockjs-protocol/sockjs-protocol-0.3.html
wsApplication application state pending = do
    connection <- WS.acceptRequest pending
    WS.sendTextData connection (T.pack "o") -- sockjs client expects an "o" message to open socket
    _ <- forkIO $ heartbeat connection
    application connection

heartbeat connection = do
    WS.sendTextData connection (T.pack "h")
    threadDelay 25000 -- default in sockjs js implementation
    heartbeat connection

-- TODO make this dependent on current transport state (websocket or xhr)
receiveData connection = (WS.receiveData connection :: IO T.Text) >>= (return . msgFromFrame)
sendTextData connection msg =
    WS.sendTextData connection $ T.concat ["a", msgToFrame msg] -- msg already in an encoded array at reception

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

msgFromFrame msg = (T.splitOn "\"" msg)!!1

msgToFrame msg = T.concat ["[\"", msg, "\"]"]
