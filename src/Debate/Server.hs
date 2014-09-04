{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Debate.Server 
( runServer
, Config(..)
, ServerState (..)
, httpApplication
, wsApplication
)
where

import           Control.Monad (liftM, forever)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TChan
import           GHC.Conc.Sync (atomically)
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe, fromJust)

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.HTTP.Types          as H
import qualified Network.HTTP.Types.Header   as H
import qualified Network.Wai as Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.Text          as T
import qualified Data.ByteString    as B
import qualified Data.ByteString.Lazy as L (fromChunks)
import Data.Aeson
import qualified Data.Map.Strict as Map

data Config = Config { port :: Int,
                       prefix :: T.Text }

type SessionId = T.Text
data Client = Client
  { sessionID :: SessionId
  , sendChan :: TChan T.Text
  , receiveChan :: TChan T.Text
  }
data ServerState = ServerState {
    clients :: TVar (Map.Map SessionId Client)
  }

data Frame = OpenFrame
           | MsgFrame [T.Text]
           | HeartbeatFrame
           | CloseFrame


toText OpenFrame =  "o\n"
toText (MsgFrame msgs) = T.concat ["a[\"", T.intercalate "\",\""  msgs, "\"]\n"]
toText HeartbeatFrame = "h\n"
toText CloseFrame = "c\n"

newServerState :: IO ServerState
newServerState = do
    newVar <- newTVarIO Map.empty
    return ServerState {clients = newVar}

-- TODO close and cleanup
-- TODO error handling

runServer configuration application = do
    let settings = Warp.setPort (port configuration) Warp.defaultSettings
    state <- newServerState
    Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApplication application state) (httpApplication configuration application state)


-- TODO: add routing for '/info', '/greeting', and all xhr polling
-- the websocket or xhr-polling transport should go over 
-- /prefix/server id/session id unique to session
-- add '/websocket' for websocket transport
-- add '/xhr' for xhr-polling
-- todo routing on prefix too (threading)
httpApplication configuration application state req respond = do
                                 let pathPrefix = prefix configuration
                                 print $ Wai.pathInfo req
                                 case Wai.pathInfo req of
                                   [pathPrefix, "info"] -> responseInfo req respond
                                   [pathPrefix, serverId, sessionId, "xhr_send"] -> processXHR sessionId state req respond
                                   [pathPrefix, serverId, sessionId, "xhr"] -> pollXHR application sessionId state req respond
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

pollXHR application sessionId state@ServerState{..} req respond = do
                   clientMap <- atomically $ readTVar clients
                   if Map.member sessionId clientMap
                    then pendingMessagesXHR sessionId state req respond
                    else openXHR application state sessionId req respond -- new session opened

openXHR application state@ServerState{..} sessionId req respond = do 
                   clientMap <- atomically $ readTVar clients
                   sendChan <- atomically newTChan
                   receiveChan <- atomically newTChan
                   _ <- forkIO (atomically $ application (readTChan receiveChan) (writeTChan sendChan))
                   atomically $ writeTVar clients $ Map.insert sessionId Client { sessionID = sessionId, sendChan = sendChan, receiveChan = receiveChan } clientMap
                   respond $ Wai.responseLBS H.status200 [] $ L.fromChunks [encodeUtf8 $ toText OpenFrame]

pendingMessagesXHR sessionId state@ServerState{..} req respond = do
                   clientMap <- atomically $ readTVar clients
                   let client = fromJust $ Map.lookup sessionId clientMap
                   -- read message
                   msg <- atomically $ readTChan (sendChan client) 
                   respond $ Wai.responseLBS H.status200 [] $ L.fromChunks [encodeUtf8 $ toText (MsgFrame [msg])]

processXHR sessionId ServerState{..} req respond = do 
                   (params, _) <- parseRequestBody lbsBackEnd req
                   let msg = maybe "" (msgFromFrame . decodeUtf8) (lookup "body" params) -- todo error handling on empty body
                   clientMap <- atomically $ readTVar clients
                   let client = fromJust $ Map.lookup sessionId clientMap
                   atomically $ writeTChan (receiveChan client) msg -- msg to application
                   respond $ Wai.responseLBS H.status204 [] ""

-- protocol framing see http://sockjs.github.io/sockjs-protocol/sockjs-protocol-0.3.html
wsApplication application state pending@WS.PendingConnection {pendingRequest = WS.RequestHead path headers _} = do
    -- path expected: prefix - server Id - session Id - 'websocket'
    let (pathInfo, query) = H.decodePath path
    print pathInfo

    send <- atomically newTChan
    receive <- atomically newTChan

    connection <- WS.acceptRequest pending
    WS.sendTextData connection (T.pack "o") -- sockjs client expects an "o" message to open socket
    _ <- forkIO $ heartbeat connection
    _ <- forkIO $ atomically $ application (readTChan receive) (writeTChan send)
    _ <- forkIO $ forever $ do
       msg <- WS.receiveData connection :: IO T.Text
       atomically $ writeTChan receive $ msgFromFrame msg
    forever $ do
       msg <- atomically $ readTChan send
       WS.sendTextData connection $ msgToFrame msg

-- TODO make this dependent on current transport state (websocket or xhr)
wsReceiveData connection = liftM msgFromFrame (WS.receiveData connection :: IO T.Text)
wsSendTextData connection msg =
    WS.sendTextData connection $ T.concat ["a", msgToFrame msg] -- msg already in an encoded array at reception

-- TODO XHR heartbeat. from sockjs-protocol:
-- The session must time out after 5 seconds of not having a receiving connection. The server must send a heartbeat frame every 25 seconds. The heartbeat frame contains a single h character. This delay may be configurable.
-- TODO timeout of session and removal of data from server state
heartbeat connection = do
    WS.sendTextData connection (toText HeartbeatFrame)
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

msgFromFrame msg = T.splitOn "\"" msg !! 1

msgToFrame msg = toText $ MsgFrame [msg]
