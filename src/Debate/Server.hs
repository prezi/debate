{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Debate.Server 
( runServer
, Config(..)
, ServerState (..)
, SockConnection (..)
, httpApplication
, wsApplication
)
where

import           Control.Monad (liftM, forever)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.Async
import           GHC.Conc.Sync (atomically)
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe, fromJust)

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.HTTP.Types          as H
import qualified Network.HTTP.Types.Header   as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.Text          as T
import qualified Data.ByteString    as B
import qualified Data.ByteString.Lazy as L (fromChunks)
import Data.Aeson
import qualified Data.Map.Strict as Map

import System.Timeout

data Config = Config { port :: Int,
                       prefix :: T.Text }

type SessionId = T.Text

data Client = Client
  { sessionID :: SessionId
  , receiveChan :: TChan T.Text
  , pendingMessages :: TMVar [T.Text]
  }

data ServerState = ServerState {
    clients :: TVar (Map.Map SessionId Client)
  }

data Frame = ControlFrame ControlFrame
           | DataFrame [T.Text]

data ControlFrame = OpenFrame
                  | HeartbeatFrame
                  | CloseFrame

data SockConnection = SockConnection {
                      receiveData :: IO T.Text
                    , sendTextData :: T.Text -> IO ()
                    }

frameToText :: Frame -> T.Text
frameToText (ControlFrame OpenFrame) =  "o\n"
frameToText (DataFrame msgs) = T.concat ["a[\"", T.intercalate "\",\""  msgs, "\"]\n"]
frameToText (ControlFrame HeartbeatFrame) = "h\n"
frameToText (ControlFrame CloseFrame) = "c\n"

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
                    $ fromLazyByteString $ encode [ "websocket"     .= True
                                                   , "cookie_needed" .= False
                                                   , "origins"       .= ["*:*" :: T.Text]
                                                   , "entropy"       .= ent
                                                   ]

pollXHR application sessionId state@ServerState{..} req respond = do
                   clientMap <- readTVarIO clients
                   if Map.member sessionId clientMap
                    then pendingMessagesXHR sessionId state req respond
                    else openXHR application state sessionId req respond -- new session opened

openXHR application state@ServerState{..} sessionId req respond = do 
                   receiveChan <- atomically newTChan
                   addClient state sessionId receiveChan
                   _ <- forkIO $ runApplication application sessionId state receiveChan
                   respond $ Wai.responseLBS H.status200
                      (concat [headerJSON, headerNotCached, headerCORS "*" req]) $ L.fromChunks [encodeUtf8 $ frameToText (ControlFrame OpenFrame)]

addClient state@ServerState{..} sessionId receiveChan = atomically $ do
                   clientMap <- readTVar clients
                   emptyPending <- newEmptyTMVar
                   writeTVar clients $ Map.insert sessionId Client { sessionID = sessionId, pendingMessages = emptyPending, receiveChan = receiveChan } clientMap

pendingMessagesXHR sessionId state@ServerState{..} req respond = do
                   clientMap <- readTVarIO clients
                   let client = fromJust $ Map.lookup sessionId clientMap
                   timedResponse 5000000 (getPendingMessages sessionId state) (msgResponse respond req) (emptyResponse respond req)

timedResponse delay function endedFunction timeoutFunction = do
                   result <- timeout delay function
                   case result of
                     Nothing -> timeoutFunction
                     Just msg -> endedFunction msg

emptyResponse respond req = respond $ Wai.responseLBS H.status200  
                               (concat [headerJSON, headerNotCached, headerCORS "*" req]) ""

msgResponse respond req msg = respond $ Wai.responseLBS H.status200  
                                 (concat [headerJSON, headerNotCached, headerCORS "*" req]) $ L.fromChunks [encodeUtf8 $ frameToText (DataFrame msg)]

processXHR sessionId ServerState{..} req respond = do 
                   body <- Wai.requestBody req
                   let msg = msgFromFrame . decodeUtf8 $ body -- todo error handling on empty body
                   clientMap <- readTVarIO clients
                   let client = fromJust $ Map.lookup sessionId clientMap
                   atomically $ writeTChan (receiveChan client) msg -- msg to application
                   respond $ Wai.responseLBS H.status204 
                    (concat [headerJSON, headerNotCached, headerCORS "*" req]) ""

runApplication application sessionId state receive = forever $
    application SockConnection { receiveData = atomically $ readTChan receive, sendTextData = sendMessage }
    where sendMessage = addPendingMessages sessionId state

-- add msg to TVar as a queue
addPendingMessages sessionId ServerState{..} msg = atomically $ do
    clientMap <- readTVar clients
    let client = fromJust $ Map.lookup sessionId clientMap
    putTMVar (pendingMessages client) [msg]
    writeTVar clients $ Map.update (\c -> Just client) sessionId clientMap

-- cleanPendingMessages client = Client { sessionID = sessionID client, receiveChan = receiveChan client, pendingMessages = newTMVar }

-- get pending messages and empty out the TVar
getPendingMessages sessionId state@ServerState{..} = atomically $ do
    clientMap <- readTVar clients
    let client = fromJust $ Map.lookup sessionId clientMap
    takeTMVar (pendingMessages client)

-- protocol framing see http://sockjs.github.io/sockjs-protocol/sockjs-protocol-0.3.html
wsApplication application state pending@WS.PendingConnection {pendingRequest = WS.RequestHead path headers _} = do
    -- path expected: prefix - server Id - session Id - 'websocket'
    let (pathInfo, query) = H.decodePath path
    print pathInfo

    receive <- atomically newTChan

    connection <- WS.acceptRequest pending
    addClient state "randomsessionId" receive
    WS.sendTextData connection (T.pack "o") -- sockjs client expects an "o" message to open socket
    _ <- forkIO $ heartbeat connection
    _ <- forkIO $ runApplication application "randomsessionId" state receive
    race (receiveLoop receive connection) (sendLoop state connection)
    return ()

receiveLoop receive connection =
    forever $ do
       msg <- WS.receiveData connection :: IO T.Text
       atomically $ writeTChan receive $ msgFromFrame msg
sendLoop state connection =
    forever $ do
       msg <- getPendingMessages "randomsessionId" state
       WS.sendTextData connection $ msgToFrame msg

-- TODO XHR heartbeat. from sockjs-protocol:
-- The session must time out after 5 seconds of not having a receiving connection. The server must send a heartbeat frame every 25 seconds. The heartbeat frame contains a single h character. This delay may be configurable.
-- TODO timeout of session and removal of data from server state
heartbeat connection = do
    WS.sendTextData connection (frameToText $ ControlFrame HeartbeatFrame)
    threadDelay 25000 -- default in sockjs js implementation
    heartbeat connection

-- utils
commonHeaders req = concat [headerJSON, headerNotCached, headerCORS "*" req]

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

msgToFrame msgs = frameToText $ DataFrame msgs
