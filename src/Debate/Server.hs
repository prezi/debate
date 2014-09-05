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

import           Control.Monad (forever)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe, fromJust)

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.HTTP.Types          as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import           Blaze.ByteString.Builder (fromLazyByteString)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Data.Text          as T
import qualified Data.ByteString    as B
import qualified Data.ByteString.Lazy as L (fromChunks)
import           Data.Aeson
import qualified Data.Map.Strict as Map

import           System.Timeout

import           Debate.Types

import Debug.Trace

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
                                 let (pathPrefix, _) = H.decodePath $ encodeUtf8 $ prefix configuration -- can be more than one /
                                     path = Wai.pathInfo req
                                 if matchPrefix pathPrefix path
                                   then routing path application state req respond
                                   else response404 respond

matchPrefix :: [T.Text] -> [T.Text] -> Bool
matchPrefix [] rest = True
matchPrefix (x:xs) (y:ys) = (x == y) && matchPrefix xs ys
matchPrefix (x:_) [] = False

routing pathInfo application state req respond = 
                    case pathInfo of
                        [_, "info"] -> responseInfo req respond
                        [_, _, sessionId, "xhr_send"] -> processXHR sessionId state req respond
                        [_, _, sessionId, "xhr"] -> pollXHR application sessionId state req respond
                        path -> do print path
                                   response404 respond

response404 respond = respond $ Wai.responseLBS H.status404 [("Content-Type", "text/plain")] "Not found"

responseInfo req respond = do
                ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
                respond $ Wai.responseBuilder H.status200 
                    (commonHeaders req)
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
                      (commonHeaders req) $ L.fromChunks [encodeUtf8 $ frameToText (ControlFrame OpenFrame)]

addClient :: ServerState -> SessionId -> TChan T.Text -> IO ()
addClient ServerState{..} sessionId receiveChan = atomically $ do
                   clientMap <- readTVar clients
                   emptyPending <- newEmptyTMVar
                   writeTVar clients $ Map.insert sessionId Client { sessionID = sessionId, pendingMessages = emptyPending, receiveChan = receiveChan } clientMap

pendingMessagesXHR sessionId state@ServerState{..} req respond = timedResponse 5000000 (msgFromApplication sessionId state) (msgResponse respond req) (emptyResponse respond req)

timedResponse delay function endedFunction timeoutFunction = do
                   result <- timeout delay function
                   case result of
                     Nothing -> timeoutFunction
                     Just msg -> endedFunction msg

emptyResponse respond req = respond $ Wai.responseLBS H.status200  
                               (commonHeaders req) ""

msgResponse respond req msg = respond $ Wai.responseLBS H.status200  
                                 (commonHeaders req) $ L.fromChunks [encodeUtf8 $ frameToText (DataFrame msg)]

processXHR sessionId state req respond = do 
                   body <- Wai.requestBody req
                   let msg = msgFromFrame . decodeUtf8 $ body
                   msgToApplication sessionId state msg
                   respond $ Wai.responseLBS H.status204 
                    (commonHeaders req) ""

-- application is running on separate thread
-- * gets sent msgs in TChan
-- * delivers msg to TMVar
msgToApplication :: SessionId -> ServerState -> T.Text -> IO ()
msgToApplication sessionId ServerState{..} msg = atomically $ do
                   clientMap <- readTVar clients
                   let client = fromJust $ Map.lookup sessionId clientMap
                   writeTChan (receiveChan client) msg

msgFromApplication :: SessionId -> ServerState -> IO [T.Text]
msgFromApplication sessionId ServerState{..} = atomically $ do
    clientMap <- readTVar clients
    let client = fromJust $ Map.lookup sessionId clientMap
    takeTMVar (pendingMessages client)

runApplication application sessionId state receive = forever $
    application SockConnection { receiveData = atomically $ readTChan receive, sendTextData = sendMessage }
    where sendMessage = addPendingMessages sessionId state

-- add msg to TVar as a queue
addPendingMessages :: SessionId -> ServerState -> T.Text -> IO ()
addPendingMessages sessionId ServerState{..} msg = atomically $ do
    clientMap <- readTVar clients
    let client = fromJust $ Map.lookup sessionId clientMap
    putTMVar (pendingMessages client) [msg]
    writeTVar clients $ Map.update (\_ -> Just client) sessionId clientMap

-- protocol framing see http://sockjs.github.io/sockjs-protocol/sockjs-protocol-0.3.html
wsApplication application state pending@WS.PendingConnection {pendingRequest = WS.RequestHead path _ _} = do
    -- path expected: prefix - server Id - session Id - 'websocket'
    let (pathInfo, _) = H.decodePath path
    print pathInfo

    receive <- atomically newTChan

    connection <- WS.acceptRequest pending
    addClient state "randomsessionId" receive
    WS.sendTextData connection (T.pack "o") -- sockjs client expects an "o" message to open socket
    _ <- forkIO $ heartbeat connection
    _ <- forkIO $ runApplication application "randomsessionId" state receive
    _ <- race (receiveLoop receive connection) (sendLoop state connection)
    return ()

receiveLoop receive connection =
    forever $ do
       msg <- WS.receiveData connection :: IO T.Text
       atomically $ writeTChan receive $ msgFromFrame msg
sendLoop state connection =
    forever $ do
       msg <- msgFromApplication "randomsessionId" state
       WS.sendTextData connection $ msgToFrame msg

-- TODO XHR heartbeat. from sockjs-protocol:
-- The session must time out after 5 seconds of not having a receiving connection. The server must send a heartbeat frame every 25 seconds. The heartbeat frame contains a single h character. This delay may be configurable.
-- TODO timeout of session and removal of data from server state
heartbeat connection = do
    WS.sendTextData connection (frameToText $ ControlFrame HeartbeatFrame)
    threadDelay 25000 -- default in sockjs js implementation
    heartbeat connection

-- utils
commonHeaders :: Wai.Request -> [H.Header]
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

msgFromFrame :: T.Text -> T.Text
msgFromFrame msg = T.splitOn "\"" msg !! 1

msgToFrame :: [T.Text] -> T.Text
msgToFrame msgs = frameToText $ DataFrame msgs
