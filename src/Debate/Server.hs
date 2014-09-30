{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
module Debate.Server 
( runServer
, Config(..)
, ServerState (..)
, SockConnection (..)
, httpApplication
, wsApplication
, defaultConfiguration
, setPort
, setPrefix
, setTransportWhitelist
)
where

import           Control.Monad (forever)
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception (fromException, handle, Exception(..), throw)
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe, fromJust, isNothing)

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
import           Data.List ((\\))
import           Data.Traversable (mapM)
import           Data.Typeable

import           System.Timeout

import           Debate.Types

data DebateException = ClientNotFoundException
                       deriving (Show, Typeable)

instance Exception DebateException

-- TODO close and cleanup
-- TODO more error handling
-- NOTE: for exhaustive implementation of sockjs:
--  * content types: ['text/plain', 'T', 'application/json', 'application/xml', '', 'application/json; charset=utf-8', 'text/xml; charset=utf-8', 'text/xml']
--  * haskell websockets library only supports Hybi13 (that's what the code suggests), none of the other websocket protocols (Hixie76, Hybi10, Hybi07), which are necessary for a complete implementation
--  * iframe, xhr streaming, json polling, ...
--  * params: transport types, heartbeat delay

runServer configuration application = do
    let settings = Warp.setPort (port configuration) Warp.defaultSettings
        transports = transportWhitelist configuration
    state <- newServerState
    -- at the moment just provide option of limiting to xhr polling - should handle other cases (all combos of xhr polling and websockets)
    case transports of
      ["xhr_polling"] -> Warp.runSettings settings $ httpApplication configuration application state
      otherwise -> Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApplication configuration application state) (httpApplication configuration application state)

-- TODO: add routing for '/info', '/greeting', and all xhr polling
-- the websocket or xhr-polling transport should go over 
-- /prefix/server id/session id unique to session
-- add '/websocket' for websocket transport
-- add '/xhr' for xhr-polling
httpApplication configuration application state req respond = do
                                 let (pathPrefix, _) = H.decodePath $ encodeUtf8 $ prefix configuration -- can be more than one /
                                     path = Wai.pathInfo req
                                 if matchPrefix pathPrefix path
                                   then routing (path \\ pathPrefix) application state req respond
                                   else response404 respond

matchPrefix :: [T.Text] -> [T.Text] -> Bool
matchPrefix [] rest = True
matchPrefix (x:xs) (y:ys) = (x == y) && matchPrefix xs ys
matchPrefix (x:_) [] = False

routing pathInfo application state req respond = 
                    case pathInfo of
                        ["info"] -> responseInfo req respond
                        [_, sessionId, "xhr_send"] -> processXHR sessionId state req respond
                        [_, sessionId, "xhr"] -> pollXHR application sessionId state req respond
                        path -> do putStrLn $ "http route not found: " ++ show path
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
                   existingClient <- existingSessionId sessionId state
                   if existingClient
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

removeClient :: ServerState -> SessionId -> IO ()
removeClient ServerState{..} sessionId = atomically $ do
                   clientMap <- readTVar clients
                   emptyPending <- newEmptyTMVar
                   writeTVar clients $ Map.delete sessionId clientMap

-- xhr delay 5 seconds for now - could be a parameter
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

existingSessionId :: SessionId -> ServerState -> IO Bool
existingSessionId sessionId ServerState{..} = do
                   clientMap <- readTVarIO clients
                   return $ Map.member sessionId clientMap

processXHR sessionId state req respond = do 
                   existingClient <- existingSessionId sessionId state
                   if existingClient
                   then do
                    body <- Wai.requestBody req
                    let msg = msgFromFrame . decodeUtf8 $ body
                    msgToApplication sessionId state msg
                    respond $ Wai.responseLBS H.status204 
                        (commonHeaders req) ""
                   else response404 respond

-- application is running on separate thread
-- * gets sent msgs in TChan
-- * delivers msg to TMVar
msgToApplication :: SessionId -> ServerState -> T.Text -> IO ()
msgToApplication sessionId ServerState{..} msg = atomically $ do
                   clientMap <- readTVar clients
                   let mbClient = Map.lookup sessionId clientMap
                   case mbClient of
                      Just client -> writeTChan (receiveChan client) msg
                      Nothing -> return ()

msgFromApplication :: SessionId -> ServerState -> IO [T.Text]
msgFromApplication sessionId ServerState{..} = atomically $ do
    clientMap <- readTVar clients
    let client = fromJust $ Map.lookup sessionId clientMap
    takeTMVar (pendingMessages client)

runApplication application sessionId state receive = forever $
    application SockConnection { receiveData = atomically $ readTChan receive, sendTextData = sendMessage, broadcastData = broadcast state }
    where sendMessage = addPendingMessages sessionId state

broadcast :: ServerState -> T.Text -> IO ()
broadcast state@ServerState{..} msg = do
    clientMap <- readTVarIO clients
    Data.Traversable.mapM (\client -> addPendingMessages (sessionID client) state msg) clientMap -- mapM_ for traversable?
    return ()

-- add msg to TVar as a queue
addPendingMessages :: SessionId -> ServerState -> T.Text -> IO ()
addPendingMessages sessionId state@ServerState{..} msg = atomically $ do
    clientMap <- readTVar clients
    let mbClient = Map.lookup sessionId clientMap
    if isNothing mbClient
      then throw ClientNotFoundException
      else do let client = fromJust mbClient
              putTMVar (pendingMessages client) [msg]
              writeTVar clients $ Map.update (\_ -> Just client) sessionId clientMap

-- protocol framing see http://sockjs.github.io/sockjs-protocol/sockjs-protocol-0.3.html
wsApplication configuration application state pending@WS.PendingConnection {pendingRequest = WS.RequestHead path _ _} = do
    -- path expected: prefix - server Id - session Id - 'websocket'
    let (pathInfo, _) = H.decodePath path
        (pathPrefix, _) = H.decodePath $ encodeUtf8 $ prefix configuration
    if matchPrefix pathPrefix pathInfo
       then WS.acceptRequest pending >>= newWebsocketClient application state (pathInfo \\ pathPrefix)
       else WS.rejectRequest pending "url not handled by this server"

newWebsocketClient application state path connection = do
    let [serververId, sessionId, "websocket"] = path
    receive <- atomically newTChan
    addClient state sessionId receive
    WS.sendTextData connection (T.pack "o") -- sockjs client expects an "o" message to open socket
    _ <- forkIO $ heartbeat connection
    _ <- forkIO $ runApplication application sessionId state receive
    race_ (receiveLoop receive connection state sessionId) (sendLoop sessionId state connection)
    return ()

receiveLoop receive connection state sessionId = handle (catchDisconnect state sessionId) $
    forever $ do
       msg <- WS.receiveData connection :: IO T.Text
       atomically $ writeTChan receive $ msgFromFrame msg
     where catchDisconnect state sessionId e = case fromException e of
            Just WS.ConnectionClosed -> removeClient state sessionId
            _ -> return ()
sendLoop sessionId state connection =
    forever $ do
       msg <- msgFromApplication sessionId state
       WS.sendTextData connection $ msgToFrame msg

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

-- sensible defaults?
defaultConfiguration :: Config
defaultConfiguration = Config { port = 8888, prefix = "/", transportWhitelist = ["websocket", "xhr_polling"] }

setPort num configuration = configuration { port = num } 
setPrefix pf configuration = configuration { prefix = pf } 
setTransportWhitelist tl configuration = configuration { transportWhitelist = tl } 
