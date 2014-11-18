{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Chat.Application (
  chat
, MessageData(..)
, ChatSecurity(..)
, newChatState
) where

import Control.Monad (forever, void, when, guard, unless)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Control.Exception (tryJust)
import Data.Maybe (fromMaybe, isJust)
import Data.Aeson (FromJSON(..), ToJSON(..), decodeStrict, Value(..), (.=), object)
import Data.Aeson.Encode (encodeToTextBuilder)
import GHC.Generics (Generic) -- generics
import Chat.Message
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)

import System.Log.Logger

import Debate.Server
import Chat.State

data ChatSecurity = ChatSecurity {
                      checkCredentials :: String -> String -> IO (Maybe String)
                    , checkAccess :: String -> String -> IO (Maybe String)
                    }

data ClientMessage = ClientMessage { user :: Maybe T.Text
                                   , channel :: Maybe T.Text
                                   , message :: T.Text }
                     deriving (Show, Generic)

data MessageData = MessageData { messageDataUser :: T.Text
                               , messageDataChannel :: T.Text
                               , messageDataMessage :: String }
                 | CommandMsg { commandMsgUser :: Maybe T.Text
                              , commandMsgChannel :: Maybe T.Text
                              , command :: ChatCommand }
                 | StatusMsg { loggedin :: Int
                             , chatrooms :: Int }
                     deriving Show

data ChatCommand = LoginCommand
                 | LogoutCommand
                 | JoinCommand
                 | LeaveCommand
                 | MessageCommand
                 | LoginRequired
                 | LoginFailed
                 | AlreadyLoggedIn
                 | AlreadyJoined
                 | NotJoined
                 | NoAccess
                   deriving (Show)

instance FromJSON ClientMessage

-- type -> Value
instance ToJSON MessageData where
  toJSON (MessageData user channel message) =
    object [ "user" .= user
           , "channel" .= channel
           , "message" .= message
           , "command" .= ("msg" :: String) ]
  toJSON (CommandMsg mbUser mbChannel command) =
    object [ "user" .= toJSON mbUser
           , "channel" .= toJSON mbChannel
           , "command" .= toJSON command ]
  toJSON (StatusMsg loggedin chatrooms) =
    object [ "loggedIn" .= toJSON loggedin
           , "chatRooms" .= toJSON chatrooms ]

instance ToJSON ChatCommand where
  toJSON LoginCommand = String "login"
  toJSON LogoutCommand = String "logout"
  toJSON JoinCommand = String "join"
  toJSON LeaveCommand = String "leave"
  toJSON MessageCommand = String "msg"
  toJSON LoginFailed = String "loginFailed"
  toJSON LoginRequired = String "loginRequired"
  toJSON AlreadyLoggedIn = String "alreadyLoggedIn"
  toJSON AlreadyJoined = String "alreadyJoined"
  toJSON NotJoined = String "notJoined"
  toJSON NoAccess = String "noAccess"

mainChatRoom = "Lobby"
statusIdentifier = "Status##"

chat ChatSecurity{..} chatState connection =
    forever $ do
        (_, _, msg) <- receiveJsonMessage connection
        case msg of
          Login name pass  -> do credentials <- checkCredentials name pass
                                 case credentials of
                                    Just user -> void $ loggedIn user connection chatState checkAccess
                                    Nothing   -> sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginFailed })
          Status name pass -> do credentials <- checkCredentials name pass
                                 access <- checkAccess name statusIdentifier
                                 let ok = isJust credentials && isJust access -- laziness rules
                                 if ok
                                    then statusMessage chatState >>= (sendTextData connection . toJsonMessage)
                                    else sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginFailed })
          _                -> sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginRequired})

-- TODO: only broadcast to logged in users!
-- TODO: check if user name in message matches username in state
loggedIn userName connection chatState checkAccess = do
        let user = User (T.pack userName) connection
        saveTVar chatState $ addUserToRoom mainChatRoom user
        warningM "Chat.Application" (userName ++ " just logged in")
        chatstate <- readTVarIO chatState
        let loginMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just mainChatRoom, command = LoginCommand }
        -- sendToRoom chatState mainChatRoom loginMsg -- performance issues
        sendToClient chatState mainChatRoom loginMsg user
        runEitherT $ forever $ do
            (mbUsr, mbRoom, msg) <- lift $ receiveJsonMessage connection
            case msg of
              Logout -> when (mbUsr == Just (T.pack userName)) $ do
                          lift $ logout userName chatState user
                          left () -- exit
              _      -> when (mbUsr == Just (T.pack userName)) (lift $ sendIntendedMessage msg mbRoom userName chatState user checkAccess) -- only send if you are the intended recipient.

logout :: String -> TVar ChatState -> User -> IO ()
logout userName chatState user = do
        let logoutMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Nothing, command = LogoutCommand }
        -- sendToRoom chatState mainChatRoom logoutMsg -- performance issues
        sendToClient chatState mainChatRoom logoutMsg user
        warningM "Chat.Application" (userName ++ " just logged out")
        saveTVar chatState (removeUserFromAll user)


sendIntendedMessage :: ServerMessage -> Maybe T.Text -> String -> TVar ChatState -> User -> (String -> String -> IO (Maybe String)) -> IO ()
sendIntendedMessage msg mbRoom userName chatState user checkAccess =
        case msg of
            Join roomName -> do chState <- readTVarIO chatState
                                let tRoomname = T.pack roomName
                                if isUserInRoom chState user tRoomname
                                  then sendToClient chatState tRoomname CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomname, command = AlreadyJoined } user
                                  else joinRoom user roomName chatState checkAccess
            Leave roomName -> do chState <- readTVarIO chatState
                                 let tRoomName = T.pack roomName
                                 if isUserInRoom chState user tRoomName
                                   then leaveRoom user roomName chatState
                                   else sendToClient chatState mainChatRoom CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomName, command = NotJoined } user
            Login _ _ -> do let alreadyLoggedInMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just mainChatRoom, command = AlreadyLoggedIn }
                            sendToClient chatState mainChatRoom alreadyLoggedInMsg user
            Message str -> do let roomname = fromMaybe mainChatRoom mbRoom
                              access <- checkAccess userName (T.unpack roomname)
                              -- todo check if user joined as well
                              case access of
                                 Just username -> do let chatMsg = MessageData { messageDataUser = T.pack username, messageDataChannel = roomname, messageDataMessage = username ++ ": " ++ str}
                                                     sendToRoom chatState roomname chatMsg
                                 Nothing -> warningM "Chat.Application" (userName ++ " attempted to post to " ++ T.unpack roomname)
            _  -> return () -- no action, potentially logging

receiveJsonMessage :: SockConnection -> IO (Maybe T.Text, Maybe T.Text, ServerMessage)
receiveJsonMessage connection = do
            received <- receiveData connection
            debugM "Chat.Application" $ T.unpack received
            return $ parseJsonMessage received

joinRoom user roomname chatState checkAccess = do
            let tUsername = userName user
                username = T.unpack tUsername
                tRoomName = T.pack roomname
            access <- checkAccess username roomname
            case access of
                Just _ -> do let joinedMsg = CommandMsg { commandMsgUser = Just tUsername, commandMsgChannel = Just tRoomName, command = JoinCommand }
                             saveTVar chatState $ addUserToRoom tRoomName user
                             sendToRoom chatState tRoomName joinedMsg
                             warningM "Chat.Application" (username ++ " joined " ++ roomname)
                Nothing -> do let noAccessMsg = CommandMsg { commandMsgUser = Just tUsername, commandMsgChannel = Just tRoomName, command = NoAccess }
                              sendToClient chatState mainChatRoom noAccessMsg user
                              warningM "Chat.Application" (username ++ " attempted to join " ++ roomname)

leaveRoom :: User -> String -> TVar ChatState -> IO ()
leaveRoom user roomname chatState = do
            let tRoomName = T.pack roomname
                leaveMsg = CommandMsg { commandMsgUser = Just (userName user), commandMsgChannel = Just tRoomName, command = LeaveCommand }
            unless (tRoomName == mainChatRoom) $ do
                sendToRoom chatState tRoomName leaveMsg
                saveTVar chatState (removeUserFromRoom user tRoomName)
                warningM "Chat.Application" (T.unpack (userName user) ++ " left " ++ roomname)
-- if not valid json: invalid msg
-- if valid json but message not parsed : chat msg
-- else: parsed command msg
parseJsonMessage :: T.Text -> (Maybe T.Text, Maybe T.Text, ServerMessage)
parseJsonMessage received = do
            let mbMsgData = decodeStrict (encodeUtf8 received) :: Maybe ClientMessage
            case mbMsgData of
                Just clientMessage -> do let parsedMsg = parseMessage (T.unpack $ message clientMessage)
                                         case parsedMsg of
                                            Left _ -> (Nothing, Nothing, Invalid "invalid message")
                                            Right msg -> (user clientMessage, channel clientMessage, msg)
                Nothing -> (Nothing, Nothing, Invalid "json parsing error")

toJsonMessage :: MessageData -> T.Text
toJsonMessage = toStrict . toLazyText . encodeToTextBuilder . toJSON

-- send message to all users of a chat room
-- handle cleanup of closed clients!
sendToRoom :: TVar ChatState -> T.Text -> MessageData -> IO ()
sendToRoom tvarChatState roomName message = do
           chatState <- readTVarIO tvarChatState
           let mbRoom = Map.lookup roomName (roomState chatState)
               users = maybe [] usersInRoom mbRoom
               json = toJsonMessage message
           mapM_ (sendIfClientPresent tvarChatState roomName json) users

sendToClient :: TVar ChatState -> T.Text -> MessageData -> User -> IO ()
sendToClient chatState roomName msg = sendIfClientPresent chatState roomName (toJsonMessage msg)

sendIfClientPresent :: TVar ChatState -> T.Text -> T.Text -> User -> IO ()
sendIfClientPresent tvarChatState roomName json user = do
           err <- tryJust (guard . (== ClientNotFoundException)) $ sendTextData (handle user) json
           case err of
              Left _ -> do warningM "Chat.Application" (T.unpack (userName user) ++ " disconnected")
                           putStrLn $ "disconnected " ++ show (userName user)
                           saveTVar tvarChatState (removeUserFromAll user)
              Right _ -> return ()

-- status message

statusMessage :: TVar ChatState -> IO MessageData
statusMessage tvarChatState = do
           chatState <- readTVarIO tvarChatState
           let isLoggedIn = Map.size $ userState chatState
               chatRooms = Map.size (roomState chatState) - 1 -- mainChatRoom doesn't count.
           return StatusMsg { loggedin = isLoggedIn, chatrooms = chatRooms }
