{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Chat.Application (
  chat
, MessageData(..)
, ChatSecurity(..)
, newRoomState
, newUserState
) where

import Control.Monad (mzero, forever, void, when)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import Control.Applicative (pure, empty)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.Aeson (FromJSON(..), ToJSON(..), decodeStrict, encode, Value(..), (.=), object)
import GHC.Generics (Generic) -- generics
import Chat.Message
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Lazy as HML
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import System.Log.Logger

import Debate.Server

data ChatSecurity = ChatSecurity {
                      checkCredentials :: String -> String -> IO (Maybe String)
                    , checkAccess :: String -> String -> IO (Maybe String)
                    }
data User = User { userName :: T.Text
                 , handle :: SockConnection }

instance Show User where
  show User {userName = name} = "user " ++ T.unpack name
-- TODO!! this means we cannot have 2 users with the same username, and this should be checked
instance Eq User where
  user1 == user2 = userName user1 == userName user2

data Room = Room { roomName :: T.Text
                 , usersInRoom :: [User] }
                 deriving (Show, Eq)

data UserState = UserState { users :: TVar (Map.Map T.Text [T.Text]) }
data RoomState = RoomState { rooms :: TVar (Map.Map T.Text Room) }

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
                     deriving Show



data ChatCommand = LoginCommand | LogoutCommand | JoinCommand | MessageCommand | LoginRequired | LoginFailed
                   deriving (Show)

instance FromJSON ClientMessage

instance ToJSON MessageData where
  toJSON (MessageData user channel message) =
    object [ "user" .= user
           , "channel" .= channel
           , "message" .= message ]
  toJSON (CommandMsg mbUser mbChannel command) =
    object [ "user" .= toJSON mbUser
           , "channel" .= toJSON mbChannel
           , "command" .= toJSON command ]

instance ToJSON ChatCommand where
  toJSON LoginCommand = String "login"
  toJSON LogoutCommand = String "logout"
  toJSON JoinCommand = String "join"
  toJSON MessageCommand = String "msg"
  toJSON LoginFailed = String "loginFailed"
  toJSON LoginRequired = String "loginRequired"

newRoomState = do newVar <- atomically $ newTVar (Map.insert mainChatRoom (Room mainChatRoom []) Map.empty)
                  return RoomState {rooms = newVar}
newUserState = do newVar <- atomically $ newTVar Map.empty
                  return UserState {users = newVar}
mainChatRoom = "Lobby"

chat ChatSecurity{..} userState roomState connection =
    forever $ do
        msg <- receiveJsonMessage connection
        case msg of
          Login name pass -> do credentials <- checkCredentials name pass
                                case credentials of
                                    Just user -> void (loggedIn user connection roomState userState checkAccess)
                                    Nothing   -> sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginFailed })
          _               -> sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginRequired})

-- TODO: only broadcast to logged in users!
-- TODO: check if user name in message matches username in state
loggedIn userName connection roomState userState checkAccess = do
        let user = User (T.pack userName) connection
        addUserToRoom roomState userState mainChatRoom user
        warningM "Chat.Application" (userName ++ " just logged in")
        let loginMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Nothing, command = LoginCommand }
        sendToRoom roomState mainChatRoom loginMsg
        runMaybeT $ forever $ lift $ do
            msg <- receiveJsonMessage connection
            print msg
            case msg of
              Logout ->  do let logoutMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Nothing, command = LogoutCommand }
                            sendToRoom roomState mainChatRoom logoutMsg
                            warningM "Chat.Application" (userName ++ " just logged out")
                            removeUserFromAll roomState userState user
                            breakLoop  -- leave loggedIn loop
              Join roomName -> do   access <- checkAccess userName roomName
                                    case access of
                                        Just userName -> do let tRoomName = T.pack roomName
                                                                joinedMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomName, command = JoinCommand }
                                                            addRoomIfNew roomState tRoomName
                                                            addUserToRoom roomState userState tRoomName user
                                                            sendToRoom roomState tRoomName joinedMsg
                                        Nothing -> warningM "Chat.Application" (userName ++ " attempted to join " ++ roomName)
              Message roomName str -> do access <- checkAccess userName (T.unpack roomName)
                                         -- todo check if user joined as well
                                         case access of
                                            Just userName -> do let chatMsg = MessageData { messageDataUser = T.pack userName, messageDataChannel = roomName, messageDataMessage = str}
                                                                sendToRoom roomState roomName chatMsg
                                            Nothing -> warningM "Chat.Application" (userName ++ " attempted to post to " ++ T.unpack roomName)
              _  -> return () -- no action, potentially logging

breakLoop = mzero

receiveJsonMessage connection = do
            received <- receiveData connection
            debugM "Chat.Application" $ T.unpack received
            return $ parseJsonMessage received

-- if not valid json: invalid msg
-- if valid json but message not parsed : chat msg
-- else: parsed command msg
parseJsonMessage received = do
            let mbMsgData = decodeStrict (encodeUtf8 received) :: Maybe ClientMessage
            case mbMsgData of
                Just clientMessage -> do let parsedMsg = parseMessage (T.unpack $ message clientMessage)
                                         either (toChatMessage clientMessage) id parsedMsg
                Nothing -> Invalid "json parsing error"


toChatMessage json _ = if isNothing (channel json) || isNothing (user json)
                         then Invalid "empty message"
                         else Message (fromJust $ channel json) (T.unpack (fromJust $ user json) ++ ": " ++ T.unpack (message json))

toJsonMessage = decodeUtf8 . toStrict . encode

-- keep track of users in room, and rooms a user is in.
addUserToRoom RoomState{..} UserState{..} roomName user@User{userName = userName} = atomically $ do
            roomMap <- readTVar rooms
            userMap <- readTVar users
            writeTVar rooms $ Map.adjust (addUser user) roomName roomMap
            writeTVar users $ Map.adjust (roomName:) userName userMap
            where addUser user Room{roomName = name, usersInRoom = usrs} = Room{roomName = name, usersInRoom = user:usrs }

addRoomIfNew RoomState{..} roomName = atomically $ do
            roomMap <- readTVar rooms
            when (Map.notMember roomName roomMap)
                 (writeTVar rooms $ Map.insert roomName (Room roomName []) roomMap)

-- think about transactions: atomically around all this?
removeUserFromAll roomState userState@UserState{..} user@User{userName = userName} = do
            userMap <- readTVarIO users
            let roomNames = fromMaybe [] $ Map.lookup userName userMap
            mapM_ (removeUserFromRoom roomState userState user) roomNames

removeUserFromRoom RoomState{..} UserState{..} user@User{userName = userName} roomName = atomically $ do
            roomMap <- readTVar rooms
            userMap <- readTVar users
            writeTVar rooms $ Map.adjust (removeUser user) roomName roomMap
            writeTVar users $ Map.adjust (removeRoom roomName) userName userMap
            where removeUser user Room{roomName = name, usersInRoom = usrs} = Room{roomName = name, usersInRoom = filter (\usr -> user /= usr) usrs}
                  removeRoom roomName = filter (\name -> roomName /= name)

-- send message to all users of a chat room
sendToRoom RoomState{..} roomName message = do
           roomMap <- readTVarIO rooms
           let mbRoom = Map.lookup roomName roomMap
               userConnections = maybe [] (map handle . usersInRoom) mbRoom
               json = toJsonMessage message
           mapM_ (`sendTextData` json) userConnections
