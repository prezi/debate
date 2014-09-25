{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module Chat.Application (
  chat
, MessageData(..)
, ChatSecurity(..)
) where

import Control.Monad (mzero, forever, void)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import Control.Applicative (pure, empty)
import Data.Maybe (fromMaybe)
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

data MessageData = MessageData { user :: Maybe T.Text
                               , channel :: Maybe T.Text
                               , command :: Maybe ChatCommand
                               , message :: Maybe String }
                     deriving (Show, Generic)



data ChatCommand = LoginCommand | LogoutCommand | JoinCommand | MessageCommand
                   deriving (Show)

instance FromJSON MessageData
instance ToJSON MessageData

instance FromJSON ChatCommand where
  parseJSON (Object o) = case HML.lookup (T.pack "command") o of
                           Just (String t) -> fromString (TL.unpack (TL.fromStrict t))
                           _ -> empty
        where fromString "login" = pure LoginCommand
              fromString "logout" = pure LogoutCommand
              fromString "join" = pure JoinCommand
              fromString "msg" = pure MessageCommand
              fromString _ = empty

instance ToJSON ChatCommand where
  toJSON LoginCommand = String "login"
  toJSON LogoutCommand = String "logout"
  toJSON JoinCommand = String "join"
  toJSON MessageCommand = String "msg"

newRoomState = do newVar <- atomically $ newTVar (Map.insert mainChatRoom (Room mainChatRoom []) Map.empty)
                  return RoomState {rooms = newVar}
newUserState = do newVar <- atomically $ newTVar Map.empty
                  return UserState {users = newVar}
mainChatRoom = "Lobby"

chat ChatSecurity{..} connection = do
    roomState <- newRoomState
    userState <- newUserState
    forever $ do
        received <- receiveData connection
        debugM "Chat.Application" $ T.unpack received
        msg <- parseJsonMessage received
        case msg of
          Right (Login name pass) -> do credentials <- checkCredentials name pass
                                        case credentials of
                                          Just user -> void (loggedIn user connection roomState userState checkAccess)
                                          Nothing   -> sendTextData connection "login failed"
          _                       -> sendTextData connection (toJsonMessage MessageData {message = Just "Please log in first by doing LOGIN user passwd", channel = Nothing, user = Nothing, command = Nothing})

-- TODO: only broadcast to logged in users!
loggedIn userName connection roomState userState checkAccess = do
        let user = User (T.pack userName) connection
        addUserToRoom roomState userState mainChatRoom user
        warningM "Chat.Application" (userName ++ " just logged in")
        let loginMsg = MessageData { user = Just $ T.pack userName, channel = Nothing, command = Just LoginCommand, message = Nothing }
        broadcastData connection (toJsonMessage loginMsg)
        runMaybeT $ forever $ do
            received <- lift $ receiveData connection
            lift $ debugM "Chat.Application" $ T.unpack received
            msg <- parseJsonMessage received
            lift $ print msg
            case msg of
              Right Logout -> do let logoutMsg = MessageData { user = Just $ T.pack userName, channel = Nothing, command = Just LogoutCommand, message = Nothing }
                                 lift $ broadcastData connection (toJsonMessage logoutMsg)
                                 lift $ warningM "Chat.Application" (userName ++ " just logged out")
                                 lift $ removeUserFromAll roomState userState user
                                 breakLoop  -- leave loggedIn loop
              {-
              Right Join roomName -> do access <- checkAccess userName roomName
                                        case access of
                                          Just userName -> do let joinedMsg = MessageData { user = Just $ T.pack userName, channel = roomName, command = Just JoinCommand, message = Nothing }
                                                              addUserToRoom roomState userState roomName user
                                                              sendToRoom roomState joinedMsg
                                          Nothing -> WarningM "Chat.Application" (userName ++ " attempted to join " ++ roomName)
              -}
              _  -> return () -- no action, potentially logging

breakLoop = mzero

parseJsonMessage received = do
            let json = decodeStrict (encodeUtf8 received) :: Maybe MessageData
                mbMsg = maybe Nothing message json
                msg = maybe (Right $ Invalid "no message") parseMessage mbMsg -- message is Maybe String
            return msg

toJsonMessage = decodeUtf8 . toStrict . encode

-- keep track of users in room, and rooms a user is in.
addUserToRoom RoomState{..} UserState{..} roomName user@User{userName = userName} = atomically $ do
            roomMap <- readTVar rooms
            userMap <- readTVar users
            writeTVar rooms $ Map.adjust (addUser user) roomName roomMap
            writeTVar users $ Map.adjust (roomName:) userName userMap
            where addUser user Room{roomName = name, usersInRoom = usrs} = Room{roomName = name, usersInRoom = user:usrs }

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
