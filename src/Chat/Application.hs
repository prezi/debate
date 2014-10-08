{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Chat.Application (
  chat
, MessageData(..)
, ChatSecurity(..)
, newChatState
) where

import Control.Monad (mzero, forever, void, when, guard)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Control.Exception (tryJust)
import Data.Maybe (fromJust, isNothing)
import Data.Aeson (FromJSON(..), ToJSON(..), decodeStrict, encode, Value(..), (.=), object)
import GHC.Generics (Generic) -- generics
import Chat.Message
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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
                     deriving Show

data ChatCommand = LoginCommand
                 | LogoutCommand
                 | JoinCommand
                 | LeaveCommand
                 | MessageCommand
                 | LoginRequired
                 | LoginFailed
                 | AlreadyLoggedIn
                   deriving (Show)

instance FromJSON ClientMessage

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

instance ToJSON ChatCommand where
  toJSON LoginCommand = String "login"
  toJSON LogoutCommand = String "logout"
  toJSON JoinCommand = String "join"
  toJSON LeaveCommand = String "leave"
  toJSON MessageCommand = String "msg"
  toJSON LoginFailed = String "loginFailed"
  toJSON LoginRequired = String "loginRequired"
  toJSON AlreadyLoggedIn = String "alreadyLoggedIn"

mainChatRoom = "Lobby"

chat ChatSecurity{..} chatState connection =
    forever $ do
        (_, msg) <- receiveJsonMessage connection
        case msg of
          Login name pass -> do credentials <- checkCredentials name pass
                                case credentials of
                                    Just user -> void (loggedIn user connection chatState checkAccess)
                                    Nothing   -> sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginFailed })
          _               -> sendTextData connection (toJsonMessage CommandMsg {commandMsgChannel = Nothing, commandMsgUser = Nothing, command = LoginRequired})

-- TODO: only broadcast to logged in users!
-- TODO: check if user name in message matches username in state
loggedIn userName connection chatState checkAccess = do
        let user = User (T.pack userName) connection
        saveTVar chatState $ addUserToRoom mainChatRoom user
        warningM "Chat.Application" (userName ++ " just logged in")
        chatstate <- readTVarIO chatState
        let loginMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just mainChatRoom, command = LoginCommand }
        sendToRoom chatState mainChatRoom loginMsg
        runEitherT $ forever $ do
            (mbUsr, msg) <- lift $ receiveJsonMessage connection
            case msg of
              Logout -> when (mbUsr == Just (T.pack userName)) $ do
                          lift $ logout userName chatState user
                          left () -- exit
              _      -> when (mbUsr == Just (T.pack userName)) (lift $ sendIntendedMessage msg userName chatState user checkAccess) -- only send if you are the intended recipient.

logout userName chatState user = do
        let logoutMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Nothing, command = LogoutCommand }
        sendToRoom chatState mainChatRoom logoutMsg
        warningM "Chat.Application" (userName ++ " just logged out")
        saveTVar chatState (removeUserFromAll user)


sendIntendedMessage :: ServerMessage -> String -> TVar ChatState -> User -> (String -> String -> IO (Maybe String)) -> IO ()
sendIntendedMessage msg userName chatState user checkAccess =
        case msg of
            Join roomName -> do access <- checkAccess userName roomName
                                case access of
                                    Just userName -> do let tRoomName = T.pack roomName
                                                            joinedMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomName, command = JoinCommand }
                                                        saveTVar chatState $ addUserToRoom tRoomName user
                                                        sendToRoom chatState tRoomName joinedMsg
                                                        warningM "Chat.Application" (userName ++ " joined " ++ roomName)
                                    Nothing -> warningM "Chat.Application" (userName ++ " attempted to join " ++ roomName)
            Leave roomName -> do let tRoomName = T.pack roomName
                                     leaveMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomName, command = LeaveCommand }
                                 -- TODO cannot leave mainChatRoom
                                 sendToRoom chatState tRoomName leaveMsg
                                 saveTVar chatState (removeUserFromRoom user tRoomName)
                                 warningM "Chat.Application" (userName ++ " left " ++ roomName)
            Login name pass -> do let alreadyLoggedInMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just mainChatRoom, command = AlreadyLoggedIn }
                                  sendToRoom chatState mainChatRoom alreadyLoggedInMsg
            Message roomName str -> do access <- checkAccess userName (T.unpack roomName)
                                       -- todo check if user joined as well
                                       case access of
                                           Just userName -> do let chatMsg = MessageData { messageDataUser = T.pack userName, messageDataChannel = roomName, messageDataMessage = str}
                                                               sendToRoom chatState roomName chatMsg
                                           Nothing -> warningM "Chat.Application" (userName ++ " attempted to post to " ++ T.unpack roomName)
            _  -> return () -- no action, potentially logging

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
                                         either (toChatMessage clientMessage) ((,) (user clientMessage)) parsedMsg
                Nothing -> (Nothing, Invalid "json parsing error")


toChatMessage json _ = if isNothing (channel json) || isNothing (user json)
                         then (Nothing, Invalid "empty message")
                         else do let ch = fromJust $ channel json
                                     usr = fromJust $ user json
                                 (user json, Message ch (T.unpack usr ++ ": " ++ T.unpack (message json)))

toJsonMessage = decodeUtf8 . toStrict . encode

-- send message to all users of a chat room
-- handle cleanup of closed clients!
sendToRoom :: TVar ChatState -> T.Text -> MessageData -> IO ()
sendToRoom tvarChatState roomName message = do
           chatState <- readTVarIO tvarChatState
           let mbRoom = Map.lookup roomName (roomState chatState)
               users = maybe [] usersInRoom mbRoom
               json = toJsonMessage message
           mapM_ (sendIfClientPresent tvarChatState roomName json) users
           where sendIfClientPresent tvarChatState roomName json user = do
                      err <- tryJust (guard . (== ClientNotFoundException)) $ sendTextData (handle user) json
                      case err of
                        Left _ -> do warningM "Chat.Application" (T.unpack (userName user) ++ " disconnected")
                                     saveTVar tvarChatState (removeUserFromRoom user roomName)
                        Right _ -> return ()
