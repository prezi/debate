{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Chat.Application (
  chat
, MessageData(..)
, ChatSecurity(..)
, newChatState
) where

import Control.Monad (mzero, forever, void, when, guard, unless)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Control.Exception (tryJust)
import Data.Maybe (fromJust, isNothing, fromMaybe)
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
                 | AlreadyJoined
                 | NotJoined
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
  toJSON AlreadyJoined = String "alreadyJoined"
  toJSON NotJoined = String "notJoined"

mainChatRoom = "Lobby"

chat ChatSecurity{..} chatState connection =
    forever $ do
        (_, _, msg) <- receiveJsonMessage connection
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
            (mbUsr, mbRoom, msg) <- lift $ receiveJsonMessage connection
            case msg of
              Logout -> when (mbUsr == Just (T.pack userName)) $ do
                          lift $ logout userName chatState user
                          left () -- exit
              _      -> when (mbUsr == Just (T.pack userName)) (lift $ sendIntendedMessage msg mbRoom userName chatState user checkAccess) -- only send if you are the intended recipient.

logout userName chatState user = do
        let logoutMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Nothing, command = LogoutCommand }
        sendToRoom chatState mainChatRoom logoutMsg
        warningM "Chat.Application" (userName ++ " just logged out")
        saveTVar chatState (removeUserFromAll user)


sendIntendedMessage :: ServerMessage -> Maybe T.Text -> String -> TVar ChatState -> User -> (String -> String -> IO (Maybe String)) -> IO ()
sendIntendedMessage msg mbRoom userName chatState user checkAccess =
        case msg of
            Join roomName -> do chState <- readTVarIO chatState
                                let tRoomname = T.pack roomName
                                if isUserInRoom chState user tRoomname
                                  then sendToRoom chatState tRoomname CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomname, command = AlreadyJoined }
                                  else joinRoom user roomName chatState checkAccess
            Leave roomName -> do chState <- readTVarIO chatState
                                 let tRoomName = T.pack roomName
                                 if isUserInRoom chState user tRoomName
                                   then leaveRoom user roomName chatState
                                   else sendToRoom chatState mainChatRoom CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just tRoomName, command = NotJoined }
            Login name pass -> do let alreadyLoggedInMsg = CommandMsg { commandMsgUser = Just $ T.pack userName, commandMsgChannel = Just mainChatRoom, command = AlreadyLoggedIn }
                                  sendToRoom chatState mainChatRoom alreadyLoggedInMsg
            Message str -> do let roomname = fromMaybe mainChatRoom mbRoom
                              access <- checkAccess userName (T.unpack roomname)
                              -- todo check if user joined as well
                              case access of
                                 Just userName -> do let chatMsg = MessageData { messageDataUser = T.pack userName, messageDataChannel = roomname, messageDataMessage = userName ++ ": " ++ str}
                                                     sendToRoom chatState roomname chatMsg
                                 Nothing -> warningM "Chat.Application" (userName ++ " attempted to post to " ++ T.unpack roomname)
            _  -> return () -- no action, potentially logging

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
                Nothing -> warningM "Chat.Application" (username ++ " attempted to join " ++ roomname)

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
parseJsonMessage received = do
            let mbMsgData = decodeStrict (encodeUtf8 received) :: Maybe ClientMessage
            case mbMsgData of
                Just clientMessage -> do let parsedMsg = parseMessage (T.unpack $ message clientMessage)
                                         case parsedMsg of
                                            Left _ -> (Nothing, Nothing, Invalid "invalid message")
                                            Right msg -> (user clientMessage, channel clientMessage, msg)
                Nothing -> (Nothing, Nothing, Invalid "json parsing error")

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
