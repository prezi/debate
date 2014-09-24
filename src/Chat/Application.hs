{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Chat.Application (
  chat
, MessageData(..)
) where

import Control.Monad (mzero, forever, void)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Control.Applicative (pure, empty)
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

data User = User { name :: T.Text
                 , handle :: SockConnection }

-- logged in users
data UserState = TVar (Map.Map T.Text User)

data MessageData = MessageData { user :: Maybe T.Text
                               , channel :: Maybe T.Text
                               , command :: Maybe ChatCommand
                               , message :: Maybe String }
                     deriving (Show, Generic)



data ChatCommand = LoginCommand | LogoutCommand
                   deriving (Show)

instance FromJSON MessageData
instance ToJSON MessageData

instance FromJSON ChatCommand where
  parseJSON (Object o) = case HML.lookup (T.pack "command") o of
                           Just (String t) -> fromString (TL.unpack (TL.fromStrict t))
                           _ -> empty
        where fromString "login" = pure LoginCommand
              fromString "logout" = pure LogoutCommand
              fromString _ = empty

instance ToJSON ChatCommand where
  toJSON LoginCommand = String "login"
  toJSON LogoutCommand = String "logout"

chat checkCredentials connection = forever $ do
        received <- receiveData connection
        print received
        msg <- parseJsonMessage received
        case msg of
          Right (Login name pass) -> case checkCredentials name pass of
                                       Just user -> void (loggedIn user connection)
                                       Nothing   -> sendTextData connection "login failed"
          _                       -> sendTextData connection (toJsonMessage MessageData {message = Just "Please log in first by doing LOGIN user passwd", channel = Nothing, user = Nothing, command = Nothing})

-- TODO: only broadcast to logged in users!
loggedIn user connection = do
        warningM "Chat.Application" (user ++ " just logged in")
        let loginMsg = MessageData { user = Just $ T.pack user, channel = Nothing, command = Just LoginCommand, message = Nothing }
        broadcastData connection (toJsonMessage loginMsg)
        runMaybeT $ forever $ do
            received <- lift $ receiveData connection
            msg <- parseJsonMessage received
            case msg of
              Right Logout -> do let logoutMsg = MessageData { user = Just $ T.pack user, channel = Nothing, command = Just LogoutCommand, message = Nothing }
                                 lift $ broadcastData connection (toJsonMessage logoutMsg)
                                 lift $ warningM "Chat.Application" (user ++ " just logged out")
                                 breakLoop  -- leave loggedIn loop
              _  -> lift $ broadcastData connection (T.concat [T.pack user, ": ", received])

breakLoop = mzero

parseJsonMessage received = do
            let json = decodeStrict (encodeUtf8 received) :: Maybe MessageData
                mbMsg = maybe Nothing message json
                msg = maybe (Right $ Invalid "no message") parseMessage mbMsg -- message is Maybe String
            return msg

toJsonMessage = decodeUtf8 . toStrict . encode
