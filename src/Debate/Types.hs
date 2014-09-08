{-# LANGUAGE OverloadedStrings #-}
module Debate.Types (
  Config(..)
, SessionId
, Client(..)
, ServerState(..)
, Frame(..)
, ControlFrame(..)
, SockConnection(..)
, frameToText
, newServerState
)
where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TChan
import qualified Data.Map.Strict as Map
import qualified Data.Text          as T

data Config = Config { port :: Int
                     , prefix :: T.Text
                     , transportWhitelist :: [T.Text] }

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
