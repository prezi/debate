{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import           Control.Monad      (forever)
import qualified Data.Text          as T
import Debate.Server
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic) -- generics

main = runServer (setPort 8989 (setPrefix "/chat" defaultConfiguration)) chat

chat connection = do
    broadcastData connection "hello"
    forever $ do
        received <- receiveData connection
        -- let msg = decode received :: Maybe ChatMsg
        sendTextData connection received



data Room = Common | ChatRoom T.Text
            deriving (Show, Generic)

data ChatMsg = Broadcast Room String
               deriving (Show, Generic)
