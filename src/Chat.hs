{-# LANGUAGE OverloadedStrings #-}
import Debate.Server
import Control.Monad (when, mzero, forever, void)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM.TVar
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic) -- generics
import Chat.Message
import qualified Data.Map.Strict as Map
import qualified Data.Text          as T

import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

data User = User { name :: T.Text
                 , handle :: SockConnection }

-- logged in users
data UserState = TVar (Map.Map T.Text User)

main = do
        setupLogger
        runServer (setPort 8989 (setPrefix "/chat" defaultConfiguration)) chat


setupLogger = do
        myStreamHandler <- streamHandler stderr INFO
        let myStreamHandler' = setFormatter myStreamHandler (simpleLogFormatter "[$time $loggername $prio] $msg")
        updateGlobalLogger rootLoggerName (setLevel INFO)
        updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler'])

chat connection = forever $ do
        received <- receiveData connection
        let msg = parseMessage (T.unpack received)
        case msg of
          Right (Login name pass) -> case checkCredentials name pass of
                                       Just user -> void (loggedIn user connection)
                                       Nothing   -> sendTextData connection "login failed"
          otherwise               -> sendTextData connection "Please log in first by doing LOGIN user passwd"

-- log in ok for now
checkCredentials user pass = Just user

loggedIn user connection = do
        warningM "Chat" (user ++ " just logged in")
        broadcastData connection (T.concat [T.pack user, " just logged in"])
        runMaybeT $ forever $ do
            received <- lift $ receiveData connection
            case parseMessage (T.unpack received) of
              Right Logout -> do lift $ broadcastData connection (T.concat [T.pack user, " just logged out"])
                                 breakLoop  -- leave loggedIn loop
              otherwise    -> lift $ sendTextData connection (T.concat [T.pack user, ": ", received])

breakLoop = mzero
