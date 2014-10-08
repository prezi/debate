{-# LANGUAGE OverloadedStrings #-}

import Debate.Server
import Chat.Application
import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

main :: IO ()
main = do
        setupLogger
        chatState <- newChatState
        runServer (setPort 8989 (setPrefix "/chat" defaultConfiguration)) (chat ChatSecurity {checkCredentials = checkUserPass, checkAccess = checkAccessToRoom} chatState)

-- pass in authentication and access
checkUserPass :: String -> String -> IO (Maybe String)
checkUserPass userName _ = return $ Just userName
checkAccessToRoom :: String -> String -> IO (Maybe String)
checkAccessToRoom userName _ = return $ Just userName

setupLogger :: IO ()
setupLogger = do
        myStreamHandler <- streamHandler stderr DEBUG
        let myStreamHandler' = setFormatter myStreamHandler (simpleLogFormatter "[$time $loggername $prio] $msg")
        updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler'])
