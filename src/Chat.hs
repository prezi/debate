{-# LANGUAGE OverloadedStrings #-}

import Debate.Server
import Chat.Application
import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

main = do
        setupLogger
        roomState <- newRoomState
        userState <- newUserState
        runServer (setPort 8989 (setPrefix "/chat" defaultConfiguration)) (chat ChatSecurity {checkCredentials = checkUserPass, checkAccess = checkAccessToRoom} userState roomState)

-- pass in authentication and access
checkUserPass :: String -> String -> IO (Maybe String)
checkUserPass userName pass = return $ Just userName
checkAccessToRoom :: String -> String -> IO (Maybe String)
checkAccessToRoom userName _ = return $ Just userName

setupLogger = do
        myStreamHandler <- streamHandler stderr INFO
        let myStreamHandler' = setFormatter myStreamHandler (simpleLogFormatter "[$time $loggername $prio] $msg")
        updateGlobalLogger rootLoggerName (setLevel DEBUG)
        updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler'])
