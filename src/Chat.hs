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
        runServer (setPort 8989 (setPrefix "/chat" defaultConfiguration)) chat

setupLogger = do
        myStreamHandler <- streamHandler stderr INFO
        let myStreamHandler' = setFormatter myStreamHandler (simpleLogFormatter "[$time $loggername $prio] $msg")
        updateGlobalLogger rootLoggerName (setLevel INFO)
        updateGlobalLogger rootLoggerName (setHandlers [myStreamHandler'])
