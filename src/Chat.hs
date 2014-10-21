{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import Debate.Server
import Chat.Application
import System.IO (stderr)
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Network.Wai
import Network.Wai.Application.Static

import Data.String (fromString)
import Data.Text (pack)
import Data.Maybe (mapMaybe)
import WaiAppStatic.Types (toPiece)
import Data.FileEmbed (embedDir)

main :: IO ()
main = do
        setupLogger
        chatState <- newChatState
        runServer (setPort 8989 (setPrefix "/chat" defaultConfiguration)) (chat ChatSecurity {checkCredentials = checkUserPass, checkAccess = checkAccessToRoom} chatState) (Just intercept)

intercept :: Middleware
intercept app req respond = do
     let path = pathInfo req
     print path
     case path of
       [] -> staticApp (defaultFileServerSettings $ fromString "public/chat")
                   {
                      ssIndices = mapMaybe (toPiece . pack) ["index.html"]
                   } req respond
       ["status.html"] -> do putStrLn "here here"
                             staticApp (defaultFileServerSettings $ fromString "public/chat")
                                {
                                    ssIndices = mapMaybe (toPiece . pack) ["status.html"]
                                } req respond
       ["js", _] -> (staticApp $ embeddedSettings $(embedDir "public/chat") ) req respond
       _ ->  app req respond

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
