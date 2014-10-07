{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Test (
  chatSuite
) where

import Test.HUnit hiding (test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan

import qualified Data.Text as T

import Debate.Server
import Chat.Application

chatSuite =
  [
    testGroup "login - logout" [ testCase "login successful" caseLoginSuccessful
                               , testCase "logout" caseLogout ]
  , testGroup "chatrooms" [ testCase "chat in lobby" caseChatInLobby
                          , testCase "join chat room" caseJoinChatroom
                          , testCase "second user joins chat room" caseJoinChatroomTwoUsers
                          , testCase "leave chat room" caseLeaveChatroom ]
  ]

-- setup test helpers to be able to test sockjs apps independently from underlying sockjs handling


-- simplest possible thing -- test must be readable
newTestConnection input output broadcast = SockConnection { receiveData = atomically $ takeTMVar input, sendTextData = sendTest, broadcastData = broadcastTest }
   where sendTest text = atomically $ putTMVar output text
         broadcastTest text = atomically $ putTMVar broadcast text

type TestConnection = (TMVar T.Text, TMVar T.Text, TMVar T.Text)


runTestApplication security userState roomState assertions = do
    input <- newEmptyTMVarIO
    output <- newEmptyTMVarIO
    broadcast <- newEmptyTMVarIO
    let conn = newTestConnection input output broadcast
    _ <- forkIO (chat security userState roomState conn)
    assertions (input, output, broadcast)

-- pass in authentication method
checkPasses user pass = return $ Just user
checkAccessPasses user _ = return $ Just user
allOKSecurity = ChatSecurity {checkCredentials = checkPasses, checkAccess = checkAccessPasses}

sendText input text = atomically $ putTMVar input text

-- 2.5 seconds should be enough to retrieve anything
retrieval (input, output, broadcast) = do
    retrievalLoop output broadcast 10
    where retrievalLoop _ _ 0 = do putStrLn "** message retrieval giving up **"
                                   return (Nothing, Nothing)
          retrievalLoop output broadcast n = do
            outputData <- atomically $ tryTakeTMVar output
            broadcastData <- atomically $ tryTakeTMVar broadcast
            if outputData == Nothing && broadcastData == Nothing
              then do threadDelay 250000
                      retrievalLoop output broadcast (n-1)
              else return (outputData, broadcastData)

caseLoginSuccessful = do
    userState <- newUserState
    roomState <- newRoomState
    runTestApplication allOKSecurity userState roomState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN test test\"}"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "output data" (Just "{\"command\":\"login\",\"channel\":\"Lobby\",\"user\":\"test\"}") outputData

caseLogout = do
    userState <- newUserState
    roomState <- newRoomState
    runTestApplication allOKSecurity userState roomState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN test test\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"test\", \"channel\": \"Lobby\", \"message\": \"LOGOUT\"}"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "output data" (Just "{\"command\":\"logout\",\"channel\":null,\"user\":\"test\"}") outputData

caseChatInLobby = do
    userState <- newUserState
    roomState <- newRoomState
    runTestApplication allOKSecurity userState roomState $ \conn@(input,_,_) -> do
      sendText input "{\"message\": \"LOGIN user1 pass1\"}"
      _ <- retrieval conn
      sendText input "{\"message\":\"I say something\", \"user\": \"user1\", \"channel\":\"Lobby\"}"
      (outputData, broadcastData) <- retrieval conn
      assertEqual "incoming data" (Just "{\"command\":\"msg\",\"channel\":\"Lobby\",\"user\":\"user1\",\"message\":\"user1: I say something\"}") outputData

caseJoinChatroom = do
    userState <- newUserState
    roomState <- newRoomState
    runTestApplication allOKSecurity userState roomState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"join\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData

caseJoinChatroomTwoUsers = do
    userState <- newUserState
    roomState <- newRoomState
    runTestApplication allOKSecurity userState roomState $ \conn1@(input1,_,_) -> do
       sendText input1 "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn1
       runTestApplication allOKSecurity userState roomState $ \conn2@(input2,_,_) -> do
            sendText input2 "{\"message\": \"LOGIN user2 pass2\"}"
            _ <- retrieval conn1
            _ <- retrieval conn2
            sendText input1 "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
            _ <- retrieval conn1
            sendText input2 "{\"user\": \"user2\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
            _ <- retrieval conn2
            (outputData, _) <- retrieval conn1
            assertEqual "outputData" (Just "{\"command\":\"join\",\"channel\":\"room1\",\"user\":\"user2\"}") outputData

caseLeaveChatroom = do
    userState <- newUserState
    roomState <- newRoomState
    runTestApplication allOKSecurity userState roomState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"LEAVE room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"leave\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData
