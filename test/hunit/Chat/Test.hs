{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Chat.Test (
  chatSuite
) where

import Test.HUnit hiding (test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework

import Control.Concurrent (forkIO)
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
  {-
  , testGroup "chatrooms" [ testCase "chat in lobby" caseChatInLobby
                          , testCase "join chat room" caseJoinChatroom ]
  -}
  ]

-- setup test helpers to be able to test sockjs apps independently from underlying sockjs handling


-- simplest possible thing -- test must be readable
newTestConnection input output broadcast = SockConnection { receiveData = atomically $ takeTMVar input, sendTextData = sendTest, broadcastData = broadcastTest }
   where sendTest text = atomically $ putTMVar output text
         broadcastTest text = atomically $ putTMVar broadcast text

type TestConnection = (TMVar T.Text, TMVar T.Text, TMVar T.Text)


runTestApplication checkCredentials assertions = do
    input <- newEmptyTMVarIO
    output <- newEmptyTMVarIO
    broadcast <- newEmptyTMVarIO
    let conn = newTestConnection input output broadcast
    _ <- forkIO (chat checkCredentials conn)
    assertions (input, output, broadcast)

-- pass in authentication method
checkWorks user pass = Just user

retrieval (input, output, broadcast) text = do
    atomically $ putTMVar input text
    retrievalLoop output broadcast
    where retrievalLoop output broadcast = do
            outputData <- atomically $ tryTakeTMVar output
            broadcastData <- atomically $ tryTakeTMVar broadcast
            if outputData == Nothing && broadcastData == Nothing
              then retrievalLoop output broadcast
              else return (outputData, broadcastData)

caseLoginSuccessful = do
    runTestApplication checkWorks $ \conn -> do
       (outputData, broadcastData) <- retrieval conn "{\"message\": \"LOGIN test test\"}"
       assertEqual "broadcasted data" (Just "{\"command\":\"login\",\"channel\":null,\"user\":\"test\",\"message\":null}") broadcastData

caseLogout = do
    runTestApplication checkWorks $ \conn -> do
       _ <- retrieval conn "{\"message\": \"LOGIN test test\"}"
       (outputData, broadcastData) <- retrieval conn "{\"channel\": \"Lobby\", \"message\": \"LOGOUT\"}"
       assertEqual "broadcasted data" (Just "{\"command\":\"logout\",\"channel\":null,\"user\":\"test\",\"message\":null}") broadcastData

{-
caseChatInLobby = do
    runTestApplication checkWorks $ \conn1-> do
      _ <- retrieval conn1 "LOGIN user1 pass1"
      runTestApplication $ \conn2 -> do
        _ <- retrieval conn2 "LOGIN user2 pass2"
        _ <- retrieval conn2 "I say something"
        (_, broadcastData) <- retrieval conn2 "I say something"
        assertEqual "broadcasted data" (Just "user2: I say something") broadcastData

caseJoinChatroom = do
    runTestApplication checkWorks $ \conn -> do
       _ <- retrieval conn "LOGIN user1 pass1"
       (outputData, broadcastData) <- retrieval conn "JOIN room1"
       assertEqual "broadcasted data" (Just "test just logged in") broadcastData
-}
