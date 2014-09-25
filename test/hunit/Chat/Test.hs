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
  , testGroup "chatrooms" [ testCase "chat in lobby" caseChatInLobby
                          , testCase "join chat room" caseJoinChatroom ]
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

sendText input text = atomically $ putTMVar input text

retrieval (input, output, broadcast) = do
    retrievalLoop output broadcast
    where retrievalLoop output broadcast = do
            outputData <- atomically $ tryTakeTMVar output
            broadcastData <- atomically $ tryTakeTMVar broadcast
            if outputData == Nothing && broadcastData == Nothing
              then retrievalLoop output broadcast
              else return (outputData, broadcastData)

caseLoginSuccessful = do
    runTestApplication checkWorks $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN test test\"}"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "broadcasted data" (Just "{\"command\":\"login\",\"channel\":null,\"user\":\"test\",\"message\":null}") broadcastData

caseLogout = do
    runTestApplication checkWorks $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN test test\"}"
       _ <- retrieval conn
       sendText input "{\"channel\": \"Lobby\", \"message\": \"LOGOUT\"}"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "broadcasted data" (Just "{\"command\":\"logout\",\"channel\":null,\"user\":\"test\",\"message\":null}") broadcastData

caseChatInLobby = do
    runTestApplication checkWorks $ \conn1@(input,_,_) -> do
      sendText input "{\"message\":\"LOGIN user1 pass1\"}"
      _ <- retrieval conn1
      sendText input "{\"message\":\"I say something\", \"user\": \"user1\", \"channel\":\"Lobby\"}"
      (outputData, broadcastData) <- retrieval conn1
      assertEqual "incoming data" (Just "{\"command\":\"msg\", \"channel\":\"Lobby\", \"user\":\"user1\", \"message\": \"user1: I say something\"") outputData

caseJoinChatroom = do
    runTestApplication checkWorks $ \conn@(input,_,_) -> do
       sendText input "LOGIN user1 pass1"
       _ <- retrieval conn
       sendText input "JOIN room1"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "broadcasted data" (Just "test just logged in") broadcastData
