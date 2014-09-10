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

import Debate.Server
import Chat.Application

chatSuite =
  [
    testGroup "login" [ testCase "login successful" caseLoginSuccessful ]
  ]

-- setup test helpers to be able to test sockjs apps independently from underlying sockjs handling


-- simplest possible thing -- test must be readable
newTestConnection input output broadcast = SockConnection { receiveData = atomically $ takeTMVar input, sendTextData = sendTest, broadcastData = broadcastTest }
   where sendTest text = atomically $ putTMVar output text
         broadcastTest text = atomically $ putTMVar broadcast text


runTestApplication input assertions = do
    input <- newTMVarIO input
    output <- newEmptyTMVarIO
    broadcast <- newEmptyTMVarIO
    let conn = newTestConnection input output broadcast
    _ <- forkIO (chat conn)
    (outputData, broadcastData) <- retrievalLoop output broadcast
    assertions outputData broadcastData
    where retrievalLoop output broadcast = do
            outputData <- atomically $ tryTakeTMVar output
            broadcastData <- atomically $ tryTakeTMVar broadcast
            if outputData == Nothing && broadcastData == Nothing
              then retrievalLoop output broadcast
              else return (outputData, broadcastData)

caseLoginSuccessful = do
    runTestApplication "LOGIN test test" $ \outputData broadcastData ->
       assertEqual "broadcasted data" (Just "test just logged in") broadcastData
