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
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, fromJust)

import Debate.Server
import Chat.State
import Chat.Application

chatSuite =
  [
    testGroup "login - logout" [ testCase "login successful" caseLoginSuccessful
                               , testCase "logout" caseLogout
                               , testCase "login twice" caseLoginTwice ]
  , testGroup "chatrooms" [ testCase "chat in lobby" caseChatInLobby
                          , testCase "join chat room feedback" caseJoinChatroomMsg
                          , testCase "join chat room" caseJoinChatroom
                          , testCase "join chat room" caseJoinChatroomTwice
                          , testCase "second user joins chat room" caseJoinChatroomTwoUsers
                          , testCase "two users chat in a chatroom" caseChatroomChat
                          , testCase "leave chat room" caseLeaveChatroom
                          , testCase "leave chat room when not previously joined" caseLeaveChatroomWhenNotJoined
                          , testCase "attempt to leave when not logged in" caseLeaveWhenNotLoggedIn ]
  , testGroup "state management" [ testCase "add user to room" caseAddUserToRoom
                                 , testCase "remove user from room" caseRemoveUserFromRoom
                                 , testCase "remove user from all rooms" caseRemoveUserFromAllRooms ]
  , testGroup "access" [ testCase "wrong credentials" caseWrongCredentials
                       , testCase "no access to chatroom" caseNoAccess ]
  , testGroup "status" [ testCase "status users and rooms" caseStatus ]
  ]

-- setup test helpers to be able to test sockjs apps independently from underlying sockjs handling


-- simplest possible thing -- test must be readable
newTestConnection input output broadcast = SockConnection { receiveData = atomically $ takeTMVar input, sendTextData = sendTest, broadcastData = broadcastTest }
   where sendTest text = atomically $ putTMVar output text
         broadcastTest text = atomically $ putTMVar broadcast text

type TestConnection = (TMVar T.Text, TMVar T.Text, TMVar T.Text)


runTestApplication security chatState assertions = do
    input <- newEmptyTMVarIO
    output <- newEmptyTMVarIO
    broadcast <- newEmptyTMVarIO
    let conn = newTestConnection input output broadcast
    _ <- forkIO (chat security chatState conn)
    assertions (input, output, broadcast)

-- pass in authentication method
checkPasses user _ = return $ Just user
checkFails _ _ = return $ Nothing
checkAccessPasses user _ = return $ Just user
checkAccessFails _ _ = return $ Nothing
allOKSecurity = ChatSecurity {checkCredentials = checkPasses, checkAccess = checkAccessPasses}
badCredentialsSecurity = ChatSecurity {checkCredentials = checkFails, checkAccess = checkAccessPasses}
noAccessSecurity = ChatSecurity {checkCredentials = checkPasses, checkAccess = checkAccessFails}

sendText input text = atomically $ putTMVar input text

-- 2.5 seconds should be enough to retrieve anything
retrieval (input, output, broadcast) = do
    retrievalLoop output broadcast (10 :: Int)
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
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN test test\"}"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "output data" (Just "{\"command\":\"login\",\"channel\":\"Lobby\",\"user\":\"test\"}") outputData

caseLogout = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN test test\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"test\", \"channel\": \"Lobby\", \"message\": \"LOGOUT\"}"
       (outputData, broadcastData) <- retrieval conn
       assertEqual "output data" (Just "{\"command\":\"logout\",\"channel\":null,\"user\":\"test\"}") outputData

caseChatInLobby = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
      sendText input "{\"message\": \"LOGIN user1 pass1\"}"
      _ <- retrieval conn
      sendText input "{\"message\":\"Msg I say something\", \"user\": \"user1\", \"channel\":\"Lobby\"}"
      (outputData, broadcastData) <- retrieval conn
      assertEqual "incoming data" (Just "{\"command\":\"msg\",\"channel\":\"Lobby\",\"user\":\"user1\",\"message\":\"user1: I say something\"}") outputData

caseJoinChatroomMsg = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"join\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData

-- note: tests polls internal state of app.
caseJoinChatroom = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       _ <- retrieval conn
       newState <- readTVarIO chatState
       let newRooms = fromMaybe [] (Map.lookup (T.pack "user1") (userState newState))
           newUsers = map userName $ usersInRoom $ fromJust $ Map.lookup (T.pack "room1") (roomState newState)
       assertBool "the user was not added to the room" (elem (T.pack "user1") newUsers)
       assertBool "the room was not added to the user" (elem (T.pack "room1") newRooms)

caseJoinChatroomTwice = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"alreadyJoined\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData

caseJoinChatroomTwoUsers = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn1@(input1,_,_) -> do
       sendText input1 "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn1
       runTestApplication allOKSecurity chatState $ \conn2@(input2,_,_) -> do
            sendText input2 "{\"message\": \"LOGIN user2 pass2\"}"
            _ <- retrieval conn1
            _ <- retrieval conn2
            sendText input1 "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
            _ <- retrieval conn1
            sendText input2 "{\"user\": \"user2\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
            _ <- retrieval conn2
            (outputData, _) <- retrieval conn1
            assertEqual "outputData" (Just "{\"command\":\"join\",\"channel\":\"room1\",\"user\":\"user2\"}") outputData

caseChatroomChat = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn1@(input1,_,_) -> do
       sendText input1 "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn1
       runTestApplication allOKSecurity chatState $ \conn2@(input2,_,_) -> do
            sendText input2 "{\"message\": \"LOGIN user2 pass2\"}"
            _ <- retrieval conn1
            _ <- retrieval conn2
            sendText input1 "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
            _ <- retrieval conn1
            sendText input2 "{\"user\": \"user2\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
            _ <- retrieval conn2
            _ <- retrieval conn1
            sendText input2 "{\"user\": \"user2\", \"channel\": \"room1\", \"message\": \"MSG hello\"}"
            (outputData, _) <- retrieval conn1
            assertEqual "msg in room1 from user2" (Just "{\"command\":\"msg\",\"channel\":\"room1\",\"user\":\"user2\",\"message\":\"user2: hello\"}") outputData

caseLeaveChatroomMsg = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"LEAVE room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"leave\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData

-- note: checks internal state of chatroom
caseLeaveChatroom = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"LEAVE room1\"}"
       _ <- retrieval conn
       newState <- readTVarIO chatState
       let newRooms = fromMaybe [] (Map.lookup (T.pack "user1") (userState newState))
           newUsers = map userName $ usersInRoom $ fromJust $ Map.lookup (T.pack "room1") (roomState newState)
       assertBool "the user was not removed from the room" (not $ elem (T.pack "user1") newUsers)
       assertBool "the room was not removed from the user" (not $ elem (T.pack "room1") newRooms)

caseLoginTwice = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"LOGIN user1 pass1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"alreadyLoggedIn\",\"channel\":\"Lobby\",\"user\":\"user1\"}") outputData

caseLeaveWhenNotLoggedIn = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"LEAVE room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"loginRequired\",\"channel\":null,\"user\":null}") outputData

caseLeaveChatroomWhenNotJoined = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"LEAVE room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"notJoined\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData

caseWrongCredentials = do
    chatState <- newChatState
    runTestApplication badCredentialsSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"loginFailed\",\"channel\":null,\"user\":null}") outputData

caseNoAccess = do
    chatState <- newChatState
    runTestApplication noAccessSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       (outputData, _) <- retrieval conn
       assertEqual "outputData" (Just "{\"command\":\"noAccess\",\"channel\":\"room1\",\"user\":\"user1\"}") outputData

caseStatus = do
    chatState <- newChatState
    runTestApplication allOKSecurity chatState $ \conn@(input,_,_) -> do
       sendText input "{\"message\": \"LOGIN user1 pass1\"}"
       _ <- retrieval conn
       sendText input "{\"user\": \"user1\", \"channel\": \"Lobby\", \"message\": \"JOIN room1\"}"
       _ <- retrieval conn
       runTestApplication allOKSecurity chatState $ \conn2@(input2,_,_) -> do
            sendText input2 "{\"message\": \"STATUS user2 pass2\"}"
            (outputData, _) <- retrieval conn2
            assertEqual "status output isn't correct" (Just "{\"loggedIn\":1,\"chatRooms\":1}") outputData

-- useless handle to add to user
dummyHandle = SockConnection { receiveData = return (T.pack "hello"), sendTextData = print, broadcastData = print } 

caseAddUserToRoom = do
    let emptyState = ChatState { userState = Map.empty, roomState = Map.empty } 
        user1 = T.pack "hobbes"
        room1 = T.pack "livingroom"
        newState = addUserToRoom room1 User {userName = user1, handle = dummyHandle} emptyState
        newRooms = fromMaybe [] (Map.lookup user1 (userState newState))
        newUsers = map userName $ usersInRoom $ fromJust $ Map.lookup room1 (roomState newState)
    print newState
    assertBool "the user was not added to the room" (elem user1 newUsers)
    assertBool "the room was not added to the user" (elem room1 newRooms)

caseRemoveUserFromRoom = do
    let username1 = T.pack "hobbes"
        user1 = User {userName = username1, handle = dummyHandle}
        room1 = T.pack "livingroom"
        state = ChatState { userState = Map.insert username1 [room1] Map.empty, roomState = Map.insert room1 (Room room1 [user1]) Map.empty }
        newState = removeUserFromRoom user1 room1 state
        newRooms = fromMaybe [] (Map.lookup username1 (userState newState))
        newUsers = map userName $ usersInRoom $ fromJust $ Map.lookup room1 (roomState newState)
    assertBool "the user was not removed from the room" (not $ elem username1 newUsers)
    assertBool "the room was not removed from the user" (not $ elem room1 newRooms)

caseRemoveUserFromAllRooms = do
    let username1 = T.pack "hobbes"
        user1 = User {userName = username1, handle = dummyHandle}
        username2 = T.pack "calvin"
        user2 = User {userName = username2, handle = dummyHandle}
        room1 = T.pack "livingroom"
        room2 = T.pack "kitchen"
        state = ChatState { userState = Map.insert username2 [room1, room2] (Map.insert username1 [room1] Map.empty),
                            roomState = Map.insert room2 (Room room2 [user2]) (Map.insert room1 (Room room1 [user1, user2]) Map.empty) }
        newState = removeUserFromAll user2 state
        newRooms2 = fromMaybe [] (Map.lookup username2 (userState newState))
        newUsers1 = map userName $ usersInRoom $ fromJust $ Map.lookup room1 (roomState newState)
        newUsers2 = map userName $ usersInRoom $ fromJust $ Map.lookup room2 (roomState newState)
    assertEqual "the rooms were not removed from the user" [] newRooms2
    assertBool "the user was not removed from room1" (not $ elem username2 newUsers1)
    assertBool "the user was not removed from room2" (not $ elem username2 newUsers2)
