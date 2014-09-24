{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Debate.Test (
  debateSuite
)
where

import Test.HUnit hiding (test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework

import Debate.Server

import Network.Wai
import Network.Wai.Test
import qualified Data.Text as T
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import Control.Concurrent.STM.TVar
import GHC.Conc.Sync (atomically)
import qualified Data.Map.Strict as Map

debateSuite = [
                testGroup "basic sockjs" [ testCase "info request" caseInfoRequest ]
              , testGroup "xhr-polling" [ testCase "open connection" caseXHRopen
                                        , testCase "receive message" caseXHRReceiveMessage
                                        , testCase "receive and send message" caseXHRReceiveSendMessage
                                        , testCase "try to send message on unopened session" caseXHRBadMessage
                                        , testCase "broadcast" caseXHRBroadcast ]
               ]

-- helpers
echoApp connection = forever $ do
    msg <- receiveData connection
    sendTextData connection msg

broadcastApp connection = do
    broadcastData connection "hello"
    forever $ do
        msg <- receiveData connection
        sendTextData connection msg

get path = srequest $ SRequest (setPath defaultRequest path) ""
post path content = srequest $ SRequest ((setPath defaultRequest path) {requestMethod = "POST", requestHeaders = [("Content-Type","application/x-www-form-urlencoded")]}) (L.fromChunks [content])

emptyState = do newVar <- atomically $ newTVar Map.empty
                return $ ServerState {clients = newVar}

-- test cases
caseInfoRequest = do
    state <- emptyState
    flip runSession (httpApplication (setPort 8881 (setPrefix "/foo" defaultConfiguration)) echoApp state) $ do
        infoResponse <- get "/foo/info"
        assertStatus 200 infoResponse
        assertHeader "Content-type" "application/json; charset=UTF-8" infoResponse

caseXHRopen = do
    state <- emptyState
    flip runSession (httpApplication (setPort 8881 (setPrefix"/foo" defaultConfiguration)) echoApp state) $ do
        openResponse <- post "/foo/000/aeiou/xhr" ""
        assertStatus 200 openResponse
        assertBody "o\n" openResponse

caseXHRReceiveMessage = do
    state <- emptyState
    flip runSession (httpApplication (setPort 8881 (setPrefix"/foo" defaultConfiguration)) echoApp state) $ do
        -- prerequisite: open session
        _ <- post "/foo/000/aeiou/xhr" ""
        sendResponse <- post "/foo/000/aeiou/xhr_send" "[\"test\"]"
        assertStatus 204 sendResponse

caseXHRReceiveSendMessage = do
    state <- emptyState
    flip runSession (httpApplication (setPort 8881 (setPrefix"/foo" defaultConfiguration)) echoApp state) $ do
        -- prerequisite: open session
        _ <- post "/foo/000/aeiou/xhr" ""
        sendResponse <- post "/foo/000/aeiou/xhr_send" "[\"test\"]"
        -- test app is echo so we receive message back
        receiveResponse <- post "/foo/000/aeiou/xhr" ""
        assertBody "a[\"test\"]\n" receiveResponse

caseXHRBadMessage = do
    state <- emptyState
    flip runSession (httpApplication (setPort 8881 (setPrefix"/foo" defaultConfiguration)) echoApp state) $ do
        sendResponse <- post "/foo/000/aeiou/xhr_send" "[\"test\"]"
        assertStatus 404 sendResponse

caseXHRBroadcast = do
    state <- emptyState
    flip runSession (httpApplication (setPort 8881 (setPrefix"/foo" defaultConfiguration)) broadcastApp state) $ do
        _ <- post "/foo/000/aeiou/xhr" ""
        -- broadcast app broadcasts on open of session
        receiveResponse <- post "/foo/000/aeiou/xhr" ""
        assertBody "a[\"hello\"]\n" receiveResponse
