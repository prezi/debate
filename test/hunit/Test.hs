{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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

main =
  defaultMain [
    testGroup "basic sockjs" [ testCase "info request" caseInfoRequest ]
  , testGroup "xhr-polling" [ testCase "open connection" caseXHRopen
                            , testCase "receive message" caseXHRReceiveMessage
                            , testCase "receive and send message" caseXHRReceiveSendMessage
                            , testCase "try to send message on unopened session" caseXHRBadMessage ]
  ]

-- helpers
echoApp connection = forever $ do
    msg <- receiveData connection :: IO T.Text
    sendTextData connection msg

get path = request $ setPath defaultRequest path
post path content = request $ setPath (defaultRequest {requestMethod = "POST", queryString = content}) path


-- test cases
caseInfoRequest = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    infoResponse <- get "/foo/info"
    assertStatus 200 infoResponse
    assertHeader "Content-type" "application/json; charset=UTF-8" infoResponse

caseXHRopen = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    openResponse <- post "/foo/000/aeiou/xhr" []
    assertStatus 200 openResponse
    assertBody "o\n" openResponse

caseXHRReceiveMessage = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    -- prerequisite: open session
    _ <- post "/foo/000/aeiou/xhr" []
    sendResponse <- post "/foo/000/aeiou/xhr_send" [("body", Just "a[\"test\"]")]
    assertStatus 204 sendResponse

caseXHRReceiveSendMessage = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    -- prerequisite: open session
    _ <- post "/foo/000/aeiou/xhr" []
    sendResponse <- post "/foo/000/aeiou/xhr_send" [("body", Just "a[\"test\"]")]
    -- test app is echo so we receive message back
    receiveResponse <- post "/foo/000/aeiou/xhr" []
    assertBody "a[\"test\"]" receiveResponse

caseXHRBadMessage = undefined
