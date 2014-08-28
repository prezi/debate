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

main =
  defaultMain [
    testGroup "basic sockjs" [ testCase "info request" caseInfoRequest ],
    testGroup "xhr-polling" [ testCase "open connection" caseXHRtest ]
  ]

-- helpers
echoApp connection = forever $ do
    msg <- receiveData connection :: IO T.Text
    sendTextData connection msg

get path = request $ setPath defaultRequest path
post path = request $ setPath (defaultRequest {requestMethod = "POST"}) path

-- test cases
caseInfoRequest = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    infoResponse <- get "/foo/info"
    assertStatus 200 infoResponse
    assertHeader "Content-type" "application/json; charset=UTF-8" infoResponse

caseXHRtest = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    openResponse <- post "/foo/000/aeiou/xhr"
    assertStatus 200 openResponse
    assertBody "o\n" openResponse
