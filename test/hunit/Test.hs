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

echoApp connection = forever $ do
    msg <- receiveData connection :: IO T.Text
    sendTextData connection msg

caseInfoRequest = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    infoResponse <- request defaultRequest
                { requestMethod = "GET"
                , rawPathInfo = "/foo/info"
                }
    assertStatus 200 infoResponse
    assertHeader "Content-type" "application/json; charset=UTF-8" infoResponse

caseXHRtest = flip runSession (httpApplication Config {port = 8881, prefix = "/foo"} echoApp) $ do
    openResponse <- request defaultRequest
                { requestMethod = "POST"
                , rawPathInfo = "/foo/000/session/xhr"
                }
    assertStatus 200 openResponse
    assertBody "o\n" openResponse
