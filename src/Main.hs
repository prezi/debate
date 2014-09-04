{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forever)
import qualified Data.Text          as T
import Debate.Server

-- info query should return something sensible, and then move on to websocket
main = runServer Config {port = 8888, prefix = "/echo"} echo

echo receiveData sendTextData = forever $ do
    msg <- receiveData
    sendTextData msg
