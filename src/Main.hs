{-# LANGUAGE TemplateHaskell #-}
import           Control.Monad      (forever)
import qualified Network.WebSockets as WS

import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Blaze.ByteString.Builder (copyByteString)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.CaseInsensitive as CI ( mk )

import Data.Aeson


-- info query should return something sensible, and then move on to websocket
main = do
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 8888 } $ WaiWS.websocketsOr WS.defaultConnectionOptions application httpApplication

httpApplication :: Wai.Application
httpApplication req respond = respond $ Wai.responseBuilder status200 [(CI.mk (B.pack "Content-Type"), B.pack "application/json")] $ copyByteString (B.pack "")

{-
httpApplication :: Application
httpApplication req = responseBuilder status200 [("Content-Type", "application/json")] $ encode []
-}

application pending = do
    connection <- WS.acceptRequest pending
    echo connection

echo connection = forever $ do
    msg <- (WS.receiveData connection) :: IO T.Text
    WS.sendTextData connection $ msg
