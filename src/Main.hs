{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
import           Control.Monad      (forever)
import           Control.Monad.Trans (liftIO)
import           System.Random (randomRIO)

import qualified Network.WebSockets as WS
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Blaze.ByteString.Builder (fromLazyByteString)

import qualified Data.Text          as T

import Data.Aeson


-- info query should return something sensible, and then move on to websocket
main = do
    let settings = Warp.setPort 8888 Warp.defaultSettings
    Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions application httpApplication

httpApplication :: Wai.Application
httpApplication req respond = do ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
                                 respond $ Wai.responseBuilder status200 
                                        [ ("Content-Type", "application/json") 
                                        , ("Access-Control-Allow-Credentials", "true")
                                        , ("Access-Control-Allow-Origin", "http://0.0.0.0:9999") ] -- to configure
                                      $ fromLazyByteString $ encode         [ "websocket"     .= True
                                                                            , "cookie_needed" .= True
                                                                            , "origins"       .= ["*:*" :: T.Text]
                                                                            , "entropy"       .= ent
                                                                            ]

application pending = do
    connection <- WS.acceptRequest pending
    echo connection

echo connection = forever $ do
    msg <- (WS.receiveData connection) :: IO T.Text
    WS.sendTextData connection $ msg
