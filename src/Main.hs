{-# LANGUAGE TemplateHaskell #-}
import           Control.Monad      (forever)
import           Control.Monad.Trans (liftIO)
import           System.Random (randomRIO)

import qualified Network.WebSockets as WS
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Blaze.ByteString.Builder (copyByteString, fromLazyByteString)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text          as T
import qualified Data.CaseInsensitive as CI ( mk )

import Data.Aeson

import Debug.Trace


-- info query should return something sensible, and then move on to websocket
main = do
    let settings = Warp.setPort 8888 Warp.defaultSettings
    Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions application httpApplication

--httpApplication :: Wai.Application

httpApplication a b | trace ("http " ++ show (Wai.requestHeaders a) ++ " " ++  show (Wai.pathInfo a)) False = undefined
httpApplication req respond = do ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
                                 respond $ Wai.responseBuilder status200 
                                        [ (CI.mk (B.pack "Content-Type"), B.pack "application/json") 
                                        , (CI.mk (B.pack "Access-Control-Allow-Credentials"), B.pack "true")
                                        , (CI.mk (B.pack "Access-Control-Allow-Origin"), B.pack "http://0.0.0.0:9999") ] -- to configure
                                      $ fromLazyByteString $ encode         [ (T.pack "websocket")     .= True
                                                                            , (T.pack "cookie_needed") .= True
                                                                            , (T.pack "origins")       .= ["*:*"]
                                                                            , (T.pack "entropy")       .= ent
                                                                            ]

application pending = do
    connection <- WS.acceptRequest pending
    echo connection

echo connection = forever $ do
    msg <- (WS.receiveData connection) :: IO T.Text
    WS.sendTextData connection $ msg
