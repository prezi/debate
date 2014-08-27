{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
import           Control.Monad      (forever)
import           Control.Monad.Trans (liftIO)
import           System.Random (randomRIO)
import           Data.Maybe (fromMaybe)

import qualified Network.WebSockets as WS
import qualified Network.HTTP.Types          as H
import qualified Network.HTTP.Types.Header   as H
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Blaze.ByteString.Builder (fromLazyByteString)

import qualified Data.Text          as T
import qualified Data.ByteString    as B

import Data.Aeson


-- info query should return something sensible, and then move on to websocket
main = do
    let settings = Warp.setPort 8888 Warp.defaultSettings
    Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions application httpApplication

-- TODO: add routing for '/info' and else 404
httpApplication :: Wai.Application
httpApplication req respond = do ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
                                 respond $ Wai.responseBuilder H.status200 
                                           (concat [headerJSON, headerNotCached, headerCORS "*" req])
                                         $ fromLazyByteString $ encode         [ "websocket"     .= True
                                                                               , "cookie_needed" .= False
                                                                               , "origins"       .= ["*:*" :: T.Text]
                                                                               , "entropy"       .= ent
                                                                               ]

application pending = do
    connection <- WS.acceptRequest pending
    echo connection

echo connection = forever $ do
    msg <- (WS.receiveData connection) :: IO T.Text
    WS.sendTextData connection $ msg


headerJSON :: H.ResponseHeaders
headerJSON = [("Content-Type", "application/json; charset=UTF-8")]

headerNotCached :: H.ResponseHeaders
headerNotCached = [("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")]

headerCORS :: B.ByteString -> Wai.Request -> H.ResponseHeaders
headerCORS def req = allowHeaders ++ allowOrigin ++ allowCredentials
    where allowCredentials = [("Access-Control-Allow-Credentials", "true")]
          allowHeaders =
              case lookup "Access-Control-Request-Headers" $ Wai.requestHeaders req of
                   Just "" -> []
                   Just ah -> [("Access-Control-Allow-Headers", ah)]
                   Nothing -> []
          allowOrigin =
              case origin of
                   ""     -> []
                   "null" -> [("Access-Control-Allow-Origin", def)]
                   _      -> [("Access-Control-Allow-Origin", origin)]
          origin = fromMaybe def . lookup "Origin" $ Wai.requestHeaders req
