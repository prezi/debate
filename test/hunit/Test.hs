module Main where

import Test.HUnit hiding (test)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
--import Network.Wai.Test

--import Network.Wai


--import qualified Data.Map as Map

main =
  defaultMain [
    --testGroup "xhr-polling" [ testCase "open connection" testXHRtest ]
  ]

{-

testXHRtest = do let url = "/test/test/xhr"
                 flip runSession jsonpApp $ do
                     sres1 <- request defaultRequest
                                 { queryString = [("callback", Just "test")]
                                 , requestHeaders = [("Accept", "text/javascript")]
                                 }
                     assertContentType "text/javascript" sres1
                     assertBody "test({\"foo\":\"bar\"})" sres1
                 


testdata = "A unicode string\x2122"
url = "/clipboard/..."

ajaxRequest :: StdMethod -> B.ByteString -> Map.Map Text Text
               -> 
ajaxRequest method url datacontent = do
    let params = mapM_ (uncurry byName) $ Map.toList datacontent
    doRequestHeaders (renderStdMethod method)
                     url
                     [("X-Requested-With", "XMLHttpRequest")]
                     params

testXHRtest = do
      ajaxRequest GET url Map.empty
      bodyEquals $ map (chr . fromIntegral)
                       (B.unpack $ encodeUtf8 testdata)
      statusIs 200
-}
