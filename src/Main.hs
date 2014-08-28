import           Control.Monad      (forever)
import qualified Data.Text          as T
import Debate.Server

-- info query should return something sensible, and then move on to websocket
main = runServer Config {port = 8888} echo

echo connection = forever $ do
    msg <- receiveData connection :: IO T.Text
    sendTextData connection msg
