import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS


main = WS.runServer "0.0.0.0" 8888 echo


echo pending = do
    connection <- WS.acceptRequest pending
    msg <- (WS.receiveData connection) :: IO T.Text
    WS.sendTextData connection $ msg
