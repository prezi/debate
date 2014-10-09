{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forever)
import qualified Data.Text          as T
import Debate.Server

import System.Environment (getArgs)
import System.Console.GetOpt

main = do
   args <- getArgs
   let (flags, nonOpts, msgs) = getOpt RequireOrder options args
   if Transport "xhr" `elem` flags
     then runServer (setPrefix "/echo" (setTransportWhitelist ["xhr_polling"] defaultConfiguration)) echo Nothing
     else runServer (setPrefix "/echo" defaultConfiguration) echo Nothing

options :: [OptDescr Flag] 
options = [ Option "t" ["transport"] (ReqArg Transport "xhr") "to limit transport whitelist to xhr" ]

data Flag = Transport String
            deriving (Show, Eq)

-- Simplest possible example: echo server
echo connection = forever $ do
    msg <- receiveData connection
    sendTextData connection msg
