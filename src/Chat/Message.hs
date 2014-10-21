{-# LANGUAGE OverloadedStrings #-}

module Chat.Message (
  ServerMessage(..)
, parseMessage
) where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error hiding (Message)
import Text.ParserCombinators.Parsec.Language
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import qualified Text.Parsec.Token as Token

type RoomName = T.Text

data Room = Room { name :: RoomName }
            deriving (Show)

-- TOD GADT with credentials (could be any combos of datas)
data ServerMessage =
  Login String String |
  Status String String |
  Join String |
  Message String |
  Leave String |
  Logout |
  Invalid String
  deriving (Read, Show, Eq)

chatDef :: Token.LanguageDef st
chatDef = emptyDef { Token.reservedNames = ["LOGIN", "LOGOUT", "JOIN", "LEAVE", "MSG", "STATUS"]
                   , Token.caseSensitive = False }

lexer = Token.makeTokenParser chatDef

reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer

messageParser :: Parser ServerMessage
messageParser =
      try parseLogin
  <|> try parseLogout
  <|> try parseJoin
  <|> try parseLeave
  <|> try parseMsg
  <|> try parseStatus

parseLogin = Login <$>
                (reserved "LOGIN"  *>
                many (noneOf " ")) <*>
                (whiteSpace *>
                many (noneOf " "))
                <* eof

parseStatus = Status <$>
                (reserved "STATUS"  *>
                many (noneOf " ")) <*>
                (whiteSpace *>
                many (noneOf " "))
                <* eof

parseLogout = reserved "LOGOUT" <* eof >> return Logout

parseJoin = Join <$>
              (reserved "JOIN"  *>
              many (noneOf " "))
              <* eof

parseLeave = Leave <$>
              (reserved "LEAVE"  *>
              many (noneOf " "))
              <* eof

parseMsg = Message <$>
             (reserved "MSG"  *>
              whiteSpace *>
              many anyChar)
              <* eof

parseMessage :: String -> Either ParseError ServerMessage
parseMessage = parse messageParser "message didn't parse successfully"
