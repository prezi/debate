{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Chat.Message (
  ServerMessage(..)
, parseMessage
) where

import qualified Data.Text as T
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import qualified Text.Parsec.Token as Token

type RoomName = T.Text

data Room = Room { name :: RoomName }
            deriving (Show)

-- TOD GADT with credentials (could be any combos of datas)
data ServerMessage =
  Login String String |
  Join String |
  Message RoomName String |
  Part String |
  Logout |
  Invalid String
  deriving (Read, Show, Eq)

chatDef :: Token.LanguageDef st
chatDef = emptyDef { Token.reservedNames = ["LOGIN", "LOGOUT"]
                   , Token.caseSensitive = False }

lexer = Token.makeTokenParser chatDef

reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer

messageParser :: Parser ServerMessage
messageParser =
      try parseLogin
  <|> try parseLogout
  <|> return (Invalid "Couldn't parse")

parseLogin = Login <$>
                (reserved "LOGIN"  *>
                many (noneOf " ")) <*>
                (whiteSpace *>
                many (noneOf " "))

parseLogout = reserved "LOGOUT" >> return Logout

parseMessage :: String -> Either ParseError ServerMessage
parseMessage input = parse messageParser "message didn't parse successfully" input
