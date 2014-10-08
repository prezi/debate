{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Chat.State (
  ChatState(..)
, RoomState(..)
, UserState(..)
, User(..)
, Room(..)
, newChatState
, addUserToRoom
, removeUserFromRoom
, removeUserFromAll
, saveTVar
) where

import qualified Data.Text       as T
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Debate.Server

data User = User { userName :: T.Text
                 , handle :: SockConnection }

instance Show User where
  show User {userName = name} = "user " ++ T.unpack name
-- TODO!! this means we cannot have 2 users with the same username, and this should be checked
instance Eq User where
  user1 == user2 = userName user1 == userName user2

data Room = Room { roomName :: T.Text
                 , usersInRoom :: [User] }
                 deriving (Show, Eq)

type UserState = Map.Map T.Text [T.Text]
type RoomState = Map.Map T.Text Room

data ChatState = ChatState { userState :: UserState
                           , roomState :: RoomState }
                 deriving Show

newChatState = atomically $ newTVar ChatState {userState = Map.empty, roomState = Map.empty}

-- functions to manipulate state

saveTVar :: TVar ChatState -> (ChatState -> ChatState) -> IO ()
saveTVar chatState f = atomically $ readTVar chatState >>= writeTVar chatState . f

addUserToRoom :: T.Text -> User -> ChatState -> ChatState
addUserToRoom roomname usr@User{userName = username} chatState = do
            let chatStateWithRoom = addRoomIfNew roomname chatState
                chatStateWithUserAndRoom = addUserIfNew username chatStateWithRoom
                roomMap = roomState chatStateWithUserAndRoom
                newRoomMap = Map.adjust (addUser usr) roomname roomMap
                userMap = userState chatStateWithUserAndRoom
                newUserMap = Map.adjust (addRoom roomname) username userMap
            ChatState { userState = newUserMap, roomState = newRoomMap }
            where addUser usr Room{roomName = name, usersInRoom = usrs} = Room{roomName = name, usersInRoom = usr:usrs }
                  addRoom roomname rooms = roomname:rooms


addRoomIfNew :: T.Text -> ChatState -> ChatState
addRoomIfNew roomname chatState@ChatState{..} = if Map.notMember roomname roomState
                                                  then ChatState { userState = userState, roomState = Map.insert roomname (Room roomname []) roomState }
                                                  else chatState

addUserIfNew :: T.Text -> ChatState -> ChatState
addUserIfNew username chatState@ChatState{..} = if Map.notMember username userState
                                                 then ChatState { userState = Map.insert username [] userState, roomState = roomState }
                                                 else chatState

-- think about transactions: atomically around all this?
removeUserFromAll :: User -> ChatState -> ChatState
removeUserFromAll user@User{userName = userName} chatState = do
            let roomNames = fromMaybe [] $ Map.lookup userName (userState chatState)
            foldr (removeUserFromRoom user) chatState roomNames

removeUserFromRoom :: User -> T.Text -> ChatState -> ChatState
removeUserFromRoom user@User{userName = userName} roomName ChatState{..} = do
            let newRoomState = Map.adjust (removeUser user) roomName roomState
                newUserState = Map.adjust (removeRoom roomName) userName userState
            ChatState {userState = newUserState, roomState = newRoomState }
            where removeUser user Room{roomName = name, usersInRoom = usrs} = Room{roomName = name, usersInRoom = filter (\usr -> user /= usr) usrs}
                  removeRoom roomName = filter (\name -> roomName /= name)
