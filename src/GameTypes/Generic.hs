{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module GameTypes.Generic where

import Database.Persist.TH
import ClassyPrelude.Yesod

import Yesod.Core.Dispatch

data GameType = Tichu deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "GameType"

instance PathPiece GameType where
    fromPathPiece "tichu" = Just Tichu
    fromPathPiece _ = Nothing

    toPathPiece Tichu = "tichu"

data GameState = Waiting | Playing | Finished deriving (Show, Read, Eq)
derivePersistField "GameState"

data GenericMsg = PlayerHere
                | ChatMessage { msg :: Text }
                | PlayerGone
                deriving (Generic, FromJSON, ToJSON)
