{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module GameTypes.Tichu
    ( TichuSeat(..)
    , TichuBet(..)
    , TichuStatus(..)
    , TichuCard(..)
    , TichuMsg(TMGeneric, TMSatDown, TMPickedUp, TMMadeBet, TMPassed, TMPlayed, TMGaveDragon)
    , TichuMsgOut(TMOSatDown, TMOPassed)
    ) where

import Database.Persist.TH
import ClassyPrelude.Yesod

import GameTypes.Generic
import Data.Aeson.TH

data TichuSeat = Head1 | Side1 | Head2 | Side2
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
derivePersistField "TichuSeat"

data TichuBet = Tichued | Granded
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data TichuStatus = Pickednt | Picked | MadeBet TichuBet
    deriving (Show, Read, Eq)
derivePersistField "TichuStatus"

data TichuCard = Card deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
derivePersistField "TichuCard"

data TichuMsg = TMGeneric    { msg :: GenericMsg }
              | TMSatDown    { seat :: TichuSeat, uid :: Int64 }
              | TMPickedUp   { seat :: TichuSeat }
              | TMMadeBet    { seat :: TichuSeat, bet :: TichuBet }
              | TMPassed     { seat :: TichuSeat, left :: TichuCard, across :: TichuCard, right :: TichuCard }
              | TMPlayed     { seat :: TichuSeat, cards :: [TichuCard] }
              | TMGaveDragon { seat :: TichuSeat, recip :: TichuSeat }
              | TMReadyToStart
              | TMPassesFinished
              deriving (Generic, FromJSON, ToJSON)

data TichuMsgOut = TMOSatDown { oseat :: TichuSeat, ouid :: Int64, oname :: Text }
                 | TMOPassed  { oseat :: TichuSeat }
$(deriveJSON defaultOptions{fieldLabelModifier = Prelude.drop 1} ''TichuMsgOut)
