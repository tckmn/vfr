{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module GameTypes.Tichu
    ( TichuSeat(..)
    , TichuBet(..)
    , TichuStatus(..)
    , TichuCard(..)
    , TichuMsg(TMGeneric, TMSatDown, TMPickedUp, TMMadeBet, TMPassed, TMPlayed, TMGaveDragon)
    , TichuMsgOut(TMOSatDown, TMOPassed)
    ) where

import Database.Persist.TH
import Database.Persist.Sql
import ClassyPrelude.Yesod
import Prelude ()

import GameTypes.Generic
import Data.Aeson.TH

data TichuSeat = Head1 | Side1 | Head2 | Side2 deriving (Generic, FromJSON, ToJSON, Enum)

instance PersistField TichuSeat where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 n) = Right . toEnum . fromIntegral $ n
    fromPersistValue _ = Left "TichuSeat parse failure"

instance PersistFieldSql TichuSeat where
    sqlType _ = SqlInt32

data TichuBet = Tichued | Granded deriving (Generic, FromJSON, ToJSON)
data TichuStatus = Pickednt | Picked | MadeBet TichuBet

instance PersistField TichuStatus where
    toPersistValue Pickednt          = PersistInt64 0
    toPersistValue Picked            = PersistInt64 1
    toPersistValue (MadeBet Tichued) = PersistInt64 2
    toPersistValue (MadeBet Granded) = PersistInt64 3
    fromPersistValue (PersistInt64 0) = Right Pickednt
    fromPersistValue (PersistInt64 1) = Right Picked
    fromPersistValue (PersistInt64 2) = Right (MadeBet Tichued)
    fromPersistValue (PersistInt64 3) = Right (MadeBet Granded)
    fromPersistValue _ = Left "TichuStatus parse failure"

instance PersistFieldSql TichuStatus where
    sqlType _ = SqlInt32

data TichuSuit = Jade | Pagoda | Star | Sword deriving (Generic, FromJSON, ToJSON, Enum)
data TichuCard = NumberCard TichuSuit Int | Dog | Dragon | Mahjong | Phoenix
    deriving (Generic, FromJSON, ToJSON)

instance PersistField TichuCard where
    toPersistValue (NumberCard s v) = PersistInt64 . fromIntegral $ 15*fromEnum s + v
    toPersistValue Dog     = PersistInt64 60
    toPersistValue Dragon  = PersistInt64 61
    toPersistValue Mahjong = PersistInt64 62
    toPersistValue Phoenix = PersistInt64 63
    fromPersistValue (PersistInt64 60) = Right Dog
    fromPersistValue (PersistInt64 61) = Right Dragon
    fromPersistValue (PersistInt64 62) = Right Mahjong
    fromPersistValue (PersistInt64 63) = Right Phoenix
    fromPersistValue (PersistInt64 n) =
        let (d,m) = n `divMod` 15 in
            Right $ NumberCard (toEnum . fromIntegral $ d) (fromIntegral m)
    fromPersistValue _ = Left "TichuCard parse failure"

instance PersistFieldSql TichuCard where
    sqlType _ = SqlInt32

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
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TichuMsgOut)
