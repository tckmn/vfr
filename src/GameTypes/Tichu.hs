{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module GameTypes.Tichu
    ( TichuSeat(..)
    , TichuBet(..)
    , TichuStatus(..)
    , TichuSuit(..), TichuCard(..), TichuCardTrio, TichuCardList
    , TichuMsgIn(TMIGeneric, TMISatDown, TMIPickedUp, TMIMadeBet, TMIPassed, TMIPlayed, TMIGaveDragon, TMIReadyToStart, TMIPassesFinished)
    , TichuMsgOut(TMOSatDown, TMOPassed, TMOCards)
    , TichuMsg(TMWrapper)
    , encodeMsg
    ) where

import ClassyPrelude.Yesod
import Prelude ()
import Database.Persist.TH
import Database.Persist.Sql
import qualified Data.Scientific as S
import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict as M
import Tools
import GameTypes.Generic

data TichuSeat = Head1 | Side1 | Head2 | Side2 deriving (Eq, Generic, FromJSON, ToJSON, Enum, Bounded)

instance PersistField TichuSeat where
    toPersistValue = PersistInt64 . fromIntegral . fromEnum
    fromPersistValue (PersistInt64 n) = Right . toEnum . fromIntegral $ n
    fromPersistValue _ = Left "TichuSeat parse failure"

instance PersistFieldSql TichuSeat where
    sqlType _ = SqlInt32

data TichuBet = Tichued | Granded deriving (Eq, Generic, FromJSON, ToJSON)
data TichuStatus = Pickednt | Picked | MadeBet TichuBet deriving (Eq)

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

data TichuSuit = Jade | Pagoda | Star | Sword deriving (Eq, Ord, Generic, FromJSON, ToJSON, Enum, Bounded)
data TichuCard = NumberCard TichuSuit Int | Dog | Dragon | Mahjong | Phoenix
    deriving (Eq, Generic, FromJSON, ToJSON)
type TichuCardTrio = Trio TichuCard
type TichuCardList = [TichuCard]

instance Ord TichuCard where
    -- phoenix comes first for convenience in matching plays
    compare = compare `on` numeralize
        where numeralize Phoenix = 0
              numeralize Dog = 1
              numeralize Mahjong = 1
              numeralize (NumberCard _ v) = v
              numeralize Dragon = 15

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

data TichuMsgIn = TMIGeneric { imsg :: GenericMsg }
                | TMISatDown { iseat :: TichuSeat }
                | TMIPickedUp
                | TMIMadeBet { ibet :: TichuBet }
                | TMIPassed { ileft :: TichuCard, iacross :: TichuCard, iright :: TichuCard }
                | TMIPlayed { icards :: [TichuCard] }
                | TMIGaveDragon { irecip :: TichuSeat }
                | TMIReadyToStart
                | TMIPassesFinished
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1, constructorTagModifier = drop 3, sumEncoding = ObjectWithSingleField} ''TichuMsgIn)

data TichuMsgOut = TMOSatDown { oseat :: TichuSeat, oname :: Text }
                 | TMOPassed
                 | TMOCards { ocards :: [TichuCard] }
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1, constructorTagModifier = drop 3, sumEncoding = ObjectWithSingleField} ''TichuMsgOut)

data TichuMsg = TMWrapper { uid :: Int64, msg :: TichuMsgIn }

encodeMsg :: ToJSON a => Int64 -> a -> LByteString
encodeMsg uid msg = encode $
    case toJSON msg of
      Object o -> Object $ M.insert "uid" (Number $ S.scientific (fromIntegral uid) 0) o
      _ -> Null
