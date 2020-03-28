{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Play where

import Import
import Tools
import Yesod.WebSockets
import qualified StmContainers.Map as SM
import Data.Function ((&))

handleGeneric :: UserId -> GenericMsg -> WebSocketsT Handler ()
handleGeneric uid PlayerHere = sendTextData ("foo" :: ByteString)
handleGeneric uid (ChatMessage msg) = sendTextData msg
handleGeneric uid PlayerGone = sendTextData ("baz" :: ByteString)

liftDB = liftHandler . runDB

tmWrap :: UserId -> TichuMsgIn -> TichuMsg
tmWrap = TMWrapper . fromSqlKey

beginTichuGame :: TichuGameId -> WebSocketsT Handler ()
beginTichuGame gid = do
    deck <- liftIO . shuffle $
        [NumberCard suit n | suit <- [minBound..maxBound], n <- [2..14]] ++
        [Dog, Dragon, Mahjong, Phoenix]
    liftDB $ foldr (>>) (return ()) [do
        updateWhere
            [ TichuPlayerTichuGameId ==. gid
            , TichuPlayerSeat ==. seat
            ] [TichuPlayerHand =. (take 14 . drop (14*fromEnum seat) $ deck)]
              | seat <- [minBound..maxBound]]

tichuAppHandler ::
    TChan TichuMsg -> TChan TichuMsg ->
    TichuGameId -> Maybe UserId ->
    Maybe TichuMsgIn -> WebSocketsT Handler ()

tichuAppHandler writeChan readChan gid (Just uid) (Just msg@(TMISatDown seat)) = do
    existing <- liftDB $ selectFirst
        ([TichuPlayerTichuGameId ==. gid] ++
         ([TichuPlayerSeat ==. seat] ||. [TichuPlayerUserId ==. uid])) []
    if isJust existing
       then sendTextData ("Someone's already sitting there!" :: Text)
       else do
           players <- liftDB $ do
               insert (TichuPlayer uid gid seat Pickednt [] [] Nothing)
               selectList [TichuPlayerTichuGameId ==. gid] []
           atomically . writeTChan writeChan $ tmWrap uid msg
           if length players == 4
              then do beginTichuGame gid
                      atomically . writeTChan writeChan $ tmWrap uid TMIReadyToStart
              else return ()

-- parse failure, unauthenticated, or similar
tichuAppHandler _ _ _ _ _ = return ()

tichuApp :: Int64 -> WebSocketsT Handler ()
tichuApp gid = do
    App {..} <- getYesod
    muser' <- maybeAuthId
    let gid' = toSqlKey gid
        muser = fromSqlKey <$> muser'

    (writeChan, readChan) <- atomically $ do
        mwriteChan <- SM.lookup gid appTichuChans
        writeChan <- case mwriteChan of
                       Just x -> return x
                       Nothing -> do
                           x <- newBroadcastTChan
                           SM.insert x gid appTichuChans
                           return x
        readChan <- dupTChan writeChan
        for_ muser $ writeTChan writeChan . flip TMWrapper (TMIGeneric PlayerHere)
        return (writeChan, readChan)

    race_
        (forever $ do
            TMWrapper uid msg <- atomically $ readTChan readChan
            let uid' = toSqlKey uid
            case msg of
              TMIGeneric g -> handleGeneric uid' g
              TMISatDown seat -> uid' & liftDB . get >>=
                  sendTextData . encodeMsg uid . TMOSatDown seat . maybe "???" userName
              TMIPassed _ _ _ -> sendTextData $ encodeMsg uid TMOPassed
              _ -> sendTextData $ encodeMsg uid msg
            return ()
        )
        (runConduit $ sourceWS .| mapM_C (tichuAppHandler writeChan readChan gid' muser' . decode))

    atomically $ for_ muser $ writeTChan writeChan . flip TMWrapper (TMIGeneric PlayerGone)

getPlayR :: GameType -> Int64 -> Handler Html
getPlayR Tichu gid = do
    webSockets $ tichuApp gid
    defaultLayout $ do
        setTitle "Tichu game"
        $(widgetFile "tichu")
