{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Play where

import Import
import Yesod.WebSockets
import qualified StmContainers.Map as SM
import Data.Function ((&))

handleGeneric :: Int64 -> GenericMsg -> WebSocketsT Handler ()
handleGeneric uid PlayerHere = sendTextData ("foo" :: ByteString)
handleGeneric uid (ChatMessage msg) = sendTextData msg
handleGeneric uid PlayerGone = sendTextData ("baz" :: ByteString)

liftDB = liftHandler . runDB

tichuAppHandler ::
    TChan TichuMsg -> TChan TichuMsg ->
    Int64 -> Maybe Int64 ->
    Maybe TichuMsgIn -> WebSocketsT Handler ()

tichuAppHandler writeChan readChan gid (Just uid) (Just msg@(TMISatDown seat)) = do
    existing <- liftDB $ selectFirst
        [ TichuPlayerTichuGameId ==. toSqlKey gid
        , TichuPlayerSeat ==. seat
        ] []
    if isJust existing
       then sendTextData ("Someone's already sitting there!" :: Text)
       else do
           players <- liftDB $
               insert (TichuPlayer (toSqlKey uid) (toSqlKey gid) seat Pickednt [] [] Nothing) >>
               selectList [TichuPlayerTichuGameId ==. toSqlKey gid] []
           atomically $
               TMWrapper uid msg & writeTChan writeChan >>
                   if length players == 4
                      then writeTChan writeChan (TMWrapper uid TMIReadyToStart)
                      else return ()

-- parse failure, unauthenticated, or similar
tichuAppHandler _ _ _ _ _ = return ()

tichuApp :: Int64 -> WebSocketsT Handler ()
tichuApp gid = do
    App {..} <- getYesod
    muser <- fmap fromSqlKey <$> maybeAuthId

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
            case msg of
              TMIGeneric g -> handleGeneric uid g
              TMISatDown seat -> uid & liftDB . get . toSqlKey >>=
                  sendTextData . encodeMsg uid . TMOSatDown seat . maybe "???" userName
              TMIPassed _ _ _ -> sendTextData $ encodeMsg uid TMOPassed
              _ -> sendTextData $ encodeMsg uid msg
            return ()
        )
        (runConduit $ sourceWS .| mapM_C (tichuAppHandler writeChan readChan gid muser . decode))

    atomically $ for_ muser $ writeTChan writeChan . flip TMWrapper (TMIGeneric PlayerGone)

getPlayR :: GameType -> Int64 -> Handler Html
getPlayR Tichu gid = do
    webSockets $ tichuApp gid
    defaultLayout $ do
        setTitle "Tichu game"
        $(widgetFile "tichu")
