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

handleGeneric :: GenericMsg -> WebSocketsT Handler ()
handleGeneric (PlayerHere pid) = sendTextData ("foo" :: ByteString)
handleGeneric (ChatMessage pid msg) = sendTextData msg
handleGeneric (PlayerGone pid) = sendTextData ("baz" :: ByteString)

liftDB = liftHandler . runDB

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
        for_ muser $ writeTChan writeChan . TMGeneric . PlayerHere
        return (writeChan, readChan)

    race_
        (forever $ do
            msg <- atomically $ readTChan readChan
            case msg of
              TMGeneric g -> handleGeneric g
              TMSatDown seat uid -> uid & liftDB . get . toSqlKey >>=
                  sendTextData . encode . TMOSatDown seat uid . maybe "???" userName
              TMPassed seat _ _ _ -> sendTextData . encode $ TMOPassed seat
              _ -> sendTextData . encode $ msg
            return ()
        )
        (runConduit $ sourceWS .| mapM_C (\msg ->
            case decode msg of
              Just msg@(TMSatDown seat uid) -> do
                  existing <- liftDB $ selectFirst [TichuPlayerSeat ==. seat] []
                  if isJust existing
                     then sendTextData ("Someone's already sitting there!" :: Text)
                     else do
                         liftDB . insert $
                             TichuPlayer (toSqlKey uid) (toSqlKey gid) seat Pickednt [] [] Nothing
                         atomically $ writeTChan writeChan msg
              _ -> return ()
        ))

    atomically $ for_ muser $ writeTChan writeChan . TMGeneric . PlayerGone

getPlayR :: GameType -> Int64 -> Handler Html
getPlayR Tichu gid = do
    webSockets $ tichuApp gid
    defaultLayout $ do
        setTitle "Tichu game"
        $(widgetFile "tichu")
