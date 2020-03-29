{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Handler.Games where

import Import

data NewGame = NewGame { which :: GameType }
newGameForm :: Form NewGame
newGameForm = renderDivs $ NewGame <$> areq (selectField optionsEnum) "" Nothing

getGamesR :: Handler Html
getGamesR = do
    (widget, enctype) <- generateFormPost newGameForm
    gamesList <- runDB $ selectList [GameState !=. Finished] [Desc GameId]
    defaultLayout $ do
        setTitle . toHtml $ show (length gamesList) ++ " active games"
        $(widgetFile "games")

postGamesR :: Handler Html
postGamesR = do
    ((result, widget), enctype) <- runFormPost newGameForm
    case result of
      FormSuccess (NewGame gt@Tichu) -> do
          Entity subid _ <- runDB $ insertEntity (TichuGame Nothing [])
          runDB $ insertEntity (Game gt (fromSqlKey subid) Waiting 0)
          getGamesR
      _ -> getGamesR
