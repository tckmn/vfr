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

liftDB = liftHandler . runDB
player gid uid = [TichuPlayerTichuGameId ==. gid, TichuPlayerUserId ==. uid]

handleGeneric :: UserId -> GenericMsg -> WebSocketsT Handler ()
handleGeneric uid PlayerHere = sendTextData ("foo" :: ByteString)
handleGeneric uid (ChatMessage msg) = sendTextData msg
handleGeneric uid PlayerGone = sendTextData ("baz" :: ByteString)

-- there's already a thing called Single, whoops
data TichuPlay = Singl TichuCard
               | Pair Int
               | Triple Int
               | FullHouse Int
               | Straight Int Int
               | Tractor Int Int
               | Bomb Int Int

resolvePlay :: [TichuCard] -> Maybe TichuPlay
resolvePlay cards
  | any (/=1) counts = Nothing
  | otherwise        = resolvePlay' sorted
  where sorted = sort cards
        counts = length <$> group sorted

-- helper function that takes sorted list of unique cards
resolvePlay' :: [TichuCard] -> Maybe TichuPlay

resolvePlay' [] = Nothing
resolvePlay' [a] = Just $ Singl a

resolvePlay' [NumberCard _ n, NumberCard _ n']
  | n == n'   = Just $ Pair n
  | otherwise = Nothing
resolvePlay' [Phoenix, NumberCard _ n] = Just $ Pair n
resolvePlay' [_, _] = Nothing

resolvePlay' [NumberCard _ n, NumberCard _ n', NumberCard _ n'']
  | n == n' && n' == n'' = Just $ Triple n
  | otherwise            = Nothing
resolvePlay' [Phoenix, NumberCard _ n, NumberCard _ n']
  | n == n'   = Just $ Triple n
  | otherwise = Nothing
resolvePlay' [_, _, _] = Nothing

-- handle this case explicitly because of bombs
resolvePlay' [NumberCard _ n, NumberCard _ n', NumberCard _ n'', NumberCard _ n''']
  | n == n' && n' == n'' && n'' == n'''   = Just $ Bomb 4 n
  | n == n' && n'' == n''' && n+1 == n'' = Just $ Tractor 2 n
  | otherwise = Nothing
resolvePlay' [_, _, _, _] = Nothing

-- with the phoenix, we make it a different suit from another card to avoid making bombs
-- TODO: maybe we should let players decide what the phoenix is when ambiguous
-- (for now, it acts as the highest legal value, which is usually what you want)
resolvePlay' (Phoenix:xs@(x0:_)) =
    asum [resolvePlay' $ (NumberCard freeSuit n):xs | n <- [14,13..2]]
        where freeSuit = case x0 of NumberCard Jade _ -> Pagoda
                                    _ -> Jade

resolvePlay' xs@(x0':_)
  | length xs /= len = Nothing
  | all (==1) diffs = Just $ if length (group suits) == 1 then Bomb len x0 else Straight len x0
  | even len && and (zipWith (==) diffs (cycle [0,1])) = Just $ Tractor len x0
  | otherwise = Nothing
  where (suits, nums) = unzip $ mapMaybe unwrapNum xs
        len = length nums
        diffs = zipWith (-) (drop 1 nums) nums
        x0 = case x0' of NumberCard _ n -> n
                         _ -> 0
        unwrapNum (NumberCard s v) = Just (s, v)
        unwrapNum _ = Nothing
        cycle xs = xs' where xs' = xs ++ xs'

-- the mapMaybe won't throw anything out because these are guaranteed to resolve
canPlayOn :: [[TichuCard]] -> TichuPlay -> Bool
canPlayOn = canPlayOn' . mapMaybe resolvePlay

canPlayOn' :: [TichuPlay] -> TichuPlay -> Bool

canPlayOn' [] _ = True

canPlayOn' ((Singl Phoenix):[]) (Singl c')   = c' > Dog
canPlayOn' ((Singl Phoenix):xs) c'@(Singl _) = canPlayOn' xs c'
canPlayOn' ((Singl c):_) (Singl Phoenix)     = c /= Dragon

canPlayOn' ((Singl c):_)        (Singl c')         = c' > c
canPlayOn' ((Pair n):_)         (Pair n')          = n' > n
canPlayOn' ((Triple n):_)       (Triple n')        = n' > n
canPlayOn' ((FullHouse n):_)    (FullHouse n')     = n' > n
canPlayOn' ((Straight len n):_) (Straight len' n') = len' == len && n' > n
canPlayOn' ((Tractor len n):_)  (Tractor len' n')  = len' == len && n' > n
canPlayOn' ((Bomb len n):_)     (Bomb len' n')     = compare len' len <> compare n' n == GT

canPlayOn' _ _ = False

tmWrap :: UserId -> TichuMsgIn -> TichuMsg
tmWrap = TMWrapper . fromSqlKey

beginTichuGame :: TichuGameId -> WebSocketsT Handler ()
beginTichuGame gid = do
    deck <- liftIO . shuffle $
        [NumberCard suit n | suit <- [minBound..maxBound], n <- [2..14]] ++
        [Dog, Dragon, Mahjong, Phoenix]
    liftDB $ sequence_ [do
        updateWhere
            [ TichuPlayerTichuGameId ==. gid
            , TichuPlayerSeat ==. seat
            ] [TichuPlayerHand =. (take 14 . drop (14*fromEnum seat) $ deck)]
              | seat <- [minBound..maxBound]]

resolvePasses :: MonadIO m => TichuGameId -> ReaderT SqlBackend m ()
resolvePasses gid = do
    mhead1 <- selectFirst [TichuPlayerTichuGameId ==. gid, TichuPlayerSeat ==. Head1] []
    mside1 <- selectFirst [TichuPlayerTichuGameId ==. gid, TichuPlayerSeat ==. Side1] []
    mhead2 <- selectFirst [TichuPlayerTichuGameId ==. gid, TichuPlayerSeat ==. Head2] []
    mside2 <- selectFirst [TichuPlayerTichuGameId ==. gid, TichuPlayerSeat ==. Side2] []
    -- none of this garbage should ever be Nothing, so we ignore everything if it is
    -- (TODO: probably better to log an error or something but whatever)
    sequence_ $ do
        Entity h1id h1 <- mhead1
        Entity s1id s1 <- mside1
        Entity h2id h2 <- mhead2
        Entity s2id s2 <- mside2
        h1p <- tichuPlayerPasses h1
        s1p <- tichuPlayerPasses s1
        h2p <- tichuPlayerPasses h2
        s2p <- tichuPlayerPasses s2
        let h1h = tfst s2p:tsnd h2p:thrd s1p:[c | c <- tichuPlayerHand h1, all (c /=) h1p]
            s1h = tfst h1p:tsnd s2p:thrd h2p:[c | c <- tichuPlayerHand s1, all (c /=) s1p]
            h2h = tfst s1p:tsnd h1p:thrd s2p:[c | c <- tichuPlayerHand h2, all (c /=) h2p]
            s2h = tfst h2p:tsnd s1p:thrd h1p:[c | c <- tichuPlayerHand s2, all (c /=) s2p]
            gofirst =
                if Mahjong `elem` h1h then Head1 else
                if Mahjong `elem` s1h then Side1 else
                if Mahjong `elem` h2h then Head2 else Side2
        return $ do
            update h1id [TichuPlayerHand =. h1h]
            update s1id [TichuPlayerHand =. s1h]
            update h2id [TichuPlayerHand =. h2h]
            update s2id [TichuPlayerHand =. s2h]
            update gid [TichuGameTurn =. Just gofirst]

revealCardsHandler :: Int -> TichuGameId -> Maybe UserId -> WebSocketsT Handler ()
revealCardsHandler n gid (Just uid) =
    liftDB (selectFirst (player gid uid) []) >>=
    traverse_ (sendTextData . encode . TMOCards . take n . tichuPlayerHand . entityVal)
revealCardsHandler _ _ _ = return ()

tichuAppHandler ::
    TChan TichuMsg -> TChan TichuMsg ->
    TichuGameId -> Maybe UserId ->
    Maybe TichuMsgIn -> WebSocketsT Handler ()

-- TODO: allow players to unsit
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
           when (length players == 4) $ do
               beginTichuGame gid
               atomically . writeTChan writeChan $ tmWrap uid TMIReadyToStart

tichuAppHandler writeChan readChan gid (Just uid) (Just msg@(TMIPickedUp)) = do
    playerWhoPicked <- liftDB $ do
        res <- updateWhereCount
            (player gid uid ++ [TichuPlayerStatus ==. Pickednt])
            [TichuPlayerStatus =. Picked]
        if res == 1
           then selectFirst (player gid uid) []
           else return Nothing
    for_ playerWhoPicked $
        (sendTextData . encode . TMOCards . tichuPlayerHand . entityVal) >=>
        (pure $ atomically . writeTChan writeChan $ tmWrap uid msg)

tichuAppHandler writeChan readChan gid (Just uid) (Just msg@(TMIMadeBet bet)) = do
    isValid <- liftDB $ do
        mplayer <- selectFirst (player gid uid) []
        case mplayer of
          Just (Entity pid p) -> do
              let isValid =
                      tichuPlayerStatus p == Pickednt ||
                      (bet == Tichued && tichuPlayerStatus p == Picked &&
                          length (tichuPlayerHand p) == 14)
              when isValid $ update pid [TichuPlayerStatus =. MadeBet bet]
              return isValid
          Nothing -> return False
    when isValid $ atomically . writeTChan writeChan $ tmWrap uid msg

-- TODO: allow players to unpass
tichuAppHandler writeChan readChan gid (Just uid) (Just msg@(TMIPassed left across right)) = do
    mshouldStart <- liftDB $ do
        mplayer <- selectFirst (player gid uid) []
        case mplayer of
          Just (Entity pid p) -> do
              let isValid = and
                      [ isNothing $ tichuPlayerPasses p
                      , left /= across
                      , across /= right
                      , left /= right
                      , left `elem` tichuPlayerHand p
                      , across `elem` tichuPlayerHand p
                      , right `elem` tichuPlayerHand p
                      ]
              if isValid
                 then do
                     update pid [TichuPlayerPasses =. Just (Trio left across right)]
                     passed <- selectList
                        [TichuPlayerTichuGameId ==. gid, TichuPlayerPasses !=. Nothing] []
                     if length passed == 4
                        then resolvePasses gid >> return (Just True)
                        else return (Just False)
                 else return Nothing
          Nothing -> return Nothing
    for_ mshouldStart $ \shouldStart -> atomically $ do
        writeTChan writeChan $ tmWrap uid msg
        when shouldStart $ writeTChan writeChan $ tmWrap uid TMIPassesFinished

tichuAppHandler writeChan readChan gid (Just uid) (Just msg@(TMIPlayed cards)) = do
    isValid <- liftDB $ do
        mgame <- get gid
        mplayer <- selectFirst (player gid uid) []
        let mplay = do
            game <- mgame
            Entity pid p <- mplayer
            let board = tichuGameBoard game
                turn = tichuGameTurn game
                hand = tichuPlayerHand p
            guard $ all (`elem` hand) cards
            play <- resolvePlay cards
            guard $ canPlayOn board play
            guard $ let myturn = any (== tichuPlayerSeat p) turn in
                        case play of
                          Bomb _ _ -> isJust turn && (myturn || not (null board))
                          _ -> myturn
            return $ do
                update pid [TichuPlayerHand =. [c | c <- hand, c `notElem` cards]]
                update gid [TichuGameBoard =. (cards:board), TichuGameTurn =. succ' <$> turn]
        case mplay of
          Just playAction -> playAction >> return True
          Nothing -> return False
    when isValid $ atomically . writeTChan writeChan $ tmWrap uid msg

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
              TMIReadyToStart -> revealCardsHandler 8 gid' muser'
              TMIPassesFinished -> revealCardsHandler 14 gid' muser'
              _ -> sendTextData $ encodeMsg uid msg
        )
        (runConduit $ sourceWS .| mapM_C (tichuAppHandler writeChan readChan gid' muser' . decode))

    atomically $ for_ muser $ writeTChan writeChan . flip TMWrapper (TMIGeneric PlayerGone)

getPlayR :: GameType -> Int64 -> Handler Html
getPlayR Tichu gid = do
    webSockets $ tichuApp gid
    defaultLayout $ do
        setTitle "Tichu game"
        $(widgetFile "tichu")
