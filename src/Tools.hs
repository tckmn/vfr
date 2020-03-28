{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Tools where

import ClassyPrelude.Yesod
import Prelude as P (foldMap, foldr)
import System.Random
import Database.Persist.Sql

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    pos <- getStdRandom $ randomR (0, length xs - 1)
    let (l, (x:r)) = splitAt pos xs
    (x:) <$> shuffle (l++r)

data Trio a = Trio a a a
tfst (Trio x _ _) = x
tsnd (Trio _ x _) = x
thrd (Trio _ _ x) = x

instance Functor Trio where
    fmap f (Trio x y z) = Trio (f x) (f y) (f z)

instance Foldable Trio where
    foldMap f (Trio x y z) = f x ++ f y ++ f z
    foldr f base (Trio x y z) = f x . f y . f z $ base

instance MonoFoldable (Trio a)
type instance Element (Trio a) = a

instance PersistField a => PersistField (Trio a) where
    toPersistValue (Trio x y z) = PersistList $ toPersistValue <$> [x,y,z]
    fromPersistValue (PersistList [x,y,z]) = do
        x <- fromPersistValue x
        y <- fromPersistValue y
        z <- fromPersistValue z
        Right $ Trio x y z
    fromPersistValue _ = Left "Trio parse failure"

instance PersistField a => PersistFieldSql (Trio a) where
    sqlType _ = SqlString
