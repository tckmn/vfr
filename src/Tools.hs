module Tools where

import System.Random

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    pos <- getStdRandom $ randomR (0, length xs - 1)
    let (l, (x:r)) = splitAt pos xs
    (x:) <$> shuffle (l++r)
