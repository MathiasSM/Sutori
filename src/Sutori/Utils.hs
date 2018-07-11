module Sutori.Utils where

import Data.List

class SutShow a where
  showSut :: a -> String

anyf :: [a -> Bool] -> a -> Bool
anyf fns x = any (\fn -> fn x) fns

allf :: [a -> Bool] -> a -> Bool
allf fns x = all (\fn -> fn x) fns

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . group . sort

repeated :: Ord a => [a] -> [a]
repeated = map head . filterByLength (>1)
