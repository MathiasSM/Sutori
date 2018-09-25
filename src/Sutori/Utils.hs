module Sutori.Utils
( anyf
, allf
, filterBy
, repeated
, repeatedBy
, SutID
) where

import Data.List(group, groupBy, sort, sortBy)

-- Global type for Sutori IDs
type SutID = String

-- Helper general functions
anyf :: [a -> Bool] -> a -> Bool
anyf fns x = any (\fn -> fn x) fns

allf :: [a -> Bool] -> a -> Bool
allf fns x = all (\fn -> fn x) fns

filterBy :: Ord a => ([a] -> Bool) -> [a] -> [[a]]
filterBy p = filter p . group . sort

repeated :: Ord a => [a] -> [a]
repeated = map head . filterBy ((> 1) . length)

repeatedBy :: (Eq c, Ord b) => (a -> c) -> (a -> b) -> [a] -> [a]
repeatedBy g s =
  map head . filter ((> 1) . length) . groupBy (\a b -> g a == g b) . sortBy
    (\a b -> compare (s a) (s b))
