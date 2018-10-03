{-|
Description : Provide common type definitions and functions missing in the snapshot
-}
module Sutori.Utils
( anyf
, allf
, filterBy
, repeated
, repeatedBy
, SutID
) where

import Data.List(group, groupBy, sort, sortBy)

-- |Global type for Sutori IDs
type SutID = String

-- |Conjunction of a the results of a list of functions applied to a single value
anyf :: [a -> Bool] -> a -> Bool
anyf fns x = any (\fn -> fn x) fns

-- |Conjunction of a the results of a list of functions applied to a single value
allf :: [a -> Bool] -> a -> Bool
allf fns x = all (\fn -> fn x) fns

-- |Filters a list with a given filter function
filterBy :: Ord a => ([a] -> Bool) -> [a] -> [[a]]
filterBy p = filter p . group . sort

-- |Filters out unique elements in a list
repeated :: Ord a => [a] -> [a]
repeated = map head . filterBy ((> 1) . length)

-- |Filters consecutive uniques, given a grouping and a sorting function
repeatedBy :: (Eq c, Ord b) => (a -> c) -> (a -> b) -> [a] -> [a]
repeatedBy g s = map head . filter ((> 1) . length)
  . groupBy (\a b -> g a == g b)
  . sortBy (\a b -> compare (s a) (s b))
