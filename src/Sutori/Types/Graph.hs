{-|
Description : Provides a graph of types that allows a bijection
              between generated 'SutTypeID's and 'SutType's
-}
module Sutori.Types.Graph
( TypeGraph(TypeGraph)
, initialTypeGraph
, initialNextTypeID
, insertType
, lookupTypeID
, lookupType
, orderedGraph
, memberOffset
) where

import qualified Data.Map as Map
import Data.List(sortOn, foldl')
import Control.Arrow(first)
import Data.Maybe (fromJust)

import Sutori.Types.Primitives
import Sutori.Types.Constructors


-- |The type graph must be a biderectional map (as there's a bijection between type and its ID)
data TypeGraph = TypeGraph {
  typeToIDMap :: Map.Map SutType (SutTypeID, Int), -- ^ Bimap component: Maps 'SutTypeID' to 'SutType'
  idToTypeMap :: Map.Map SutTypeID (SutType, Int)  -- ^ Bimap component: Maps 'SutTypeID' to 'SutType'
}

-- |An empty type graph (is an empty bimap)
emptyTypeGraph :: TypeGraph
emptyTypeGraph = TypeGraph{ typeToIDMap = Map.empty, idToTypeMap = Map.empty }

-- |The initial value for the graph. That is, the primitives added
initialTypeGraph :: TypeGraph
initialTypeGraph = foldl' (flip insertType) emptyTypeGraph (map (first SutPrimitiveType) primitiveIDs)

-- |The initial value for the "next type ID"
initialNextTypeID :: Int
initialNextTypeID = length primitiveIDs

-- |Inserts a type mapping into the map
insertType :: (SutType, SutTypeID) -> TypeGraph -> TypeGraph
insertType (t, tid) g@TypeGraph{typeToIDMap = l, idToTypeMap = r} =
  let l' = Map.insert t   (tid, s) l
      r' = Map.insert tid (t, s)   r
      s  = roundWords $ typeSize t
   in TypeGraph{typeToIDMap = l', idToTypeMap = r'}

  where typeSize :: SutType -> Int
        typeSize (SutPrimitiveType p) = primitiveSize p
        typeSize (SutDirection _)     = wordSize
        typeSize (SutChain i tid')    = i * snd (fromJust $ lookupType tid' g)
        typeSize (SutThing ms)        = maximum $ map memberSize ms
        typeSize (SutMachine ms)      = packSize ms

        packSize :: [(a, SutTypeID)] -> Int
        packSize = foldr ((+) . roundWords . memberSize) 0

        memberSize :: (a, SutTypeID) -> Int
        memberSize = typeSize . fst . fromJust . (`lookupType` g) . snd

        roundWords :: Int -> Int
        roundWords i = div (i + wordSize - 1) wordSize * wordSize

        wordSize :: Int
        wordSize = 4

-- |Lookup and check a member's offset
memberOffset :: SutType -> String -> Int
memberOffset (SutMachine members) sid = f sid members
  where
    f a []           = 0
    f a ((mid,s):ms) = if sid == mid then 0 else s + f a ms
memberOffset _ _ = 0

-- |Maybe the 'SutTypeID' of the given type
lookupTypeID :: SutType -> TypeGraph -> Maybe (SutTypeID, Int)
lookupTypeID t g = Map.lookup t $ typeToIDMap g

-- |Maybe the type of the given 'SutTypeID'
lookupType :: SutTypeID -> TypeGraph -> Maybe (SutType, Int)
lookupType tid g = Map.lookup tid $ idToTypeMap g

-- |The graph as a list sorted by ID (first appearance?)
orderedGraph :: TypeGraph -> [(SutTypeID, (SutType, Int))]
orderedGraph g = sortOn fst $ Map.toList $ idToTypeMap g
