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
) where

import qualified Data.Map.Strict as Map
import Data.List(sortOn, foldl')
import Control.Arrow(first)

import Sutori.Utils              (SutID)

import Sutori.Types.Primitives   (primitiveIDs)
import Sutori.Types.Constructors (SutType(SutPrimitiveType), SutTypeID)


-- |The type graph must be a biderectional map (as there's a bijection between type and its ID)
data TypeGraph = TypeGraph {
  typeToIDMap :: Map.Map SutType SutTypeID, -- ^ Bimap component: Maps 'SutTypeID' to 'SutType'
  idToTypeMap :: Map.Map SutTypeID SutType  -- ^ Bimap component: Maps 'SutTypeID' to 'SutType'
}

-- |An empty type graph (is an empty bimap)
emptyTypeGraph = TypeGraph{ typeToIDMap = Map.empty, idToTypeMap = Map.empty }

-- |The initial value for the graph. That is, the primitives added
initialTypeGraph :: TypeGraph
initialTypeGraph = foldl' (flip insertType) emptyTypeGraph (map (first SutPrimitiveType) primitiveIDs)

-- |The initial value for the "next type ID"
initialNextTypeID :: Int
initialNextTypeID = length primitiveIDs

-- |Inserts a type mapping into the map
insertType :: (SutType, SutTypeID) -> TypeGraph -> TypeGraph
insertType (t,id) TypeGraph{typeToIDMap = l, idToTypeMap = r} = TypeGraph{typeToIDMap = l', idToTypeMap = r'}
  where l' = Map.insert t id l
        r' = Map.insert id t r

-- |Maybe the 'SutTypeID' of the given type
lookupTypeID :: SutType -> TypeGraph -> Maybe SutTypeID
lookupTypeID t g = Map.lookup t $ typeToIDMap g

-- |Maybe the type of the given 'SutTypeID'
lookupType :: SutTypeID -> TypeGraph -> Maybe SutType
lookupType id g = Map.lookup id $ idToTypeMap g

-- |The graph as a list sorted by ID (first appearance?)
orderedGraph :: TypeGraph -> [(SutTypeID, SutType)]
orderedGraph g = sortOn snd $ Map.toList $ idToTypeMap g
