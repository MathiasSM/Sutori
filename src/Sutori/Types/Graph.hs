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

import Sutori.Types.Primitives   (primitiveIDs)
import Sutori.Types.Constructors (SutType(SutPrimitiveType), SutTypeID)
import Sutori.Utils              (SutID)

type TypeToIDMap = Map.Map SutType SutTypeID
type IDToTypeMap = Map.Map SutTypeID SutType

-- The type graph must be a biderectional map (as there's a bijection between type and its ID)
data TypeGraph = TypeGraph {typeToIDMap :: TypeToIDMap, idToTypeMap :: IDToTypeMap}
emptyTypeGraph = TypeGraph{ typeToIDMap = Map.empty, idToTypeMap = Map.empty }
initialTypeGraph = foldl' (flip insertType) emptyTypeGraph (map (first SutPrimitiveType) primitiveIDs)
initialNextTypeID = length primitiveIDs

insertType :: (SutType, SutTypeID) -> TypeGraph -> TypeGraph
insertType (t,id) TypeGraph{typeToIDMap = l, idToTypeMap = r} = TypeGraph{typeToIDMap = l', idToTypeMap = r'}
  where l' = Map.insert t id l
        r' = Map.insert id t r

lookupTypeID :: SutType -> TypeGraph -> Maybe SutTypeID
lookupTypeID t g = Map.lookup t $ typeToIDMap g

lookupType :: SutTypeID -> TypeGraph -> Maybe SutType
lookupType id g = Map.lookup id $ idToTypeMap g

orderedGraph :: TypeGraph -> [(SutTypeID, SutType)]
orderedGraph g = sortOn snd $ Map.toList $ idToTypeMap g
