module Sutori.Types.Graph
( TypeGraph(TypeGraph)
, TypeGraphState
, initialTypeGraphState
, orderedGraph
) where

import qualified Data.Map.Strict as Map
import Data.List(sortOn)
import Control.Arrow(first)

import Sutori.Types.Primitives(primitiveIDs)
import Sutori.Types.Constructors(SutType(SutPrimitiveType), SutID, SutTypeID)


-- The type graph implemented as a stateful hashmap
newtype TypeGraph = TypeGraph (Map.Map SutType SutTypeID)

type TypeGraphState = (TypeGraph, SutTypeID)

initialTypeGraphState :: TypeGraphState
initialTypeGraphState = (TypeGraph $ Map.fromList (map (first SutPrimitiveType) primitiveIDs), length primitiveIDs)

orderedGraph :: TypeGraph -> [(SutType, SutTypeID)]
orderedGraph (TypeGraph g) = sortOn snd $ Map.toList g
