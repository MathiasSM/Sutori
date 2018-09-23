module Sutori.Types
( SutTypedExpression(getExpressionType)
, TypeGraph(TypeGraph)
, TypeGraphState
, initialTypeGraphState
, findType
) where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad.State

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))

import Sutori.Types.Primitives(primitives)
import Sutori.Types.Constructors(SutType(SutPrimitiveType), SutID, SutTypeID)


-- Class for typed expressions
class SutTypedExpression a where
  getExpressionType :: a -> SutTypeID



-- The type graph implemented as a stateful hashmap
newtype TypeGraph = TypeGraph (Map.Map SutType SutTypeID)

instance SutShow TypeGraph where
  showSut (TypeGraph g) = SutLogNode "Type Graph" $ map showType orderedGraph
    where showType (c,v) = let (SutLogLeave s) = showSut c in SutLogLeave $ show v ++ s
          orderedGraph = sortOn snd $ Map.toList g


type TypeGraphState = (TypeGraph, SutTypeID)

initialTypeGraphState :: TypeGraphState
initialTypeGraphState = (TypeGraph $ Map.fromList $ zip (map SutPrimitiveType primitives) [1..], length primitives)

-- Finds the existent typeID or inserts the type and gets the new ID
findType :: SutType -> State TypeGraphState SutTypeID
findType t = do
  (TypeGraph graph, nextID) <- get
  case Map.lookup t graph of
    Just tid -> return tid
    Nothing -> let newGraph = Map.insert t nextID graph
                in put (TypeGraph newGraph, nextID + 1) >> return nextID
