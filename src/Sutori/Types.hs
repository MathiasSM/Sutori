{-|
Description : API for handling Sutori type system.

TODO: Sort and document API
-}
module Sutori.Types
( SutTypeID
, SutType(..)
, primitiveType
, generalizeTypes
, primitiveError
, TypeGraph(TypeGraph)
, initialTypeGraph
, initialNextTypeID
, insertType
, lookupTypeID
, lookupType
, orderedGraph
, SutPrimitive(..)
, primitives
, primitiveID
, primitiveIDs
, generalizePrimitives
, toTypeLight
, toTypeNum
, toTypeBag
, toTypeWallet
, toTypePhrase
, toTypeSortable
, toTypeEq
, memberOffset
) where

import Sutori.Types.Primitives
import Sutori.Types.Constructors
import Sutori.Types.Graph
import Sutori.Types.Logger ()
