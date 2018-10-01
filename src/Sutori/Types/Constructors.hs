module Sutori.Types.Constructors
( SutTypeID
, SutType(..)
, primitiveType
, generalizeTypes
) where

import Data.List(intercalate)

import Sutori.Types.Primitives (SutPrimitive(SutTypeError), SutTypeID, generalizePrimitives)
import Sutori.Utils            (SutID)

-- Sutori type constructors
data SutType  = SutPrimitiveType SutPrimitive
              | SutDirection SutTypeID
              | SutChain Int SutTypeID
              | SutMachine [(SutID, SutTypeID)]
              | SutThing [(SutID, SutTypeID)]
              deriving (Eq, Ord)

primitiveType :: SutType -> SutPrimitive
primitiveType (SutPrimitiveType p) = p
primitiveType _                    = SutTypeError

generalizeTypes :: SutType -> SutType -> SutType
generalizeTypes t1 t2 = let p1 = primitiveType t1
                            p2 = primitiveType t2
                         in SutPrimitiveType $ generalizePrimitives p1 p2
