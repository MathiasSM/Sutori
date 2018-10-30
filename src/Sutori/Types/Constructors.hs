{-|
Description : Defines the diferent type constructors for the language
              and some functions for coersion and generalization
-}
module Sutori.Types.Constructors
( SutTypeID
, SutType(..)
, primitiveType
, generalizeTypes
, primitiveError
) where

import Data.List(intercalate)

import Sutori.Utils            (SutID)

import Sutori.Types.Primitives (SutPrimitive(SutTypeError), SutTypeID, generalizePrimitives)

-- |Sutori type constructors
data SutType  = SutPrimitiveType SutPrimitive   -- ^ A Primitive Type
              | SutDirection SutTypeID          -- ^ A Direction (Pointer) to a value of some type
              | SutChain Int SutTypeID          -- ^ A Chain (Array) of fixed size of element type
              | SutMachine [(SutID, SutTypeID)] -- ^ A Machine (Struct) of different components
              | SutThing [(SutID, SutTypeID)]   -- ^ A Thing (Union) that might have different names
  deriving (Eq, Ord, Show)

-- |Extract the primitive from the type definition
primitiveType :: SutType -> SutPrimitive
primitiveType (SutPrimitiveType p) = p
primitiveType _                    = SutTypeError

-- |Generalize two types to their LCA, if any
generalizeTypes :: SutType -> SutType -> SutType
generalizeTypes t1 t2 = let p1 = primitiveType t1
                            p2 = primitiveType t2
                         in SutPrimitiveType $ generalizePrimitives p1 p2

-- |Constant primitive error type
primitiveError :: SutType
primitiveError = SutPrimitiveType SutTypeError
