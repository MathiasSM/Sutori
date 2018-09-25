module Sutori.Types.Constructors
( SutID
, SutTypeID
, SutType(..)
) where

import Data.List(intercalate)

import Sutori.Types.Primitives(SutPrimitive, SutTypeID)


-- A Sutori ID is a string
type SutID = String

-- Sutori type constructors
data SutType  = SutPrimitiveType SutPrimitive
              | SutDirection SutTypeID
              | SutChain Int SutTypeID
              | SutMachine [(SutID, SutTypeID)]
              | SutThing [(SutID, SutTypeID)]
              deriving (Eq, Ord)
