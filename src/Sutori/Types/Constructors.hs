module Sutori.Types.Constructors
( SutID
, SutTypeID
, SutType(..)
) where

import Data.List(intercalate)

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave))
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

instance SutShow SutType where
  showSut (SutPrimitiveType t) = showSut t
  showSut (SutDirection t) = SutLogLeave $ "Direction to type " ++ show t
  showSut (SutChain i t)   = SutLogLeave $ "Chain of size " ++ show i ++ " of type " ++ show t
  showSut (SutMachine ms)  = SutLogLeave $ "Machine with " ++ showMembers " and a " ms
  showSut (SutThing ms)    = SutLogLeave $ "Thing that is either " ++ showMembers " or a " ms


-- Helper function to print a list of members
showMembers :: String -> [(SutID, SutTypeID)] -> String
showMembers sep ms = intercalate sep (map (\(id, tid) -> (id ++ " of type " ++ show tid)) ms)
