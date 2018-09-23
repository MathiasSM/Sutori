module Sutori.Types.Logger() where

import Data.List(intercalate)

import Sutori.Utils(SutID)
import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))

import Sutori.Types.Graph(TypeGraph(TypeGraph), orderedGraph)
import Sutori.Types.Primitives(SutPrimitive(..))
import Sutori.Types.Constructors(SutType(..), SutTypeID)

-- The graph prints as a list of types
instance SutShow TypeGraph where
  showSut g = SutLogNode "Type Graph" $ map showType $ orderedGraph g
    where showType (c,v) = let (SutLogLeave s) = showSut c in SutLogLeave $ show v ++ s

-- Each primitive prints as its natural name
instance SutShow SutPrimitive where
  showSut SutBag           = SutLogLeave "Bag"
  showSut SutWallet        = SutLogLeave "Wallet"
  showSut SutPhrase        = SutLogLeave "Phrase"
  showSut SutLight         = SutLogLeave "Light"
  showSut SutLetter        = SutLogLeave "Letter"
  showSut SutTypeVoid      = SutLogLeave "No Type"
  showSut SutTypeError     = SutLogLeave "Type error"

instance SutShow SutType where
  showSut (SutPrimitiveType t) = showSut t
  showSut (SutDirection t) = SutLogLeave $ "Direction to type " ++ show t
  showSut (SutChain i t)   = SutLogLeave $ "Chain of size " ++ show i ++ " of type " ++ show t
  showSut (SutMachine ms)  = SutLogLeave $ "Machine with " ++ showMembers " and a " ms
  showSut (SutThing ms)    = SutLogLeave $ "Thing that is either " ++ showMembers " or a " ms

-- Helper function to print a list of members
showMembers :: String -> [(SutID, SutTypeID)] -> String
showMembers sep ms = intercalate sep (map (\(id, tid) -> (id ++ " of type " ++ show tid)) ms)
