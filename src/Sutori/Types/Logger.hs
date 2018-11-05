{-|
Description : Provides 'ShowSut' instances for "Sutori.Types"
-}
module Sutori.Types.Logger() where

import Data.List(intercalate)

import Sutori.Utils               (SutID)
import Sutori.Logger              (SutShow(showSut), SutLog(SutLogLeave, SutLogNode), fromLeave)

import Sutori.Types.Graph         (TypeGraph, orderedGraph)
import Sutori.Types.Primitives    (SutPrimitive(..))
import Sutori.Types.Constructors  (SutType(..), SutTypeID)

-- |The graph prints as a list of types
instance SutShow TypeGraph where
  showSut g = SutLogNode "Type Graph" $ map showType $ orderedGraph g
    where showType (tid, (t, s)) = SutLogLeave $ "Type[" ++ show tid ++ "] ("++show s++"): " ++ (fromLeave . showSut) t

-- |Each primitive prints as its natural name
instance SutShow SutPrimitive where
  showSut SutBag           = SutLogLeave "Bag (Int)"
  showSut SutWallet        = SutLogLeave "Wallet (Float)"
  showSut SutPhrase        = SutLogLeave "Phrase (String)"
  showSut SutLight         = SutLogLeave "Light (Bool)"
  showSut SutLetter        = SutLogLeave "Letter (Char)"
  showSut SutTypeVoid      = SutLogLeave "No Type (Void)"
  showSut SutTypeError     = SutLogLeave "Type error"

-- |Each type construct prints nicely
instance SutShow SutType where
  showSut (SutPrimitiveType t)  = showSut t
  showSut (SutDirection t)      = SutLogLeave $ "Direction to type " ++ show t
  showSut (SutChain i t)        = SutLogLeave $ "Chain of size " ++ show i ++ " of type " ++ show t
  showSut (SutMachine ms)       = SutLogLeave $ "Machine with a " ++ showMembers " and a " ms
  showSut (SutThing ms)         = SutLogLeave $ "Thing that is either a " ++ showMembers " or a " ms

-- |Print a list of members
showMembers :: String -> [(SutID, SutTypeID)] -> String
showMembers sep ms = intercalate sep (map (\(sid, tid) -> ("'" ++sid ++ "' of type " ++ show tid)) ms)
