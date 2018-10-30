{-|
Description : Defines ways to log information, errors and the like

Provides a common framework (class) to 'showSut' sutori components/elements/constructs.
-}
module Sutori.Logger.Log
( SutShow(..)
, SutLog(SutLogLeave, SutLogNode)
, fromLeave
) where

import Data.List (intercalate)
import Sutori.Error.Error

-- |Interface for pretty-printing Sutori constructs (token, actions, tables, ...)
class SutShow a where
  -- |Allows to pretty-print a Sutori (internal or external) element
  showSut :: a -> SutLog

-- |Simple tree data structure to allow pretty printing of sutori logs
data SutLog = SutLogLeave String           -- ^ A final message
            | SutLogNode String [SutLog]   -- ^ A message with children messages

-- |'SutLog' can be pretty printed on a console
instance Show SutLog where
  show = showNested 0
   where
    -- |Prints a nested log
    showNested :: Int -> SutLog -> String
    showNested lvl (SutLogLeave s)   = showIndent lvl ++ s
    showNested lvl (SutLogNode s []) = showIndent lvl ++ s ++ "\n" ++ showChildren (lvl+1) [SutLogLeave ""]
    showNested lvl (SutLogNode s nx) = showIndent lvl ++ s ++ "\n" ++ showChildren (lvl+1) nx
    -- |Prints a (nested) list of children logs
    showChildren :: Int -> [SutLog] -> String
    showChildren lvl nx = intercalate "\n" $ map (showNested lvl) nx
    -- |Prints the required indent for the level
    showIndent :: Int -> String
    showIndent n = (concat . replicate n) "|   "

-- |Extract log strings
fromLeave :: SutLog -> String
fromLeave (SutLogLeave l) = l
