module Sutori.Logger
( SutLog(SutLogLeave, SutLogNode)
, SutShow(showSut)
, SutLogger(SutLogger,logInfo, logError)
, SutError(..)
, fromLeave
) where

import Data.List (intercalate)

-- Different possible Sutori Errors
data SutError = LexicalError | GrammaticalError | TypeError | InternalError | NoError
  deriving Show

-- Simple tree data structure to allow pretty printing of sutori logs
data SutLog = SutLogLeave String
            | SutLogNode String [SutLog]

instance Show SutLog where
  show = showNested 0
   where
    showNested :: Int -> SutLog -> String
    showNested lvl (SutLogLeave s)   = showIndent lvl ++ s
    showNested lvl (SutLogNode s nx) = showIndent lvl ++ s ++ "\n" ++ showChildren (lvl+1) nx

    showChildren :: Int -> [SutLog] -> String
    showChildren lvl nx = intercalate "\n" $ map (showNested lvl) nx

    showIndent :: Int -> String
    showIndent n = (concat . replicate n) "  "

-- The logger is just a string for the time being
-- newtype SutLogger = SutLogger {getLog :: String}
-- instance Monoid SutLogger where
--   mempty = SutLogger ""
--   mappend (SutLogger a) (SutLogger b) = SutLogger (a++b)
-- instance Show SutLogger where
--   show (SutLogger a) = a
data SutLogger = SutLogger {logInfo :: [SutLog], logError :: [(SutError, SutLog)]}

instance Monoid SutLogger where
  mempty = SutLogger [] []
  mappend (SutLogger info err) (SutLogger info' err') = SutLogger (info++info') (err++err')

-- We'll print the log this way, probably. Need a better formatting
instance Show SutLogger where
  show (SutLogger info err) = concatMap show info ++ concatMap show err

-- Extract log strings
fromLeave :: SutLog -> String
fromLeave (SutLogLeave l) = l

-- Interface for showing Sutori constructs (token, actions, tables, ...)
class SutShow a where
  showSut :: a -> SutLog
