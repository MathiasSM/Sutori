{-|
Description : Defines ways to log information, errors and the like

Provides a common framework (class) to 'showSut' sutori components/elements/constructs.
Right now, error codes are defined here.
-}
module Sutori.Logger
( SutLog(SutLogLeave, SutLogNode)
, SutShow(showSut)
, SutLogger(SutLogger,logInfo, logError)
, SutError(..)
, fromLeave
) where

import Data.List (intercalate)


-- |Different possible Sutori Errors
data SutError = LexicalError          -- ^ An unknown or malformed token was read
              | GrammaticalError      -- ^ The source code does not follow Sutori grammar
              | TypeError             -- ^ A Type error occurred
              | UndefinedSymbolError  -- ^ A symbol was used before it was defined
              | ArgumentsNumberError  -- ^ A function was called with the wrong number of arguments
              | DuplicateSymbolError  -- ^ A symbol was defined twice in the same scope
              | InternalError         -- ^ The compiler failed for a known reason (not the user's fault)
              | NoError               -- ^ The lack of error
  deriving Show

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
    showNested lvl (SutLogNode s nx) = showIndent lvl ++ s ++ "\n" ++ showChildren (lvl+1) nx
    -- |Prints a (nested) list of children logs
    showChildren :: Int -> [SutLog] -> String
    showChildren lvl nx = intercalate "\n" $ map (showNested lvl) nx
    -- |Prints the required indent for the level
    showIndent :: Int -> String
    showIndent n = (concat . replicate n) "  "

-- |Sutori logger, where we register logs for 'reasons', via a WriterT
data SutLogger = SutLogger {
  logInfo :: [SutLog],                -- ^ The ``info'' log
  logWarning :: [SutLog],             -- ^ The ``warning'' log
  logError :: [(SutError, SutLog)]    -- ^ The ``error'' log
}

-- |SutLogger must be a Monoid to be used with WriterT
instance Monoid SutLogger where
  -- |An empty logger, is the set of empty logs
  mempty = SutLogger [] [] []
  -- |Joining logs is concatenating the every pair of lists
  mappend (SutLogger info warn err) (SutLogger info' warn' err') =
    SutLogger (info++info') (warn++warn') (err++err')

-- |The way we print the logs is to print info logs, then error logs.
instance Show SutLogger where
  show (SutLogger info warn err) = concatMap show info
                                 ++ concatMap show warn
                                 ++ concatMap show err

-- |Extract log strings
fromLeave :: SutLog -> String
fromLeave (SutLogLeave l) = l

-- |Interface for pretty-printing Sutori constructs (token, actions, tables, ...)
class SutShow a where
  -- |Allows to pretty-print a Sutori (internal or external) element
  showSut :: a -> SutLog
