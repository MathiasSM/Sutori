{-|
Description : Definition for error codes and other error-related data.
-}
module Sutori.Error.Error
( SutError(..)
) where

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
