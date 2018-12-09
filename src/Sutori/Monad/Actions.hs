{-|
  Description : General-use monadic actions for 'SutMonad'
-}
module Sutori.Monad.Actions
( whenVerbose
, runSutMonad
, insertScope
, removeScope
, parserCurrentScope
, setErrorCode
, functionStarts
) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Set as Set
import qualified Data.Map as Map

import Sutori.Error.Error (SutError)

import Sutori.Monad.Monad
import Sutori.Monad.State

-- |Run a monadic action only if verbose if turned on
whenVerbose :: SutMonad () -> SutMonad ()
whenVerbose f = get >>= \SutState{logVerbose = v} -> when v f

-- |Inserts a new scope into the parse
--
-- - Inserts the scope into the open scopes set.
-- - Pushes the scope into the open scopes stack.
-- - Saves the current offset to return to it once the scope is closed.
insertScope :: SutMonad ()
insertScope = do
  oldState@SutState
    { parserNextScope = newScope
    , parserScopes    = scopes
    , parserOffsetStk = offsets
    , parserOffset    = offset
    , parserStack     = stack } <- get

  let newSet       = Set.insert newScope scopes
      newStack     = newScope : stack
      newOffsetStk = offset : offsets

  put $ oldState
    { parserStack     = newStack
    , parserNextScope = newScope + 1
    , parserScopes    = newSet
    , parserOffsetStk = newOffsetStk }

functionStarts :: SutMonad ()
functionStarts = get >>= \s -> put s{ parserOffset = 0 }

-- |Removes last scope from the parse
--
-- - Pops closing scope from stack of open scopes.
-- - Removes closing scope from set of open scopes.
-- - Recovers previous offset to write new data over just-closed scope
removeScope :: SutMonad ()
removeScope = do
  oldState@SutState
    { parserStack     = (currentScope:newStack)
    , parserScopes    = scopes
    , parserOffsetStk = (prevOffset:offsets) } <- get

  put $ oldState
    { parserStack     = newStack
    , parserScopes    = Set.delete currentScope scopes
    , parserOffset    = prevOffset
    , parserOffsetStk = offsets}

-- |Set the current error code for the compilation
setErrorCode :: SutError -> SutMonad ()
setErrorCode err = get >>= \s -> put s{ errorCode = err }
