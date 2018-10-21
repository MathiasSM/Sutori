{-|
  Description : General-use monadic actions for 'SutMonad'
-}
module Sutori.Monad.Actions
( ifVerbose
, runSutMonad
, insertScope
, removeScope
, parserCurrentScope
, setErrorCode
) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set

import Sutori.AST         (SutModule)
import Sutori.Lexer.Posn  (SutPosn, initialPosn)
import Sutori.Logger      (SutLogger, SutLog)
import Sutori.SymTable    (SymTable, Scope)
import Sutori.Types       (SutTypeID, TypeGraph, initialTypeGraph, initialNextTypeID)
import Sutori.Error.Error (SutError(NoError))

import Sutori.Monad.Monad
import Sutori.Monad.State

-- |Run a monadic action only if verbose if turned on
ifVerbose :: SutMonad () -> SutMonad ()
ifVerbose f = get >>= \SutState{logVerbose = v} -> when v f

-- |Inserts a new scope into the parse
insertScope :: SutMonad ()
insertScope = do
  oldState@SutState{parserNextScope = newScope, parserScopes = scopes, parserStack = stack} <- get
  let newSet = Set.insert newScope scopes
      newStack = newScope : stack
  put $ oldState { parserStack = newStack, parserNextScope = newScope + 1, parserScopes = newSet}

-- |Removes last scope from the parse
removeScope :: SutMonad ()
removeScope = do
  oldState@SutState{parserStack = stack, parserScopes = scopes} <- get
  let newSet = Set.delete (head stack) scopes
      newStack = tail stack
  put $ oldState { parserStack = newStack, parserScopes = newSet}

-- |Set the current error code for the compilation
setErrorCode :: SutError -> SutMonad ()
setErrorCode err = get >>= \s -> put s{ errorCode = err }