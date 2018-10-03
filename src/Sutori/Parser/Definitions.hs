{-|
Description : Provides definition functions that add the appropiate symbols to the table,
              given that all checks passed and the definition is legal.
-}
module Sutori.Parser.Definitions where

import Control.Monad.State.Lazy (get, put)
import Data.List                (find)

import Sutori.AST               (SutExpression, SutBlock, SutModule)
import Sutori.Monad             (SutMonad, SutState(SutState, parserTable), parserCurrentScope)
import Sutori.Monad.Logger      (duplicateSymbolError)
import Sutori.Types.Primitives  (SutTypeID)
import Sutori.Utils             (SutID)

import Sutori.SymTable
  ( SutSymbol(symCat), SutParamKind(..), SutSymCategory(..), SutSymOther(..)
  , lookupID, insert
  , isPerson, isType, isVariable)


-- |Includes a new person into the story
--
-- TODO: Refactor code. all these functions use mostly repeated code
defPerson :: SutID -> SutMonad ()
defPerson pid = do
  state@SutState{ parserTable = table } <- get
  let syms = lookupID table pid
      sym  = find (isPerson . symCat) syms
  case sym of
    Just s  -> do
      duplicateSymbolError pid CatPerson ""
      return ()
    Nothing -> do
      let newTable     = insert table currentScope CatPerson 0 SymNothing [pid]
          currentScope = parserCurrentScope state
      put state{ parserTable = newTable }


-- |Defines a new variable of given type and optionally assigns it an initial value.
--
-- TODO: Add assignment to AST (separate from definition, which does not go into AST)
defVariable :: SutID -> SutTypeID -> (SutID, Maybe SutExpression) -> SutMonad ()
defVariable pid tid (id, mexp) = do
  state@SutState{ parserTable = table } <- get
  let syms = lookupID table id
      sym  = find (isVariable . symCat) syms
  case sym of
    Just s  -> do
      duplicateSymbolError id CatType ""
      return ()
    Nothing -> do
      let newTable     = insert table currentScope CatType 0 (SymTypeDef tid) [id]
          currentScope = parserCurrentScope state
      put state{ parserTable = newTable }


-- |Associates the SutID to the newly constructed type, assuming the name has not been used before
defType :: SutID -> SutID -> SutTypeID -> SutMonad ()
defType pid id tid = do
  state@SutState{ parserTable = table } <- get
  let syms = lookupID table id
      sym  = find (isType . symCat) syms
  case sym of
    Just s  -> do
      duplicateSymbolError id CatType ""
      return ()
    Nothing -> do
      let newTable     = insert table currentScope CatType 0 (SymTypeDef tid) [id]
          currentScope = parserCurrentScope state
      put state{ parserTable = newTable }


-- |Defines a module for importing/exporting
--
-- Right now it's basically a stub as there's only one module and
-- the data structure only keeps the 'SutID' and "AST"
defModule :: SutID -> SutBlock -> SutMonad ()
defModule = error "defModule"


-- |Defines a function with ID pushed and no parameters
defFunction :: SutTypeID -> SutBlock -> SutMonad ()
defFunction = error "defFunction"

-- |Defines a function with ID and parameters already present
defFunction' :: SutTypeID -> SutBlock -> SutMonad ()
defFunction' = error "defFunction'"

-- |Defines a new function with a given name (no parameters or body yet)
insertFunctionID :: SutID -> SutMonad SutID
insertFunctionID = error "insertFunctionID"

-- |Defines a new parameter in the last function defined
insertParam :: (SutParamKind, SutTypeID, SutID) -> SutMonad ()
insertParam (pt, tid, id) = error "insertParams"
