{-|
Description : Provides definition functions that add the appropiate symbols to the table,
              given that all checks passed and the definition is legal.
-}
module Sutori.Parser.Definitions where

import Control.Monad.State.Lazy (get, put)
import Data.List               (find)

import Sutori.AST              (SutExpression, SutBlock, SutModule)
import Sutori.Monad            (SutMonad, SutState(SutState, parserTable), parserCurrentScope)
import Sutori.Monad.Logger     (duplicateSymbolError)
import Sutori.SymTable         (SutSymbol(symCat), SutParamKind, SutSymCategory(..), SutSymOther(SymTypeDef), lookupID, insert)
import Sutori.Types.Primitives (SutTypeID)
import Sutori.Utils            (SutID)

-- Declarations / Definitions
defPerson :: SutID -> SutMonad ()
defPerson = error "defPerson"

-- Define a function with ID pushed and no parameters
defFunction :: SutTypeID -> SutBlock -> SutMonad ()
defFunction = error "defFunction"

-- Define a function with ID and parameters pushed
defFunction' :: SutTypeID -> SutBlock -> SutMonad ()
defFunction' = error "defFunction'"

insertFunctionID :: SutID -> SutMonad SutID
insertFunctionID = error "insertFunctionID"

insertParam :: (SutParamKind, SutTypeID, SutID) -> SutMonad ()
insertParam (pt, tid, id) = error "insertParams"

defVariable :: SutID -> SutTypeID -> (SutID, Maybe SutExpression) -> SutMonad ()
defVariable = error "defVariable"

-- |Associates the SutID to the newly constructed type, assuming the name has not been used before
defType :: SutID -> SutID -> SutTypeID -> SutMonad ()
defType pid id tid = do
  state@SutState{ parserTable = table } <- get
  let syms = lookupID table id
      sym  = find (isType . symCat) syms
        where isType :: SutSymCategory -> Bool
              isType CatType = True
              isType _       = False
  case sym of
    Just s  -> do
      duplicateSymbolError id CatType ("Type '" ++ id ++ "' already present in the current story")
      return ()
    Nothing -> do
      let newTable     = insert table currentScope CatType 0 (SymTypeDef tid) [id]
          currentScope = parserCurrentScope state
      put state{ parserTable = newTable }


defModule :: SutID -> SutBlock -> SutMonad ()
defModule = error "defModule"
