{-|
Description : Provides definition functions that add the appropiate symbols to the table,
              given that all checks passed and the definition is legal.
-}
module Sutori.Parser.Definitions where

import Control.Monad            (filterM, when)
import Control.Monad.State.Lazy (get, put)
import Data.List                (find)
import Data.Maybe               (catMaybes, fromJust, isJust)

import Sutori.AST      (SutID, SutExpression, SutAST, SutModule(SutModule), SutInstruction(..))
import Sutori.Error    (duplicateSymbolError)
import Sutori.Monad    (SutMonad, SutState(SutState, parserTable, mainModule), parserCurrentScope)
import Sutori.Types    (SutTypeID)
import Sutori.SymTable
  ( SymPerson(..), SymVariable(..), SymType(..), SymModule(..), SymFunction(..)
  , SutSymbol'(..)
  , SymbolCat(..), SutParam
  , TypedSymbol(..), ParametricSymbol(..)
  , lookupSymbolsPerson, lookupSymbolsFunction, lookupSymbolsModule, lookupSymbolsVariable, lookupSymbolsType
  , insertSymbol, updateSymbol)

import Sutori.Parser.Expressions (assignment)
import Sutori.Parser.Symbols     (findVariable, findFunction)

-- |Includes a new person into the story
--
-- TODO: Refactor code. all these functions use mostly repeated code
defPerson :: SutID -> SutMonad ()
defPerson pid = do
  state@SutState{ parserTable = table } <- get
  let ps = lookupSymbolsPerson pid table
  case ps of
    (SymPerson{}:_)  -> do
      duplicateSymbolError pid CatPerson ""
      return ()
    _ -> do
      let currentScope = parserCurrentScope state
          person       = SymPerson' $ SymPerson pid currentScope
      put state{ parserTable = insertSymbol person table }

-- |Defines a new variable of given type and optionally assigns it an initial value.
defVariable :: SutID -> SutTypeID -> (SutID, Maybe SutExpression) -> SutMonad (Maybe SutInstruction)
defVariable pid tid (id, mexp) = do
  state@SutState{ parserTable = table } <- get
  case lookupSymbolsVariable id table of
    (SymVariable{}:_)  -> duplicateSymbolError id CatVariable ""
    _ -> do
      let currentScope = parserCurrentScope state
          variable     = SymVariable' $ SymVariable id currentScope tid
      put state{ parserTable = insertSymbol variable table }

  case mexp of
    Just exp -> return $ Just (InstExpression exp)
    Nothing  -> return Nothing

-- |Defined a list of variables and returns a list of instructions, if any definition included an assigment
defVariables :: SutID -> SutTypeID -> [(SutID, Maybe SutExpression)] -> SutMonad [SutInstruction]
defVariables id tid defs = do
  ms <- mapM (defVariable id tid) defs
  return $ catMaybes ms

-- |Associates the SutID to the newly constructed type, assuming the name has not been used before
defType :: SutID -> SutID -> SutTypeID -> SutMonad ()
defType pid id tid = do
  state@SutState{ parserTable = table } <- get
  let ts = lookupSymbolsType id table
  case ts of
    (SymType{}:_)  -> do
      duplicateSymbolError id CatType ""
      return ()
    _ -> do
      let currentScope = parserCurrentScope state
          variable     = SymType' $ SymType id currentScope tid
      put state{ parserTable = insertSymbol variable table }


-- |Defines a module for importing/exporting
--
-- Right now it's basically a stub as there's only one module and
-- the data structure only keeps the 'SutID' and "AST"
defModule :: SutID -> SutAST -> SutMonad ()
defModule id b = do
  state <- get
  put state{ mainModule = SutModule id b }


-- |Defines a AST-able function (updates the top function with this ID's AST)
defineFunction :: SutID -> SutAST -> SutMonad ()
defineFunction id ast = do
  state@SutState{ parserTable = table } <- get
  let (SymFunction _ tid ps ast':_) = lookupSymbolsFunction id table
  case ast' of
    Just _  ->
      duplicateSymbolError id CatFunction ("Function '"++ id++"' has already been defined before.")
    Nothing   -> do
      let func = SymFunction' $ SymFunction id tid ps (Just ast)
      put state{ parserTable = updateSymbol func table }


-- |Inserts a new function symbol with a given name, type and parameters,
-- if none is present (no body yet)
insertFunction :: SutID -> SutTypeID -> [SutParam] -> SutMonad SutID
insertFunction id tid ps = do
  state@SutState{ parserTable = table } <- get
  case lookupSymbolsFunction id table of
    (f@SymFunction{}:_)  -> do
      when (symType f /= tid) $
        duplicateSymbolError id CatFunction ("There is another function named '"++ id++"' with different typeID `"++show tid++"`")
      when (symParams f /= ps) $
        duplicateSymbolError id CatFunction ("There is another function named '"++ id++"' with different parameters")
    _ -> do
      let func = SymFunction' $ SymFunction id tid ps Nothing
      put state{ parserTable = insertSymbol func table }
  return id
