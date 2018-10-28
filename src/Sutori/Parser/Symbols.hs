{-|
Description : Defines finder functions for symbols: Most assume the symbol exist, so
              an error for undefined symbol is logged if they can't find it
-}
module Sutori.Parser.Symbols where

import qualified Data.Map as Map
import Control.Monad             (unless)
import Control.Monad.State       (get, put)
import Data.List                 (find)
import Data.Maybe                (fromJust, isJust)

import Sutori.AST       (SutID, SutExpression(ExprID))
import Sutori.Monad     (SutMonad, SutState(SutState, typesGraph, typesNextID, parserTable))
import Sutori.Error     (undefinedError)
import Sutori.Types     (SutType(SutPrimitiveType), primitiveError, TypeGraph(TypeGraph), lookupTypeID, lookupType, insertType, SutTypeID)
import Sutori.SymTable
  ( SymType(..), SymPerson(..), SymVariable(..), SymModule(..), SymFunction(..)
  , SutSymbol(..), TypedSymbol(symType)
  , SymbolCat(..)
  , lookupSymbols
  , lookupSymbolsVariable, lookupSymbolsPerson, lookupSymbolsModule, lookupSymbolsType, lookupSymbolsFunction)


-- |Finds an existent type from its ID
findExistentType :: SutTypeID -> SutMonad SutType
findExistentType tid = get >>= \SutState{typesGraph = tg} -> return $ fromJust (lookupType tid tg)

-- |Finds the existent typeID or inserts the type and gets the new ID
findTypeID :: SutType -> SutMonad SutTypeID
findTypeID t = do
  oldState@SutState { typesGraph = graph, typesNextID = nextID } <- get
  case lookupTypeID t graph of
    Just tid -> return tid
    Nothing -> let newGraph = insertType (t, nextID) graph
                in put oldState { typesGraph = newGraph, typesNextID = nextID + 1 } >> return nextID

-- |Finds the typeID from a SutID
findType :: SutID -> SutMonad SutTypeID
findType id = do
  SutState { parserTable = table, typesGraph = graph, typesNextID = nextID } <- get
  case lookupSymbolsType id table of
    (s:_) -> return $ symType s
    []    -> do undefinedError id CatType ("Type '" ++ id ++ "' not present in the current story")
                findTypeID primitiveError

-- |Checks if the given person already exists
findPerson :: SutID -> SutMonad SutID
findPerson id = do
  SutState { parserTable = table } <- get
  case lookupSymbolsPerson id table of
    (SymPerson{}:_) -> return id
    _               -> do undefinedError id CatPerson ("Person '" ++ id ++ "' not present in the current story")
                          return id

-- |Checks if the given function already exists, returns the ID
findFunctionID :: SutID -> SutMonad SutID
findFunctionID id = do
  f <- findFunction id
  case f of
    Just s@SymFunction{} -> return $ symID s
    Nothing              -> return id

-- |Checks if the given function already exists, returns the symbol, if any
findFunction :: SutID -> SutMonad (Maybe SymFunction)
findFunction id = do
  SutState { parserTable = table } <- get
  case lookupSymbolsFunction id table of
    (f@SymFunction{} : _) -> return $ Just f
    _                   -> do undefinedError id CatFunction ("Function '" ++ id ++ "' not present in the current story")
                              return Nothing

-- |Checks if the given variable already exists, returns the ID wrapped as an expression
findVariable :: SutID -> SutMonad SutExpression
findVariable id = do
  SutState { parserTable = table } <- get
  case lookupSymbolsVariable id table of
    (s@SymVariable{}:_) -> do t <- findExistentType (symType s)
                              return $ ExprID t id
    _                   -> do undefinedError id CatVariable ("Variable '" ++ id ++ "' not present in the current story")
                              return $ ExprID primitiveError id
