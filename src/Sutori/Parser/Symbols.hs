{-|
Description : Defines finder functions for symbols: Most assume the symbol exist, so
              an error for undefined symbol is logged if they can't find it
-}
module Sutori.Parser.Symbols where

import Control.Monad.State       (get, put)
import Data.Maybe                (fromJust)

import Sutori.AST       (SutID, SutExpression(ExprID))
import Sutori.Monad     (SutMonad, SutState(SutState, typesGraph, typesNextID, parserTable, parserStack))
import Sutori.Error     (undefinedError)
import Sutori.Types     (SutType, primitiveError, lookupTypeID, lookupType, insertType, SutTypeID)
import Sutori.SymTable
  ( SymPerson(..), SymVariable(..), SymFunction(..)
  , SutSymbol(..), TypedSymbol(symType), SymTable
  , SymbolCat(..), Scope
  , lookupSymbolsVariable, lookupSymbolsPerson, lookupSymbolsType, lookupSymbolsFunction)


-- |Finds an existent 'SutType' from its 'SutTypeID'
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

-- |Finds the 'SutTypeID' from a 'SutID'
findType :: SutID -> SutMonad SutTypeID
findType sid = do
  SutState { parserTable = table, typesGraph = graph, typesNextID = nextID } <- get
  case lookupSymbolsType sid table of
    (s:_) -> return $ symType s
    []    -> do undefinedError sid CatType ("Type '" ++ sid ++ "' not present in the current story")
                findTypeID primitiveError

-- |Checks if the given Person already exists
findPerson :: SutID -> SutMonad SutID
findPerson pid = do
  SutState { parserTable = table } <- get
  case lookupSymbolsPerson pid table of
    (SymPerson{}:_) -> return pid
    _               -> do undefinedError pid CatPerson ("Person '" ++ pid ++ "' not present in the current story")
                          return pid

-- |Checks if the given function already exists, returns the 'SutID'
findFunctionID :: SutID -> SutMonad SutID
findFunctionID fid = do
  f <- findFunction fid
  case f of
    Just s@SymFunction{} -> return $ symID s
    Nothing              -> return fid

-- |Checks if the given function already exists, returns the 'SutSymbol', if any
findFunction :: SutID -> SutMonad (Maybe SymFunction)
findFunction fid = do
  SutState { parserTable = table } <- get
  case lookupSymbolsFunction fid table of
    (f@SymFunction{} : _) -> return $ Just f
    _                   -> do undefinedError fid CatFunction ("Function '" ++ fid ++ "' not present in the current story")
                              return Nothing

-- |Checks if the given variable already exists, returns the 'SutID' wrapped as a 'SutExpression'
findVariable :: SutID -> SutMonad SutExpression
findVariable vid = do
  vars <- lookupInScope lookupSymbolsVariable vid
  case vars of
    (s@SymVariable{}:_) -> do t <- findExistentType (symType s)
                              return $ ExprID t vid (symScope s)
    _                   -> do undefinedError vid CatVariable ("Variable '" ++ vid ++ "' not present in the current story")
                              return $ ExprID primitiveError vid (-1)


-- |Checks the living scopes for the symbols with given 'SutID'
lookupInScope :: SutSymbol a => (SutID -> SymTable -> [a]) -> SutID -> SutMonad [a]
lookupInScope lookupSym sid = get >>= \SutState{parserTable = table} -> inScope (lookupSym sid table)


-- |Crosses the living scopes with the given 'SutSymbol's
--
-- Uses the fact the both lists are ordered in decreasing order of 'SutID'
inScope :: SutSymbol a => [a] -> SutMonad [a]
inScope syms = get >>= \SutState{ parserStack = scopes } -> return $ f scopes syms
  where f :: SutSymbol a => [Scope] -> [a] -> [a]
        f [] _ = []
        f _ [] = []
        f (x:xs) (y:ys) = case compare x (symScope y) of
                            EQ -> y : f (x:xs) ys -- Add the symbol, as it lives on a live scope
                            LT -> f (x:xs) ys     -- Check next symbol, as this was born on a later scope
                            GT -> f xs (y:ys)     -- Check next scope, as this symbol wasn't yet born here
