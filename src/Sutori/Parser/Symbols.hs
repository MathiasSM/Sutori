{-|
Description : Defines finder functions for symbols: Most assume the symbol exist, so
              an error for undefined symbol is logged if they can't find it
-}
module Sutori.Parser.Symbols where

import qualified Data.Map.Strict as Map
import Control.Monad             (unless)
import Control.Monad.State       (get, put)
import Data.List                 (find)
import Data.Maybe                (fromJust, isJust)

import Sutori.AST       (SutID, SutExpression(ExprID))
import Sutori.Monad     (SutMonad, SutState(SutState, typesGraph, typesNextID, parserTable))
import Sutori.Error     (undefinedError)
import Sutori.SymTable  (SutSymbol(..), SutSymCategory(..), SutSymOther(..), lookupID, symTypeDef)
import Sutori.Types     (SutType(SutPrimitiveType), primitiveError, TypeGraph(TypeGraph), lookupType, lookupTypeID, insertType, SutTypeID)


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
  let syms = lookupID table id
      sym  = find (isType . symCat) syms
        where isType :: SutSymCategory -> Bool
              isType CatType = True
              isType _       = False
  case sym of
    Just s  -> return $ symTypeDef s
    Nothing -> do
      undefinedError id CatType ("Type '" ++ id ++ "' not present in the current story")
      findTypeID primitiveError


-- |Finds an existent type from its ID
findExistentType :: SutTypeID -> SutMonad SutType
findExistentType tid = get >>= \SutState{typesGraph = tg} -> retJust (lookupType tid tg)
  where retJust = return . fromJust

-- |Checks if the given person already exists
findPerson :: SutID -> SutMonad SutID
findPerson id = do
  SutState { parserTable = table } <- get
  let syms = lookupID table id
      sym  = find (isPerson . symCat) syms
        where isPerson :: SutSymCategory -> Bool
              isPerson CatPerson = True
              isPerson _         = False
  unless (isJust sym) $ undefinedError id CatPerson ("Person '" ++ id ++ "' not present in the current story")
  return id

-- |Checks if the given function already exists
findFunction :: SutID -> SutMonad SutID
findFunction id = do
  SutState { parserTable = table } <- get
  let syms = lookupID table id
      sym  = find (isFunction . symCat) syms
        where isFunction :: SutSymCategory -> Bool
              isFunction CatFunction = True
              isFunction _           = False
  unless (isJust sym) $ undefinedError id CatFunction ("Function '" ++ id ++ "' not present in the current story")
  return id

-- |Checks if the given variable already exists, returns the ID wrapped as an expression
findVariable :: SutID -> SutMonad SutExpression
findVariable id = do
  SutState { parserTable = table } <- get
  let syms = lookupID table id
      sym  = find (isVariable . symCat) syms
        where isVariable :: SutSymCategory -> Bool
              isVariable CatVariable = True
              isVariable _           = False
  case sym of
    Just s  -> do
      t <- findExistentType (symType s)
      return $ ExprID t id
    Nothing -> do
      unless (isJust sym) $ undefinedError id CatFunction ("Function '" ++ id ++ "' not present in the current story")
      return $ ExprID primitiveError id
