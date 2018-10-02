module Sutori.Parser.Symbols where

import qualified Data.Map.Strict as Map
import Control.Monad.State       (get, put)
import Data.List                 (find)
import Data.Maybe                (fromJust)

import Sutori.AST                (SutExpression)
import Sutori.Monad              (SutMonad, SutState(SutState, typesGraph, typesNextID, parserTable))
import Sutori.SymTable           (SutSymbol(symCat), SutSymCategory(CatType), symTypeDef, lookupID)
import Sutori.Types.Constructors (SutType(SutPrimitiveType))
import Sutori.Types.Graph        (TypeGraph(TypeGraph), lookupType, lookupTypeID, insertType)
import Sutori.Types.Primitives   (SutTypeID)
import Sutori.Utils              (SutID)


-- Finds the existent typeID or inserts the type and gets the new ID
findTypeID :: SutType -> SutMonad SutTypeID
findTypeID t = do
  oldState@SutState { typesGraph = graph, typesNextID = nextID } <- get
  case lookupTypeID t graph of
    Just tid -> return tid
    Nothing -> let newGraph = insertType (t, nextID) graph
                in put oldState { typesGraph = newGraph, typesNextID = nextID + 1 } >> return nextID

-- Finds the typeID from a SutID
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
    Nothing -> error "No type symbol found from SutID"

-- Finds an existent type from its ID
findExistentType :: SutTypeID -> SutMonad SutType
findExistentType tid = get >>= \SutState{typesGraph = tg} -> retJust (lookupType tid tg)
  where retJust = return . fromJust

findPerson :: SutID -> SutMonad SutID
findPerson id = error "findPerson"

findFunction :: SutID -> SutMonad SutID
findFunction id = error "findFunction"

findVariable :: SutID -> SutMonad SutExpression
findVariable id = error "findVariable"
