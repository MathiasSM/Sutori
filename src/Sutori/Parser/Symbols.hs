module Sutori.Parser.Symbols where

import qualified Data.Map.Strict as Map
import Control.Monad.State (get, put)

import Sutori.Monad (SutMonad, SutState(SutState, typesGraph, typesNextID))
import Sutori.Utils (SutID)
import Sutori.Types (SutType(SutPrimitiveType), SutTypeID, TypeGraph(TypeGraph))
import Sutori.Types.Graph (TypeGraph, lookupType, lookupTypeID, insertType)


-- Finds the existent typeID or inserts the type and gets the new ID
findTypeID :: SutType -> SutMonad SutTypeID
findTypeID t = do
  oldState@SutState { typesGraph = graph, typesNextID = nextID } <- get
  case lookupTypeID t graph of
    Just tid -> return tid
    Nothing -> let newGraph = insertType (t, nextID) graph
                in put oldState { typesGraph = newGraph, typesNextID = nextID + 1 } >> return nextID

-- Finds the existent typeID or inserts the type and gets the new ID
findType :: SutTypeID -> SutMonad (Maybe SutType)
findType tid = do
  oldState@SutState { typesGraph = graph, typesNextID = nextID } <- get
  if tid < nextID
     then return $ lookupType tid graph
     else return Nothing
