module Sutori.Parser.Symbols where

import qualified Data.Map.Strict as Map
import Control.Monad.State (get, put)

import Sutori.Monad (SutMonad, SutState(SutState, typesGraph, typesNextID))
import Sutori.Utils (SutID)
import Sutori.Types (SutType, SutTypeID, TypeGraph(TypeGraph))


-- Finds the existent typeID or inserts the type and gets the new ID
findType :: SutType -> SutMonad SutTypeID
findType t = do
  oldState@SutState { typesGraph = TypeGraph graph, typesNextID = nextID } <- get
  case Map.lookup t graph of
    Just tid -> return tid
    Nothing -> let newGraph = Map.insert t nextID graph
                in put oldState { typesGraph = TypeGraph newGraph, typesNextID = nextID + 1 } >> return nextID
