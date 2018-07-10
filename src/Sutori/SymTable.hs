module Sutori.SymTable
(
  Scope, SymMap, SymTable(..),
  Symbol(..), SymCategory(..), SymOther(..),
  insert
) where

import Sutori.Utils


type Scope = Int

type SymMap = Map.Map String [Symbol]

newtype SymTable = SymTable {
  getHash :: SymMap
} deriving (Eq,Show)

-- Symbol
data Symbol = Symbol {
  getId       :: String,
  getCategory :: SymCategory,
  getScope    :: Scope,
  getType     :: SutType,
  getOther    :: SymOther
} deriving (Show, Eq)

-- Symbol Category
data SymCategory = ModuleSym
                 | FunctionSym
                 | PersonSym
                 | VarSym
                 | ParamSym
                 | TypeSym
                 deriving (Eq,Show)

-- Symbol Other
data SymOther  = FunctionAST {getBlock :: SutBlock, getParams :: [SutParam]}
               | IsReference Bool
               | NoOther
               deriving (Eq,Show)

instance SutPrint Symbol where
  printSut (Symbol id cat sc t other) = "Symbol "++id++" (T: "++show t++", Cat: "++show cat++", Scope: "++show sc++")"

insertWith :: SymTable -> [SymID] -> (SymTable -> SymID -> SymTable) -> SymTable
insertWith table ids f = SymTable $ foldl f table ids

insert :: SymTable -> Scope -> SutCategory -> SutType -> SutOther -> [SymID]
insert table curScope category t o ids = insertWith table ids f
  where f table' tag = let newSymbol  = Symbol tag category curScope t o
                           newSymList = newSymbol : Map.findWithDefault [] tag table'
                        in Map.insert tag newList table'
