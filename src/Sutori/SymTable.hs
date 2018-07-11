module Sutori.SymTable
(
  Scope, SymMap, SymTable(..),
  Symbol(..), SymCategory(..), SymOther(..),
  insert, insertParam, updateSymList,
  lookupId, lookupInScope, lookupsInScope
) where

import qualified Data.Map as Map

import Sutori.Utils
import Sutori.AST
import Sutori.Types


type Scope = Int

type SymMap = Map.Map SutID [Symbol]

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

instance SutShow SymCategory where
  showSut ModuleSym = "Module"
  showSut FunctionSym = "Function"
  showSut PersonSym = "Person"
  showSut VarSym = "Variable"
  showSut TypeSym = "Type"

-- Symbol Other
data SymOther  = FunctionAST {getBlock :: SutBlock, getParams :: [SutParam]}
               | IsReference {isReference :: Bool}
               | TypeDef {typeOf :: SutType}
               | NoOther
               deriving (Eq,Show)

instance SutShow Symbol where
  showSut (Symbol id cat sc t other) = "Symbol "++id++" (T: "++show t++", Cat: "++show cat++", Scope: "++show sc++")"


-- Insertion
---------------------------------------------------------------------------------------------------
insertWith table ids f = SymTable $ foldl f (getHash table) ids

insert :: SymTable -> Scope -> SymCategory -> SutType -> SymOther -> [SutID] -> SymTable
insert table curScope category t o ids = insertWith table ids f
  where f hash tag = let newSymbol  = Symbol tag category curScope t o
                         newSymList = newSymbol : Map.findWithDefault [] tag hash
                      in updateSymList hash tag newSymList

insertParam :: SymTable -> Scope -> SymCategory -> [SutParam] -> SymTable
insertParam table curScope category ps = insertWith table ps f
  where f hash (ref, t, id) = let newSymbol  = Symbol id category curScope t (IsReference ref)
                                  newSymList = newSymbol : Map.findWithDefault [] id hash
                               in updateSymList hash id newSymList

updateSymList :: SymMap -> SutID -> [Symbol] -> SymMap
updateSymList hash tag newSymList = Map.insert tag newSymList hash


-- Lookup
---------------------------------------------------------------------------------------------------
lookupInScope :: SymTable -> Scope -> SutID -> [Symbol]
lookupInScope table scope id = filter isHere $ lookupId table id
  where isHere = (== scope) . getScope

lookupId :: SymTable -> SutID -> [Symbol]
lookupId table id = Map.findWithDefault [] id $ getHash table

lookupsInScope :: SymTable -> Scope -> [SutID] -> [[Symbol]]
lookupsInScope table scope = map (lookupInScope table scope)


-- Checks
---------------------------------------------------------------------------------------------------
