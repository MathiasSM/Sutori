module Sutori.SymTable where

import qualified Data.Map as Map

import Sutori.Utils
import Sutori.AST
import Sutori.Lexer
import Sutori.Types


type SutParamTk = (Bool, SutType, SutToken)

type Scope = Int

type SymMap = Map.Map SutID [Symbol]

newtype SymTable = SymTable {
  getHash :: SymMap
} deriving (Eq,Show)

-- Symbol
data Symbol = Symbol {
  getSymToken :: SutToken,
  getCategory :: SymCategory,
  getScope    :: Scope,
  getType     :: SutType,
  getOther    :: SymOther
} deriving (Show, Eq)

getId :: Symbol -> SutID
getId = getString.getToken.getSymToken

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
data SymOther  = FunctionAST {getBlock :: SutBlock, getParams :: [SutParamTk] }
               | IsReference {isReference :: Bool}
               | TypeDef {typeOf :: SutType}
               | NoOther
               deriving (Eq,Show)

instance SutShow Symbol where
  showSut s@(Symbol tk cat sc t other) = "Symbol "++getId s++" (T: "++show t++", Cat: "++show cat++", Scope: "++show sc++", Posn: "++showSut (getPosn tk)++")"


-- Insertion
---------------------------------------------------------------------------------------------------
insertWith table ids f = SymTable $ foldl f (getHash table) ids

insert :: SymTable -> Scope -> SymCategory -> SutType -> SymOther -> [SutToken] -> SymTable
insert table curScope category t o ids = insertWith table ids f
  where f hash tk = let newSymbol  = Symbol tk category curScope t o
                        newSymList = newSymbol : Map.findWithDefault [] ((getString.getToken) tk) hash
                    in updateSymList hash tk newSymList

insertParam :: SymTable -> Scope -> SymCategory -> [SutParamTk] -> SymTable
insertParam table curScope category ps = insertWith table ps f
  where f hash (ref, t, tk) = let newSymbol  = Symbol tk category curScope t (IsReference ref)
                                  newSymList = newSymbol : Map.findWithDefault [] ((getString.getToken) tk) hash
                               in updateSymList hash tk newSymList

updateSymList :: SymMap -> SutToken -> [Symbol] -> SymMap
updateSymList hash tk newSymList = Map.insert ((getString.getToken) tk) newSymList hash


-- Lookup
---------------------------------------------------------------------------------------------------
lookupInScope :: SymTable -> Scope -> SutToken -> [Symbol]
lookupInScope table scope tk = filter isHere $ lookupId table $ getString $ getToken tk
  where isHere = (== scope) . getScope

lookupId :: SymTable -> SutID -> [Symbol]
lookupId table id = Map.findWithDefault [] id $ getHash table

lookupsInScope :: SymTable -> Scope -> [SutToken] -> [[Symbol]]
lookupsInScope table scope = map (lookupInScope table scope)
