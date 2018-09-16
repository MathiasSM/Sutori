module Sutori.SymTable where

import qualified Data.Map as Map

import Sutori.Logger(SutShow(showSut), SutLog(SutLogNode, SutLogLeave))
import Sutori.AST(SutID, SutBlock)
import Sutori.Types



-- Data definition
---------------------------------------------------------------------------------------------------

-- A Scope is just a number uniquely representing a scope (NOT a level)
type Scope = Int


-- A SutSymCategory. Can be shown by showSut
data SutSymCategory = CatModule
                    | CatFunction
                    | CatPerson
                    | CatVariable
                    | CatParameter
                    | CatType

instance SutShow SutSymCategory where
  showSut CatModule    = SutLogLeave "Category: Module"
  showSut CatFunction  = SutLogLeave "Category: Function"
  showSut CatPerson    = SutLogLeave "Category: Person"
  showSut CatVariable  = SutLogLeave "Category: Variable"
  showSut CatParameter = SutLogLeave "Category: Parameter"
  showSut CatType      = SutLogLeave "Category: Type"

-- A SutParam can be either a sutori reference or a value
data SutParamKind = SutRef | SutVal

instance SutShow SutParamKind where
  showSut SutRef = SutLogLeave "Passed by: Reference"
  showSut SutVal = SutLogLeave "Passed by: Value"


-- A SutSymOther is either a Function definition AST, the parameter kind, the type definition or nothing
data SutSymOther = SymAST {getParams :: [SutParam], getBlock :: SutBlock }
                 | SymParamKind {getParamKind :: SutParamKind}
                 | SymTypeDef {typeOf :: SutType}
                 | SymNothing

instance SutShow SutSymOther where
  showSut (SymAST ps b)    = let params = SutLogNode "Parameters: " (map showSut ps)
                                 block  = SutLogNode "AST: " (map showSut b)
                              in SutLogNode "Function definition:" [params, block]
  showSut (SymParamKind k) = showSut k
  showSut (SymTypeDef t)   = SutLogNode  "Type definition: " [showSut t]
  showSut  SymNothing      = SutLogLeave "-"


-- A SutSymbol is made of a Token, a Category, a Scope, a Type, and perhaps other information
data SutSymbol = SutSymbol {
  getSymID    :: SutID,
  getCat      :: SutSymCategory,
  getScope    :: Scope,
  getType     :: SutType,
  getOther    :: SutSymOther
}

instance SutShow SutSymbol where
  showSut s = let etype = SutLogNode "Type:" [showSut (getType s)]
                  cat = showSut $ getCat s
                  scope = SutLogLeave ("Scope: " ++ show (getScope s))
                  other = showSut (getOther s)
               in SutLogNode ("Symbol: " ++ getSymID s) [cat, etype, scope, other]


-- A SutParam has a kind, a type and a token representing
data SutParam = SutParam {
  getKind      :: SutParamKind,
  getParamType :: SutType,
  getParamID   :: SutID
}

instance SutShow SutParam where
  showSut p = let kind = showSut (getKind p)
                  etype = SutLogNode "Type:" [showSut (getParamType p)]
               in SutLogNode ("Parameter: " ++ getParamID p) [etype, kind]


-- A SymTable matches a SutID (symbol name) to a list of symbols
type SymTable = Map.Map SutID [SutSymbol]


--
-- -- Actions on the SymTable
-- -- ===============================================================================================
--
-- -- Insertion
-- -- ------------------------------------------------------------------------------------------------
-- -- Inserts? a list of IDs witha given function
-- insertWith :: SymTable -> [a] -> (SymMap -> a -> SymMap) -> SymTable
-- insertWith table ids f = SymTable $ foldl f (getHash table) ids
--
-- -- Inserts into the table the given constructed symbol
-- insertSymbol :: SymMap -> SutSymbol -> SymMap
-- insertSymbol hash s = let id = getSymbolID s
--                           syms = Map.findWithDefault [] id hash
--                        in Map.insert id (s:syms) hash
--
-- -- Inserts new symbols into the table (from a list of tokens)
-- insert :: SymTable -> Scope -> SutSymCategory -> SutType -> SutSymOther -> [SutToken] -> SymTable
-- insert table curScope category t o ids = insertWith table ids f
--   where f hash tk = let newSymbol = SutSymbol tk category curScope t o
--                      in insertSymbol hash newSymbol
--
-- -- Inserts new symbols into the table (from a list of params)
-- insertParams :: SymTable -> Scope -> SutSymCategory -> [SutParam] -> SymTable
-- insertParams table curScope category ps = insertWith table ps f
--   where f hash (SutParam k t tk) = let newSymbol = SutSymbol tk category curScope t (ParamKind k)
--                                     in insertSymbol hash newSymbol
--
--
-- -- Lookup
-- ---------------------------------------------------------------------------------------------------
-- -- Search for all symbols that match a SutID
-- lookupID :: SymTable -> SutID -> [SutSymbol]
-- lookupID table id = Map.findWithDefault [] id $ getHash table
--
-- -- Search for all symbols that match a token in a given scope
-- lookupInScope :: SymTable -> Scope -> SutToken -> [SutSymbol]
-- lookupInScope table scope tk = filter isHere $ lookupID table $ getTokenID tk
--   where isHere = (== scope) . getScope
--
-- -- Makes several token searches in the same scope
-- lookupsInScope :: SymTable -> Scope -> [SutToken] -> [[SutSymbol]]
-- lookupsInScope table scope = map (lookupInScope table scope)
