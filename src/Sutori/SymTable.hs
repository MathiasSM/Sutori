module Sutori.SymTable where

import qualified Data.Map as Map

import Sutori.Logger(SutShow(showSut), SutLog(SutLogNode, SutLogLeave))
import Sutori.AST(SutID, SutBlock)
import Sutori.Types(SutType)



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
data SutSymOther = SymAST       { otherASTParams :: [SutParam], otherASTBlock :: SutBlock }
                 | SymParamKind { otherParamKind :: SutParamKind }
                 | SymTypeDef   { otherTypeDef   :: SutType }
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
  symID    :: SutID,
  symCat   :: SutSymCategory,
  symScope :: Scope,
  symType  :: SutType,
  symOther :: SutSymOther
}

instance SutShow SutSymbol where
  showSut s = let etype = SutLogNode "Type:" [showSut (symType s)]
                  cat   = showSut $ symCat s
                  scope = SutLogLeave ("Scope: " ++ show (symScope s))
                  other = showSut (symOther s)
               in SutLogNode ("Symbol: " ++ symID s) [cat, etype, scope, other]


-- A SutParam has a kind, a type and a token representing
data SutParam = SutParam {
  paramKind :: SutParamKind,
  paramType :: SutType,
  paramID   :: SutID
}

instance SutShow SutParam where
  showSut p = let kind  = showSut (paramKind p)
                  etype = SutLogNode "Type:" [showSut (paramType p)]
               in SutLogNode ("Parameter: " ++ paramID p) [etype, kind]


-- A SymTable matches a SutID (symbol name) to a list of symbols
type SymTable = Map.Map SutID [SutSymbol]



-- Actions on the SymTable
-- ===============================================================================================

-- Insertion
-- ------------------------------------------------------------------------------------------------
-- Folds a list of IDs with a given function over the table
insertWith :: SymTable -> [a] -> (SymTable -> a -> SymTable) -> SymTable
insertWith table ids f = foldl f table ids

-- Inserts symbol into the table the given constructed symbol
insertSymbol :: SymTable -> SutSymbol -> SymTable
insertSymbol table s = let id   = symID s
                           syms = lookupID table id
                        in Map.insert id (s:syms) table

-- Inserts new symbols into the table (from a list of IDs)
insert :: SymTable -> Scope -> SutSymCategory -> SutType -> SutSymOther -> [SutID] -> SymTable
insert table scope cat t o ids = insertWith table ids insertID
  where insertID table id = let newSymbol = SutSymbol { symID    = id,
                                                        symCat   = cat,
                                                        symScope = scope,
                                                        symType  = t,
                                                        symOther = o }
                             in insertSymbol table newSymbol

-- Inserts new symbols into the table (from a list of params)
insertParams :: SymTable -> Scope -> SutSymCategory -> [SutParam] -> SymTable
insertParams table scope cat ps = insertWith table ps insertParam
  where insertParam table' p = let newSymbol = SutSymbol { symID    = paramID p,
                                                           symCat   = cat, -- TODO: Isn't this always CatParameter?
                                                           symScope = scope,
                                                           symType  = paramType p,
                                                           symOther = SymParamKind $ paramKind p }
                                in insertSymbol table' newSymbol


-- Lookup
---------------------------------------------------------------------------------------------------
-- Search for all symbols that match a SutID
lookupID :: SymTable -> SutID -> [SutSymbol]
lookupID table id = Map.findWithDefault [] id table

-- Search for all symbols that match a token in a given scope
lookupInScope :: SymTable -> Scope -> SutID -> [SutSymbol]
lookupInScope table scope id = filter isHere $ lookupID table id
  where isHere = (== scope) . symScope

-- Makes several token searches in the same scope
lookupsInScope :: SymTable -> Scope -> [SutID] -> [[SutSymbol]]
lookupsInScope table scope = map (lookupInScope table scope)
