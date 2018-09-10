module Sutori.SymTable where

import qualified Data.Map as Map
import qualified Data.List as List

import Sutori.Utils
import Sutori.AST
import Sutori.Lexer
import Sutori.Types



-- Data definition
---------------------------------------------------------------------------------------------------

-- Symbol table: A single SymMap
newtype SymTable = SymTable {
  getHash :: SymMap
} deriving (Eq, Show)

-- A SymMap matches a SutID (symbol name) to a list of symbols
type SymMap = Map.Map SutID [SutSymbol]



-- A SutSymbol is made of a Token, a Category, a Scope, a Type, and perhaps other information
data SutSymbol = SutSymbol {
  getSymToken :: SutToken,
  getCategory :: SutSymCategory,
  getScope    :: Scope,
  getType     :: SutType,
  getOther    :: SutSymOther
} deriving (Show, Eq)
instance SutShow SutSymbol where
  showSut s@(SutSymbol tk cat sc t other) =
    "SutSymbol "++getSymbolID s++" ("
    ++"T: "++showSut t++", "
    ++"Cat: "++showSut cat++", "
    ++"Scope: "++show sc++", "
    ++"Posn: "++showSut (getPosn tk)
    ++")"


-- A SutSymCategory. Can be shown by showSut
data SutSymCategory = CatModule
                    | CatFunction
                    | CatPerson
                    | CatVariable
                    | CatParameter
                    | CatType
                    deriving (Eq,Show)
instance SutShow SutSymCategory where
  showSut CatModule    = "Module"
  showSut CatFunction  = "Function"
  showSut CatPerson    = "Person"
  showSut CatVariable  = "Variable"
  showSut CatParameter = "Parameter"
  showSut CatType      = "Type"


-- A SutSymOther is either a Function definition AST, the parameter kind, the type definition or nothing
data SutSymOther = FunctionAST {getParams :: [SutParam], getBlock :: SutBlock }
                 | ParamKind {getParamKind :: SutParamKind}
                 | TypeDef {typeOf :: SutType}
                 | NoOther
                 deriving (Eq,Show)
instance SutShow SutSymOther where
  showSut (FunctionAST ps b) = List.intercalate ", " (map showSut ps) ++ "\n" ++ List.intercalate ", " (map showSut b)
  showSut (ParamKind SutRef) = "reference"
  showSut (ParamKind SutVal) = "value"
  showSut (TypeDef t)        = showSut t
  showSut  NoOther           = ""


-- A SutParam can be either a sutori reference or a value
data SutParamKind = SutRef | SutVal deriving (Show, Eq)
instance SutShow SutParamKind where
  showSut SutRef = "passed by reference"
  showSut SutVal = "passed by value"


-- A SutParam has a kind, a type and a token representing
data SutParam = SutParam {
  getKind :: SutParamKind,
  getParamType :: SutType,
  getParamToken :: SutToken
} deriving (Show, Eq)
instance SutShow SutParam where
  showSut (SutParam k t tk) = "Parameter "++showSut k++" (T: "++showSut t++") (TK: "++showSut tk++")"


-- A Scope is just a number uniquely representing a scope (NOT a level)
type Scope = Int


-- Helper to get the SutID that represents a particular symbol
getSymbolID :: SutSymbol -> SutID
getSymbolID = getTokenID . getSymToken

-- Helper to get the SutID that comes in a token
getTokenID :: SutToken -> String
getTokenID = getString . getToken



-- Actions on the SymTable
-- ===============================================================================================

-- Insertion
-- ------------------------------------------------------------------------------------------------
-- Inserts? a list of IDs witha given function
insertWith :: SymTable -> [a] -> (SymMap -> a -> SymMap) -> SymTable
insertWith table ids f = SymTable $ foldl f (getHash table) ids

-- Inserts into the table the given constructed symbol
insertSymbol :: SymMap -> SutSymbol -> SymMap
insertSymbol hash s =
  let id   = getSymbolID s
      syms = Map.findWithDefault [] id hash
  in  Map.insert id (s : syms) hash

-- Inserts new symbols into the table (from a list of tokens)
insert
  :: SymTable
  -> Scope
  -> SutSymCategory
  -> SutType
  -> SutSymOther
  -> [SutToken]
  -> SymTable
insert table curScope category t o ids = insertWith table ids f
 where
  f hash tk =
    let newSymbol = SutSymbol tk category curScope t o
    in  insertSymbol hash newSymbol

-- Inserts new symbols into the table (from a list of params)
insertParams :: SymTable -> Scope -> SutSymCategory -> [SutParam] -> SymTable
insertParams table curScope category ps = insertWith table ps f
 where
  f hash (SutParam k t tk) =
    let newSymbol = SutSymbol tk category curScope t (ParamKind k)
    in  insertSymbol hash newSymbol


-- Lookup
---------------------------------------------------------------------------------------------------
-- Search for all symbols that match a SutID
lookupID :: SymTable -> SutID -> [SutSymbol]
lookupID table id = Map.findWithDefault [] id $ getHash table

-- Search for all symbols that match a token in a given scope
lookupInScope :: SymTable -> Scope -> SutToken -> [SutSymbol]
lookupInScope table scope tk = filter isHere $ lookupID table $ getTokenID tk
  where isHere = (== scope) . getScope

-- Makes several token searches in the same scope
lookupsInScope :: SymTable -> Scope -> [SutToken] -> [[SutSymbol]]
lookupsInScope table scope = map (lookupInScope table scope)
