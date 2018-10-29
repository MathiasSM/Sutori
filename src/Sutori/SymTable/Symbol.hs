{-|
Description : Defines symbols as understood by Sutori language.
-}
module Sutori.SymTable.Symbol
( SutSymbol(..)
, ParametricSymbol(..)
, TypedSymbol(..)
, ASTSymbol(..)

, Scope
, SutParam(..)

, SymModule(..)
, SymFunction(..)
, SymPerson(..)
, SymVariable(..)
, SymType(..)

, SutSymbol'(..)
, SymbolCat(..)
, isModule
, isFunction
, isPerson
, isType
, isVariable

, paramByValue
, paramByRef

, unBoxFunction
, unBoxPerson
, unBoxVariable
, unBoxModule
, unBoxType
) where

import Data.Maybe   (fromJust)

import Sutori.AST   (SutID, SutAST)
import Sutori.Types (SutTypeID)


-- |A 'Scope' is just a number uniquely representing a scope (NOT a level, but an ID)
type Scope = Int

data SymbolCat = CatModule | CatPerson | CatType | CatVariable | CatFunction | CatMember

isModule, isFunction, isPerson, isType, isVariable :: SymbolCat -> Bool
isModule CatModule = True
isModule _         = False
isPerson CatPerson = True
isPerson _         = False
isType CatType = True
isType _       = False
isVariable CatVariable = True
isVariable _           = False
isFunction CatFunction = True
isFunction _           = False


-- | Represents a basic symbol
class SutSymbol a where
  symID :: a -> SutID      -- ^ The symbol ID
  symCat :: a -> SymbolCat -- ^ The symbol category
  symScope :: a -> Scope   -- ^ The scope the symbol is defined. Zero by default
  symScope a = 0

-- | Represents a symbol which also has some parameters
class SutSymbol a => ParametricSymbol a where
  symParams :: a -> [SutParam] -- ^ The list of parameters (in order)

-- | Represents a symbol with a type
class SutSymbol a => TypedSymbol a where
  symType :: a -> SutTypeID -- ^ The type of the symbol

-- | Represents a symbol that holds an AST
class SutSymbol a => ASTSymbol a where
  symAST    :: a -> SutAST        -- ^ The AST held by the symbol
  symPreAST :: a -> SutAST        -- ^ An optional (empty by default) AST (hidden, preppended)
  symPreAST _ = []


-- |A 'SutParam' represents a function parameter, and
-- it has a kind, type and ID
data SutParam = SutParam {
  isRef     :: Bool,      -- ^ Whether it is a reference or a value parameter
  paramType :: SutTypeID, -- ^ The type of the parameter
  paramID   :: SutID      -- ^ The ID of the parameter
} deriving Eq

-- | Helper to abstract the True/False isRef field
paramByValue, paramByRef :: SutTypeID -> SutID -> SutParam
paramByValue = SutParam False
paramByRef   = SutParam True



-- | A (top-level) module
data SymModule = SymModule SutID SutAST

instance SutSymbol SymModule where
  symID (SymModule id _) = id
  symCat _ = CatModule

instance ASTSymbol SymModule where
  symAST (SymModule _ ast) = ast


-- | A (top-level) function
data SymFunction = SymFunction SutID SutTypeID [SutParam] SutAST (Maybe SutAST)

instance SutSymbol SymFunction where
  symID (SymFunction id _ _ _ _) = id
  symCat _ = CatFunction

instance TypedSymbol SymFunction where
  symType (SymFunction _ tid _ _ _) = tid

instance ParametricSymbol SymFunction where
  symParams (SymFunction _ _ ps _ _) = ps

instance ASTSymbol SymFunction where
  symAST (SymFunction _ _ _ _ ast) = fromJust ast
  symPreAST (SymFunction _ _ _ ast _) = ast


-- | A person who may do things in the story
data SymPerson = SymPerson SutID Scope

instance SutSymbol SymPerson where
  symID (SymPerson id _)   = id
  symScope (SymPerson _ s) = s
  symCat _ = CatPerson


-- | A variable
data SymVariable = SymVariable SutID Scope SutTypeID

instance SutSymbol SymVariable where
  symID (SymVariable id _ _)   = id
  symScope (SymVariable _ s _) = s
  symCat _ = CatVariable

instance TypedSymbol SymVariable where
  symType (SymVariable _ _ tid) = tid


-- | A reference to a (now named) type
data SymType = SymType SutID Scope SutTypeID

instance SutSymbol SymType where
  symID (SymType id _ _)   = id
  symScope (SymType _ s _) = s
  symCat _ = CatType

instance TypedSymbol SymType where
  symType (SymType _ _ tid) = tid


-- | Boxed SutSymbols for the table to handle
data SutSymbol' = SymModule' SymModule
                | SymFunction' SymFunction
                | SymPerson' SymPerson
                | SymVariable' SymVariable
                | SymType' SymType

unBoxFunction (SymFunction' s) = s
unBoxModule   (SymModule' s)   = s
unBoxPerson   (SymPerson' s)   = s
unBoxType     (SymType' s)     = s
unBoxVariable (SymVariable' s) = s

instance SutSymbol SutSymbol' where
  symCat (SymFunction' s)   = symCat s
  symCat (SymModule' s)     = symCat s
  symCat (SymPerson' s)     = symCat s
  symCat (SymType' s)       = symCat s
  symCat (SymVariable' s)   = symCat s

  symID (SymFunction' s)    = symID s
  symID (SymModule' s)      = symID s
  symID (SymPerson' s)      = symID s
  symID (SymType' s)        = symID s
  symID (SymVariable' s)    = symID s

  symScope (SymFunction' s) = symScope s
  symScope (SymModule' s)   = symScope s
  symScope (SymPerson' s)   = symScope s
  symScope (SymType' s)     = symScope s
  symScope (SymVariable' s) = symScope s
