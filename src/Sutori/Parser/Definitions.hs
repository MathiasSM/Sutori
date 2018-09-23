module Sutori.Parser.Definitions where

import Sutori.AST
import Sutori.Monad

-- Declarations / Definitions
defPerson :: SutID -> SutMonad ()
defPerson = error "defPerson"

-- Define a function with ID pushed and no parameters
defFunction :: SutType -> SutBlock -> SutMonad ()
defFunction = error "defFunction"

-- Define a function with ID and parameters pushed
defFunction' :: SutID -> SutBlock -> SutMonad ()
defFunction' = error "defFunction'"

insertFunctionID :: SutID -> SutMonad ()
insertFunctionID = error "insertFunctionID"

defVariable :: SutID -> SutType -> [(SutID, Maybe SutExpression)] -> SutMonad ()
defVariable = error "defVariable"

defType :: SutID -> SutID -> SutType -> SutMonad ()
defType = error "detType"

defModule :: SutID -> SutBlock -> SutMonad ()
defModule = error "defModule"
