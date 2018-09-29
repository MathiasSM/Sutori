module Sutori.Parser.Definitions where

import Sutori.Utils (SutID)
import Sutori.SymTable (SutParamKind)
import Sutori.AST (SutExpression, SutBlock, SutModule)
import Sutori.Monad (SutMonad)
import Sutori.Types (SutTypeID)

-- Declarations / Definitions
defPerson :: SutID -> SutMonad ()
defPerson = error "defPerson"

-- Define a function with ID pushed and no parameters
defFunction :: SutTypeID -> SutBlock -> SutMonad ()
defFunction = error "defFunction"

-- Define a function with ID and parameters pushed
defFunction' :: SutTypeID -> SutBlock -> SutMonad ()
defFunction' = error "defFunction'"

insertFunctionID :: SutID -> SutMonad SutID
insertFunctionID = error "insertFunctionID"

insertParam :: (SutParamKind, SutTypeID, SutID) -> SutMonad ()
insertParam (pt, tid, id) = error "insertParams"

defVariable :: SutID -> SutTypeID -> (SutID, Maybe SutExpression) -> SutMonad ()
defVariable = error "defVariable"

defType :: SutID -> SutID -> SutTypeID -> SutMonad ()
defType = error "detType"

defModule :: SutID -> SutBlock -> SutMonad ()
defModule = error "defModule"
