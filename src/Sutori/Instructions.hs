module Sutori.Instructions where

import Sutori.AST
import Sutori.Monad

-- Instructions
instrExpression  :: SutExpression -> SutParserM
instrFreePointer :: SutID -> SutExpression -> SutParserM
instrPrint       :: SutID -> SutExpression -> SutParserM
instrRead        :: SutID -> SutExpression -> SurParserM
instrSelect      :: SutID -> SutExpression -> SutBlock -> SutBlock -> SutParserM
instrIterateU    :: SutID -> SutExpression -> SutBlock -> SutParserM
instrIterateB    :: SutID -> SutExpression -> SutBlock -> SutParserM
instrReturn      :: SutExpression -> SutParserM
