module Sutori.Expressions where

import Sutori.AST
import Sutori.Monad

literalBool   :: SutToken -> SutExpression
literalChar   :: SutToken -> SutExpression
literalInt    :: SutToken -> SutExpression
literalFloat  :: SutToken -> SutExpression
literalString :: SutToken -> SutExpression
literalArray  :: [SutExpression] -> SutExpression
literalStruct :: [(SutID, SutExpression)] -> SutExpression

unaryPlus  :: SutExpression -> SutExpression
unaryMinus :: SutExpression -> SutExpression
unaryNot   :: SutExpression -> SutExpression

opAddition       :: SutExpression -> SutExpression -> SutExpression
opSubstraction   :: SutExpression -> SutExpression -> SutExpression
opMultiplication :: SutExpression -> SutExpression -> SutExpression
opDivision       :: SutExpression -> SutExpression -> SutExpression
opIntDivision    :: SutExpression -> SutExpression -> SutExpression
opPower          :: SutExpression -> SutExpression -> SutExpression
opModulo         :: SutExpression -> SutExpression -> SutExpression

opAnd          :: SutExpression -> SutExpression -> SutExpression
opOr           :: SutExpression -> SutExpression -> SutExpression
opEqual        :: SutExpression -> SutExpression -> SutExpression
opNotEqual     :: SutExpression -> SutExpression -> SutExpression
opGreaterEqual :: SutExpression -> SutExpression -> SutExpression
opLessEqual    :: SutExpression -> SutExpression -> SutExpression
opGreater      :: SutExpression -> SutExpression -> SutExpression
opLess         :: SutExpression -> SutExpression -> SutExpression

assignment  :: SutExpression -> SutExpression -> SutExpression
dereference :: SutExpression -> SutExpression
arrayGet    :: SutExpression -> SutExpression -> SutExpression
memberGet   :: SutExpression -> SutID -> SutExpression

functionCall :: SutID -> [SutExpression] -> SutExpression
