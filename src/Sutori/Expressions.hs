module Sutori.Expressions where

import Sutori.AST
import Sutori.Monad

-- Literals
literalBool :: SutToken -> SutExpression
literalBool = error "literalBool"

literalChar :: SutToken -> SutExpression
literalChar = error "literalChar"

literalInt :: SutToken -> SutExpression
literalInt = error "literalChar"

literalFloat :: SutToken -> SutExpression
literalFloat = error "literalFloat"

literalString :: SutToken -> SutExpression
literalString = error "literalString"

-- Data structure constructors
constructedArray :: [SutExpression] -> SutMonad SutExpression
constructedArray = error "constructedArray"

constructedStruct :: [(SutID, SutExpression)] -> SutMonad SutExpression
constructedStrcut = error "constructedStruct"

-- Unary operations
unaryPlus :: SutExpression -> SutExpression
unaryPlus = error "unaryPlus"

unaryMinus :: SutExpression -> SutExpression
unaryMinus = error "unaryMinus"

negation :: SutExpression -> SutExpression
negation = error "unaryNot"

dereference :: SutExpression -> SutExpression
dereference = error "dereference"

-- Binary operations
opAddition :: SutExpression -> SutExpression -> SutExpression
opAddition = error "opAddition"

opSubstraction :: SutExpression -> SutExpression -> SutExpression
opSubstraction = error "opSubstraction"

opMultiplication :: SutExpression -> SutExpression -> SutExpression
opMultiplication = error "opMultiplication"

opDivision :: SutExpression -> SutExpression -> SutExpression
opDivision = error "opDivision"

opIntDivision :: SutExpression -> SutExpression -> SutExpression
opIntDivision = error "opIntDivision"

opPower :: SutExpression -> SutExpression -> SutExpression
opPower = error "opPower"

opModulo :: SutExpression -> SutExpression -> SutExpression
opModulo = error "opModulo"


opAnd :: SutExpression -> SutExpression -> SutExpression
opAnd = error "opAnd"

opOr :: SutExpression -> SutExpression -> SutExpression
opOr = error "opOr"

opEqual :: SutExpression -> SutExpression -> SutExpression
opEqual = error "opEqual"

opNotEqual :: SutExpression -> SutExpression -> SutExpression
opNotEqual = error "opNotEqual"

opGreaterEqual :: SutExpression -> SutExpression -> SutExpression
opGreaterEqual = error "opGreaterEqual"

opLessEqual :: SutExpression -> SutExpression -> SutExpression
opLessEqual = error "opLessEqual"

opGreater :: SutExpression -> SutExpression -> SutExpression
opGreater = error "opGreater"

opLess :: SutExpression -> SutExpression -> SutExpression
opLess = error "opLess"


-- Complex (probably monadic) operations
assignment :: SutExpression -> SutExpression -> SutMonad SutExpression
assignment = error "assignment"

arrayGet :: SutExpression -> SutExpression -> SutMonad SutExpression
arrayGet = error "arrayGet"

memberGet :: SutExpression -> SutID -> SutMonad SutExpression
memberGet = error "memberGet"

functionCall :: SutID -> [SutExpression] -> SutMonad SutExpression
functionCall = error "functionCall"
