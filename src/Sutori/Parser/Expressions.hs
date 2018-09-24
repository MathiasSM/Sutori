module Sutori.Parser.Expressions where

import Sutori.AST (SutExpression(ExprLiteral), SutLiteral(..))
import Sutori.Monad (SutMonad, SutState(SutState, typesGraph, typesNextID))
import Sutori.Lexer.Tokens (SutToken(tokenChar, tokenBool, tokenInt, tokenFloat, tokenString))
import Sutori.Utils (SutID)
import Sutori.Types.Constructors (SutType(SutPrimitiveType))
import Sutori.Types.Primitives (SutPrimitive(..), primitiveID)

-- Literals
literalBool, literalChar, literalInt, literalFloat, literalString :: SutToken -> SutExpression
literalBool   = ExprLiteral (SutPrimitiveType SutLight)  . SutBool   . tokenBool
literalChar   = ExprLiteral (SutPrimitiveType SutLetter) . SutChar   . tokenChar
literalInt    = ExprLiteral (SutPrimitiveType SutBag)    . SutInt    . tokenInt
literalFloat  = ExprLiteral (SutPrimitiveType SutWallet) . SutFloat  . tokenFloat
literalString = ExprLiteral (SutPrimitiveType SutPhrase) . SutString . tokenString


-- Data structure constructors
constructArray :: [SutExpression] -> SutMonad SutExpression
constructArray = error "constructedArray"

constructStruct :: [(SutID, SutExpression)] -> SutMonad SutExpression
constructStruct = error "constructedStruct"


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
