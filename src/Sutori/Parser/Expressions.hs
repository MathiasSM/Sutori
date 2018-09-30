module Sutori.Parser.Expressions where

import Sutori.AST                (SutExpression(ExprLiteral), SutLiteral(..))
import Sutori.Lexer.Tokens       (SutToken(tokenChar, tokenBool, tokenInt, tokenFloat, tokenString))
import Sutori.Monad              (SutMonad, SutState(SutState, typesGraph, typesNextID))
import Sutori.Types.Constructors (SutType(SutPrimitiveType))
import Sutori.Types.Primitives   (SutTypeID, SutPrimitive(..), primitiveID)
import Sutori.Utils              (SutID)


-- Literals
literalBool :: Bool -> SutExpression
literalBool   = ExprLiteral (SutPrimitiveType SutLight)  . SutBool
literalChar :: String -> SutExpression
literalChar   = ExprLiteral (SutPrimitiveType SutLetter) . SutChar
literalInt :: Int -> SutExpression
literalInt    = ExprLiteral (SutPrimitiveType SutBag)    . SutInt
literalFloat :: Float -> SutExpression
literalFloat  = ExprLiteral (SutPrimitiveType SutWallet) . SutFloat
literalString :: String -> SutExpression
literalString = ExprLiteral (SutPrimitiveType SutPhrase) . SutString


-- Data structure constructors
constructArray :: [SutExpression] -> SutMonad SutExpression
constructArray = error "constructedArray"

constructStruct :: [(SutID, SutExpression)] -> SutMonad SutExpression
constructStruct = error "constructedStruct"


-- Unary operations
unaryPlus :: SutExpression -> SutMonad SutExpression
unaryPlus = error "unaryPlus"

unaryMinus :: SutExpression -> SutMonad SutExpression
unaryMinus = error "unaryMinus"

unaryNot :: SutExpression -> SutMonad SutExpression
unaryNot = error "unaryNot"

dereference :: SutExpression -> SutMonad SutExpression
dereference = error "dereference"


-- Binary operations
opAddition :: SutExpression -> SutExpression -> SutMonad SutExpression
opAddition = error "opAddition"

opSubstraction :: SutExpression -> SutExpression -> SutMonad SutExpression
opSubstraction = error "opSubstraction"

opMultiplication :: SutExpression -> SutExpression -> SutMonad SutExpression
opMultiplication = error "opMultiplication"

opDivision :: SutExpression -> SutExpression -> SutMonad SutExpression
opDivision = error "opDivision"

opIntDivision :: SutExpression -> SutExpression -> SutMonad SutExpression
opIntDivision = error "opIntDivision"

opPower :: SutExpression -> SutExpression -> SutMonad SutExpression
opPower = error "opPower"

opModulo :: SutExpression -> SutExpression -> SutMonad SutExpression
opModulo = error "opModulo"

opAnd :: SutExpression -> SutExpression -> SutMonad SutExpression
opAnd = error "opAnd"

opOr :: SutExpression -> SutExpression -> SutMonad SutExpression
opOr = error "opOr"

opEqual :: SutExpression -> SutExpression -> SutMonad SutExpression
opEqual = error "opEqual"

opNotEqual :: SutExpression -> SutExpression -> SutMonad SutExpression
opNotEqual = error "opNotEqual"

opGreaterEqual :: SutExpression -> SutExpression -> SutMonad SutExpression
opGreaterEqual = error "opGreaterEqual"

opLessEqual :: SutExpression -> SutExpression -> SutMonad SutExpression
opLessEqual = error "opLessEqual"

opGreater :: SutExpression -> SutExpression -> SutMonad SutExpression
opGreater = error "opGreater"

opLess :: SutExpression -> SutExpression -> SutMonad SutExpression
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

createPointer :: SutID -> SutTypeID -> SutMonad SutExpression
createPointer = error "createPointer"
