module Sutori.Parser.Expressions where

import Sutori.AST                (SutExpression(..), SutLiteral(..), SutOperator(..), expressionType)
import Sutori.Lexer.Tokens       (SutToken(tokenChar, tokenBool, tokenInt, tokenFloat, tokenString))
import Sutori.Monad              (SutMonad, SutState(SutState, typesGraph, typesNextID))
import Sutori.Types.Constructors (SutType(SutPrimitiveType), generalizeTypes)
import Sutori.Types.Primitives   (SutTypeID, SutPrimitive(..), primitiveID)
import Sutori.Utils              (SutID)
import Sutori.Parser.TypeCheck   (checkNumeric, checkBoolean, checkSortable, checkEq, checkInt, checkFloat)


-- Literals
-- ================================================================================================
literalBool :: Bool -> SutExpression
literalBool = ExprLiteral (SutPrimitiveType SutLight)  . SutBool

literalChar :: String -> SutExpression
literalChar = ExprLiteral (SutPrimitiveType SutLetter) . SutChar

literalInt :: Int -> SutExpression
literalInt = ExprLiteral (SutPrimitiveType SutBag)    . SutInt

literalFloat :: Float -> SutExpression
literalFloat = ExprLiteral (SutPrimitiveType SutWallet) . SutFloat

literalString :: String -> SutExpression
literalString = ExprLiteral (SutPrimitiveType SutPhrase) . SutString



-- Data structure constructors
-- ================================================================================================
constructArray :: [SutExpression] -> SutMonad SutExpression
constructArray = error "constructedArray"

constructStruct :: [(SutID, SutExpression)] -> SutMonad SutExpression
constructStruct = error "constructedStruct"



-- Unary operations
-- ================================================================================================
unaryPlus :: SutExpression -> SutMonad SutExpression
unaryPlus e' = let e = checkNumeric e' in return $ UnaryOp (expressionType e) SutOpPos e

unaryMinus :: SutExpression -> SutMonad SutExpression
unaryMinus e' = let e = checkNumeric e' in return $ UnaryOp (expressionType e) SutOpNeg e

unaryNot :: SutExpression -> SutMonad SutExpression
unaryNot e' = let e = checkBoolean e' in return $ UnaryOp (expressionType e) SutOpNot e

dereference :: SutExpression -> SutMonad SutExpression
dereference = error "dereference"



-- Binary operations
-- ================================================================================================

-- Helpers
-- ------------------------------------------------------------------------------------------------
type ExprTransform = SutExpression -> SutExpression

-- Gives, from two expressions, the most general type to which the two can be casted
generalizeExprType :: SutExpression -> SutExpression -> SutType
generalizeExprType e1 e2 = let t1 = expressionType e1
                               t2 = expressionType e2
                            in generalizeTypes t1 t2

-- Constructs an binary operation with the given operator applying the given checks
binaryOp :: ExprTransform -> ExprTransform -> ExprTransform -- Transforms for first, second and res
         -> SutOperator -> SutExpression -> SutExpression   -- Operator and operands
         -> SutMonad SutExpression                          -- Result
binaryOp f1 f2 finalCheck op e1' e2' = let (e1, e2) = (f1 e1', f2 e2')
                                           result = BinaryOp (generalizeExprType e1 e2) op e1 e2
                                        in return $ finalCheck result

-- Binary operation specific constructors
numericBinaryOp, booleanBinaryOp, sortBinaryOp, eqBinaryOp
  :: SutOperator -> SutExpression -> SutExpression -> SutMonad SutExpression
numericBinaryOp = binaryOp checkNumeric  checkNumeric  checkNumeric
booleanBinaryOp = binaryOp checkBoolean  checkBoolean  checkBoolean
sortBinaryOp    = binaryOp checkSortable checkSortable checkBoolean
eqBinaryOp      = binaryOp checkEq       checkEq       checkBoolean


-- Numerical operations
-- ------------------------------------------------------------------------------------------------
opAddition, opSubstraction, opMultiplication :: SutExpression -> SutExpression -> SutMonad SutExpression
opAddition       = numericBinaryOp SutOpAdd
opSubstraction   = numericBinaryOp SutOpSub
opMultiplication = numericBinaryOp SutOpMul

opDivision, opIntDivision, opModulo :: SutExpression -> SutExpression -> SutMonad SutExpression
opDivision    = binaryOp checkNumeric checkNumeric checkNumeric SutOpDiv
opIntDivision = binaryOp checkNumeric checkNumeric checkInt     SutOpIntDiv
opModulo      = binaryOp checkInt     checkInt     checkFloat   SutOpMod

opPower :: SutExpression -> SutExpression -> SutMonad SutExpression
opPower = numericBinaryOp SutOpPow


-- Boolean operations (all return booleans)
-- ------------------------------------------------------------------------------------------------
opAnd, opOr :: SutExpression -> SutExpression -> SutMonad SutExpression
opAnd = booleanBinaryOp SutOpAnd -- Boolean AND receives two booleans
opOr  = booleanBinaryOp SutOpOr  -- Boolean OR receives two boolean


opEqual, opNotEqual :: SutExpression -> SutExpression -> SutMonad SutExpression
opEqual    = eqBinaryOp SutOpEqual -- Equality check receives two "equalable" types
opNotEqual = eqBinaryOp SutOpNotEq -- NotEquality check receives two "equalable" types


opGreaterEqual, opLessEqual, opLess, opGreater :: SutExpression -> SutExpression -> SutMonad SutExpression
opGreaterEqual = sortBinaryOp SutOpGEq     -- (>=) receives two "sortable" type
opLessEqual    = sortBinaryOp SutOpLEq     -- (<=) receives two "sortable" type
opGreater      = sortBinaryOp SutOpGreater -- (>) receives two "sortable" type
opLess         = sortBinaryOp SutOpLess    -- (<) receives two "sortable" type


-- Complex operations
-- ================================================================================================
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
