module Sutori.Parser.Expressions where

import Data.List                 (find)
import Data.Maybe                (fromMaybe, isJust, fromJust)
import Control.Monad             (when)
import Control.Monad.State.Lazy  (get)

import Sutori.AST                (SutExpression(..), SutLiteral(..), SutOperator(..), expressionType)
import Sutori.Lexer.Tokens       (SutToken(tokenChar, tokenBool, tokenInt, tokenFloat, tokenString))
import Sutori.Monad              (SutMonad, SutState(SutState, typesGraph, typesNextID))
import Sutori.Types.Constructors (SutType(..), generalizeTypes, typeError)
import Sutori.Types.Graph        (lookupType)
import Sutori.Types.Primitives   (SutTypeID, SutPrimitive(..), primitiveID)
import Sutori.Utils              (SutID)
import Sutori.Parser.Symbols     (findType)
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

-- General constructor for binary operation with the given operator applying the given checks
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
-- It is known the left side is assignable
-- Right side must be of a more specific type than left side
assignment :: SutExpression -> SutExpression -> SutMonad SutExpression
assignment e1 e2 = do
  let t1 = expressionType e1
      gt = generalizeExprType e1 e2
      t  = if gt == t1 then gt else SutPrimitiveType SutTypeError
      -- TODO: Realize when the TypeError comes from below or was created here
  when (t == SutPrimitiveType SutTypeError) $ error "log type error here, moron"
  return $ BinaryOp t SutOpAssign e1 e2

-- It is known the left side is assignable and the right side is integer
-- Left side must be array type
arrayGet :: SutExpression -> SutExpression -> SutMonad SutExpression
arrayGet array index = case expressionType array of
    SutChain _ tid -> do
      SutState{typesGraph = tg} <- get
      let t = fromMaybe typeError (lookupType tid tg)
      return $ ArrayGet t array index
    _              -> do
      -- TODO: report type error: not and indexable type
      return $ ArrayGet typeError array index

-- It is known the left lise is assignable
-- Left side can be either a struct of a union type
-- Right side must be ID from left side's type
memberGet :: SutExpression -> SutID -> SutMonad SutExpression
memberGet struct id = case expressionType struct of
    SutMachine ms -> checkMember ms
    SutThing ms   -> checkMember ms
    _             -> do
      -- TODO: report type error: not and indexable type
      return $ MemberGet typeError struct id
  where
    checkMember :: [(SutID, SutTypeID)] -> SutMonad SutExpression
    checkMember ms = do
      let member = find (\(id',_) -> id == id') ms
          isPresent = isJust member
      -- TODO: Type Error when member missing
      when isPresent $ error "Report missing member"
      -- We check for the member's type (must exist)
      SutState{typesGraph = tg} <- get
      let tid = if isPresent then snd (fromJust member) else (-1)
          t   = fromMaybe typeError (lookupType tid tg)
      return $ MemberGet t struct id

-- Left side is known to be a person
-- Right side is known to be an existent type
createPointer :: SutID -> SutTypeID -> SutMonad SutExpression
createPointer pid tid = return $ CreatePointer (SutDirection tid) pid

-- It is know the expression is assignable
-- I must be a pointer/direction
dereference :: SutExpression -> SutMonad SutExpression
dereference e = do
  let t' = expressionType e
  case t' of
    SutDirection tid -> do
      SutState{typesGraph = tg} <- get
      let t = fromMaybe typeError (lookupType tid tg)
      return $ Dereference t e
    _ -> do
      -- TODO: report type error: not a direction type
      return $ Dereference typeError e

-- Left side is known to be existent function
-- Right side must be list of matching-type arguments
functionCall :: SutID -> [SutExpression] -> SutMonad SutExpression
functionCall = error "functionCall"
