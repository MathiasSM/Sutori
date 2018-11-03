{-|
Description : Defines all AST expression monadic constructors,
              that check for all kind of possible errors to log.
-}
module Sutori.Parser.Expressions
( -- * Type aliases
  -- Way too long signature for way too long explanations of way too long descriptions
  -- for way too long type aliases for way too long signatures.
  ExprTransform, SutUnaryOp, SutBinaryOp

  -- * Expressions
  -- Or more accurately: Expression constructors. These are monadic actions that
  -- ultimately construct the "AST" using the defined data constructors, but whoe job
  -- is to also check for type errors and other inconsistencies that need to be addressed.
  --
  -- ** Literals
  -- Including both expression literals for primitive types, and literal constructs of
  -- complex data structures (the latter might include non-literal sub-expressions).
, literalBool, literalChar, literalInt, literalFloat, literalString
, constructArray, constructStruct

  -- ** Simple Operations
  -- The usual arithmetic, boolean and otherwise obvious operations, except when otherwise noted.
  --
  -- *** Unary operations
, unaryPlus, unaryMinus, unaryNot
  -- *** Numerical binary operations
, opAddition, opSubstraction, opMultiplication, opDivision, opIntDivision, opModulo, opPower
  -- *** Sorting/Comparison operations
, opGreaterEqual, opLessEqual, opLess, opGreater
  -- *** Boolean binary operations
, opEqual, opNotEqual, opAnd, opOr

  -- ** Complex operations
, assignment, arrayGet, memberGet, functionCall, dereference, createPointer
) where

import Control.Arrow             (second)
import Control.Monad             (when, unless)
import Control.Monad.State.Lazy  (get)
import Data.List                 (find)
import Data.Maybe                (isJust, fromJust)

import Sutori.AST       (SutID, SutExpression(..), SutLiteral(..), SutOperator(..), SutConstructor(..), expressionType)
import Sutori.TAC       (addTAC, TAC(..), TACAddress(..), TACType(..))
import Sutori.Monad     (SutMonad, SutState(SutState, parserTable))
import Sutori.Error     (typeError, argumentsNumberError, undefinedError, duplicateMemberError)
import Sutori.Types     (SutType(..), generalizeTypes, primitiveError, SutTypeID, SutPrimitive(..))
import Sutori.Utils     (repeated)
import Sutori.SymTable
  ( SymbolCat(..), SutParam(..)
  , TypedSymbol(..), ParametricSymbol(..))

import Sutori.Parser.Symbols
import Sutori.Parser.TypeCheck

-- |Represents a transformation from an expression to another
--
-- Ex. Change of type to TypeError
type ExprTransform = SutExpression -> SutExpression

-- |Represents a unary operation expression constructor
type SutUnaryOp  = SutExpression -> SutMonad SutExpression

-- |Represents a binary operation expression constructor
type SutBinaryOp = SutExpression -> SutExpression -> SutMonad SutExpression


-- Literals
-- ================================================================================================

-- |Generates AST expression for the given literal
exprLiteral :: SutPrimitive -> SutLiteral -> SutMonad SutExpression
exprLiteral t l = do
  addCodeLiteral l
  return $ ExprLiteral (SutPrimitiveType t) l

-- |Generates code for the given literal
addCodeLiteral :: SutLiteral -> SutMonad TACAddress
addCodeLiteral l = addTAC $ TAC Copy (Just (TACLit l)) Nothing

literalBool :: Bool -> SutMonad SutExpression
literalBool v = do
  let l    = SutBool v
      t    = SutLight
  exprLiteral t l

literalChar :: String -> SutMonad SutExpression
literalChar v = do
  let l    = SutChar v
      t    = SutLetter
  exprLiteral t l

literalInt :: Int -> SutMonad SutExpression
literalInt v = do
  let l    = SutInt v
      t    = SutBag
  exprLiteral t l

literalFloat :: Float -> SutMonad SutExpression
literalFloat v = do
  let l    = SutFloat v
      t    = SutWallet
  exprLiteral t l

literalString :: String -> SutMonad SutExpression
literalString v = do
  let l    = SutString v
      t    = SutPhrase
  exprLiteral t l



-- Data structure constructors
-- ================================================================================================

-- |Constructs an array from the list of expressions
-- All expressions must be of the same type
--
-- Note: Sutori grammar doesn't allow empty arrays
constructArray :: [SutExpression] -> SutMonad SutExpression
constructArray es = do
  let felem = head es
      t     = expressionType felem

  tid <- findTypeID t
  let at = SutChain (length es) tid
      expr = ExprConstructor at (SutArray es)

  checkArrayType expr t (map expressionType es)
  return expr

-- |Checks the type of the array is consistent with the types of its elements
checkArrayType :: SutExpression -> SutType -> [SutType] -> SutMonad ()
checkArrayType expr t [] = return ()
checkArrayType expr t (t':ts) = do
  unless (t == t') $ typeError expr t t' "Chains must be consistent with their type"
  checkArrayType expr t ts

-- |Constructs a struct from the list of ID -> Expression mappings
--
-- Right now, we implement these by storing the member IDs into the type graph
--
-- Note: We do not allow duplicated member IDs
constructStruct :: [(SutID, SutExpression)] -> SutMonad SutExpression
constructStruct es = do
  let rms = repeated (map fst es)
  unless (null rms) $ mapM_ duplicateMemberError rms
  let mts = map (second expressionType) es
  mdef <- mapM memberType mts
  let at   = SutMachine mdef
      expr = ExprConstructor at (SutStruct es)
  return expr

-- |Transforms a member with its type definition to a member with its type ID
memberType :: (SutID, SutType) -> SutMonad (SutID, SutTypeID)
memberType (mid, t) = findTypeID t >>= \tid -> return (mid, tid)



-- Unary operations
-- ================================================================================================
unaryPlus, unaryMinus, unaryNot :: SutUnaryOp
unaryPlus e'  = let e = checkNumeric e' in return $ UnaryOp (expressionType e) SutOpPos e
unaryMinus e' = let e = checkNumeric e' in return $ UnaryOp (expressionType e) SutOpNeg e
unaryNot e'   = let e = checkBoolean e' in return $ UnaryOp (expressionType e) SutOpNot e



-- Binary operations
-- ================================================================================================

-- Helpers
-- ------------------------------------------------------------------------------------------------

-- |Gives the most general type from two expressions to which they can be casted
generalizeExprType :: SutExpression -> SutExpression -> SutType
generalizeExprType e1 e2 = let t1 = expressionType e1
                               t2 = expressionType e2
                            in generalizeTypes t1 t2

-- |General constructor for binary operation with the given operator applying the given checks
binaryOp :: ExprTransform -- ^ Transform for first operand
         -> ExprTransform -- ^ Transform for second operand
         -> ExprTransform -- ^ Transform result
         -> SutOperator   -- ^ Operator
         -> SutBinaryOp   -- ^ Both operands and result
binaryOp f1 f2 finalCheck op e1' e2' = let (e1, e2) = (f1 e1', f2 e2')
                                           gt = generalizeExprType e1 e2
                                           result = BinaryOp gt op e1 e2
                                        in return $ finalCheck result

-- |Binary operation specific constructor check
numericBinaryOp, booleanBinaryOp, sortBinaryOp, eqBinaryOp :: SutOperator -> SutBinaryOp
numericBinaryOp = binaryOp checkNumeric  checkNumeric  checkNumeric
booleanBinaryOp = binaryOp checkBoolean  checkBoolean  checkBoolean
sortBinaryOp    = binaryOp checkSortable checkSortable checkBoolean
eqBinaryOp      = binaryOp checkEq       checkEq       checkBoolean


-- Numerical operations
-- ------------------------------------------------------------------------------------------------
opAddition, opSubstraction, opMultiplication :: SutBinaryOp
opAddition       = numericBinaryOp SutOpAdd
opSubstraction   = numericBinaryOp SutOpSub
opMultiplication = numericBinaryOp SutOpMul

opDivision, opIntDivision, opModulo :: SutBinaryOp
opDivision    = binaryOp checkNumeric checkNumeric checkNumeric SutOpDiv
opIntDivision = binaryOp checkNumeric checkNumeric checkInt     SutOpIntDiv
opModulo      = binaryOp checkInt     checkInt     checkFloat   SutOpMod

opPower :: SutBinaryOp
opPower = numericBinaryOp SutOpPow


-- Boolean operations (all return booleans)
-- ------------------------------------------------------------------------------------------------
opAnd, opOr :: SutBinaryOp
-- | Boolean AND receives two booleans
opAnd = booleanBinaryOp SutOpAnd
-- | Boolean OR receives two booleans
opOr  = booleanBinaryOp SutOpOr

opEqual, opNotEqual :: SutBinaryOp
-- | Equality check receives two "equalable" types
opEqual    = eqBinaryOp SutOpEqual
-- | NotEquality check receives two "equalable" types
opNotEqual = eqBinaryOp SutOpNotEq

opGreaterEqual, opLessEqual, opLess, opGreater :: SutBinaryOp
-- | (>=) receives two "sortable" types
opGreaterEqual = sortBinaryOp SutOpGEq
-- | (<=) receives two "sortable" types
opLessEqual    = sortBinaryOp SutOpLEq
-- | (>) receives two "sortable" types
opGreater      = sortBinaryOp SutOpGreater
-- | (<) receives two "sortable" types
opLess         = sortBinaryOp SutOpLess


-- Complex operations
-- ================================================================================================

-- |An assignement to the left side of the value from the right side
--
-- This checks that a general type for converting the right side exists
--
-- Note: An assignment is both an instruction and an expression
--
-- Note: It is known the left side is assignable
--
-- Note: Right side must be of a more specific type than left side
assignment :: SutBinaryOp
assignment e1 e2 = do
  let t1 = expressionType e1
      gt = generalizeExprType e1 e2
      t  = if gt == t1 then gt else primitiveError
  let expr = BinaryOp t SutOpAssign e1 e2
  when ((t1 /= primitiveError) && (gt == primitiveError)) $ typeError expr t1 gt "General type not found"
  when ((gt /= primitiveError) && (t == primitiveError)) $ typeError expr t1 gt "Right side needs to be a more specific type"
  return expr


-- |The indexation of the array in the left side with the key/index on the right side
--
-- This checks the left side is actually of an array type
--
-- Note: It is known the left side is assignable and the right side is int expression
--
-- Note: Left side must be array type
arrayGet :: SutBinaryOp
arrayGet array index = case expressionType array of
  SutChain _ tid -> do
    t <- findExistentType tid
    return $ ArrayGet t array index
  wrongType      -> do
    let expr = ArrayGet primitiveError array index
    typeError expr (SutChain 0 0) wrongType "Not indexable"
    return $ ArrayGet primitiveError array index

-- |The access to a member/component (ID right) of a thing/machine (left)
--
-- This checks left side is either a Machine or a Thing, and that it knows a member with the ID
--
-- Note: It is known the left lise is assignable
memberGet :: SutExpression -> SutID -> SutMonad SutExpression
memberGet struct mid = case expressionType struct of
    SutMachine ms -> checkMember ms
    SutThing ms   -> checkMember ms
    wrongType     -> do
      typeError struct (SutMachine [(mid, 0)]) wrongType "Not a structured type: Machine or Thing"
      return $ MemberGet primitiveError struct mid
  where
    checkMember :: [(SutID, SutTypeID)] -> SutMonad SutExpression
    checkMember ms = do
      let member = find ((mid ==) . fst) ms
          isPresent = isJust member
          tid = if isPresent then snd (fromJust member) else (-1)
      unless isPresent $ undefinedError mid CatMember ("Member '" ++ mid ++ "' not present in structure definition")
      t <- findExistentType tid
      return $ MemberGet t struct mid


-- |Creates a direction to a value of type given
--
-- Note: We are already given the existent type ID
--
-- Note: Left side is known to be a person
createPointer :: SutID -> SutTypeID -> SutMonad SutExpression
createPointer pid tid = return $ CreatePointer (SutDirection tid) pid


-- |Creates a dereferencing expression for the given expression
--
-- This checks the expression is a direction to somewhere
--
-- Note: It is know the expression is assignable
dereference :: SutUnaryOp
dereference e = do
  let t' = expressionType e
  case t' of
    SutDirection tid -> do
      t <- findExistentType tid
      return $ Dereference t e
    wrongType -> do
      tid' <- findTypeID t'
      typeError e (SutDirection 0) wrongType "Not a direction"
      return $ Dereference primitiveError e


-- |A call to a function (ID) with parameters (given list)
--
-- This checks for the correct number of arguments and their types
--
-- Note: Left side is known to be existent function (?)
functionCall :: SutID -> [SutExpression] -> SutMonad SutExpression
functionCall fid actualParams = do
  SutState{parserTable = table} <- get
  func <- findFunction fid
  case func of
    Nothing    -> return $ SutCall primitiveError fid actualParams
    Just func' -> do
      let formalParams = symParams func'
          rType = symType func'
      formalPTypes <- mapM (findExistentType . paramType) formalParams
      returnType   <- findExistentType rType
      let (fpl, apl) = (length formalParams, length actualParams)
      when (fpl /= apl) $ argumentsNumberError fid fpl apl
      checkParamTypes formalPTypes actualParams
      return $ SutCall returnType fid actualParams


-- |Compares the given formal and actual parameters types
checkParamTypes :: [SutType] -> [SutExpression] -> SutMonad ()
checkParamTypes = checkParamTypes' 1
  where checkParamTypes' :: Int -> [SutType] -> [SutExpression] -> SutMonad ()
        checkParamTypes' i [] _ = return ()
        checkParamTypes' i _ [] = return ()
        checkParamTypes' i (ft:fts) (p:ps) = do
          let t = expressionType p
          unless (ft == t) $ typeError p ft t ("Argument #" ++ show i)
          checkParamTypes' (i+1) fts ps
