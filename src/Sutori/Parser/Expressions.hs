module Sutori.Parser.Expressions
( literalBool, literalChar, literalInt, literalFloat, literalString
, constructArray, constructStruct
, unaryPlus, unaryMinus, unaryNot
, opAddition, opSubstraction, opMultiplication, opDivision, opIntDivision, opModulo, opPower
, opGreaterEqual, opLessEqual, opLess, opGreater
, opEqual, opNotEqual, opAnd, opOr
, assignment, arrayGet, memberGet, functionCall, dereference, createPointer
) where

import Control.Arrow             (second)
import Control.Monad             (when, unless)
import Control.Monad.State.Lazy  (get)
import Data.List                 (find)
import Data.Maybe                (fromMaybe, isJust, fromJust)

import Sutori.AST                (SutExpression(..), SutLiteral(..), SutOperator(..), SutConstructor(..), expressionType)
import Sutori.Lexer.Tokens       (SutToken(tokenChar, tokenBool, tokenInt, tokenFloat, tokenString))
import Sutori.Monad              (SutMonad, SutState(SutState, typesGraph, typesNextID, parserTable))
import Sutori.Monad.Logger       (typeError, argumentsNumberError, undefinedError, duplicateSymbolError)
import Sutori.Parser.Symbols     (findType, findExistentType, findTypeID)
import Sutori.Parser.TypeCheck   (checkNumeric, checkBoolean, checkSortable, checkEq, checkInt, checkFloat)
import Sutori.SymTable           (SutSymbol(..), SutSymCategory(CatFunction, CatMember), SutParam(..), SutSymOther(..), lookupID, compareParamTypes)
import Sutori.Types.Constructors (SutType(..), generalizeTypes, primitiveError)
import Sutori.Types.Graph        (lookupType)
import Sutori.Types.Primitives   (SutTypeID, SutPrimitive(..), primitiveID)
import Sutori.Utils              (SutID, repeated)



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

-- Our grammar doesn't allow empty arrays
constructArray :: [SutExpression] -> SutMonad SutExpression
constructArray es = do
  let felem = head es
      t     = expressionType felem

  tid <- findTypeID t
  let at = SutChain (length es) tid
      expr = ExprConstructor at (SutArray es)

  checkArrayType expr t (map expressionType es)
  return expr

checkArrayType :: SutExpression -> SutType -> [SutType] -> SutMonad ()
checkArrayType expr t (t':ts) = unless (t == t') $
  typeError expr t t' "Chains must be consistent with their type"


-- We must check for duplicate members
constructStruct :: [(SutID, SutExpression)] -> SutMonad SutExpression
constructStruct es = do
  let rms = repeated (map fst es)
  unless (null rms) $ do
    let logError id = duplicateSymbolError id CatMember
    mapM_ (logError "Members of the same structure must not share names") rms
  let mts = map (second expressionType) es
  mdef <- mapM memberType mts
  let at   = SutMachine mdef
      expr = ExprConstructor at (SutStruct es)
  return expr

memberType :: (SutID, SutType) -> SutMonad (SutID, SutTypeID)
memberType (id, t) = findTypeID t >>= \tid -> return (id, tid)



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
binaryOp :: ExprTransform -> ExprTransform -> ExprTransform -- Transforms for fst, snd and res
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
      t  = if gt == t1 then gt else primitiveError
  let expr = BinaryOp t SutOpAssign e1 e2
  when ((t1 /= primitiveError) && (gt == primitiveError)) $ typeError expr t1 gt "General type not found"
  when ((gt /= primitiveError) && (t == primitiveError)) $ typeError expr t1 gt "Right side needs to be a more specific type"
  return expr


-- It is known the left side is assignable and the right side is integer
-- Left side must be array type
arrayGet :: SutExpression -> SutExpression -> SutMonad SutExpression
arrayGet array index = case expressionType array of
  SutChain _ tid -> do
    t <- findExistentType tid
    return $ ArrayGet t array index
  wrongType      -> do
    let expr = ArrayGet primitiveError array index
    typeError expr (SutChain 0 0) wrongType "Not indexable"
    return $ ArrayGet primitiveError array index

-- It is known the left lise is assignable
-- Left side can be either a struct of a union type
-- Right side must be ID from left side's type
memberGet :: SutExpression -> SutID -> SutMonad SutExpression
memberGet struct id = case expressionType struct of
    SutMachine ms -> checkMember ms
    SutThing ms   -> checkMember ms
    wrongType     -> do
      typeError struct (SutMachine [(id, 0)]) wrongType "Not a structured type: Machine or Thing"
      return $ MemberGet primitiveError struct id
  where
    checkMember :: [(SutID, SutTypeID)] -> SutMonad SutExpression
    checkMember ms = do
      let member = find ((id ==) . fst) ms
          isPresent = isJust member
          tid = if isPresent then snd (fromJust member) else (-1)
      unless isPresent $ undefinedError id CatMember ("Member '" ++ id ++ "' not present in structure definition")
      t <- findExistentType tid
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
      t <- findExistentType tid
      return $ Dereference t e
    wrongType -> do
      tid' <- findTypeID t'
      typeError e (SutDirection 0) wrongType "Not a direction"
      return $ Dereference primitiveError e


-- Compares the given formal and actual parameters types
checkParamTypes :: Int -> [SutType] -> [SutExpression] -> SutMonad ()
checkParamTypes i [] _ = return ()
checkParamTypes i _ [] = return ()
checkParamTypes i (ft:fts) (p:ps) = do
  let t = expressionType p
  unless (ft == t) $ typeError p ft t ("Argument #" ++ show i)
  checkParamTypes (i+1) fts ps


-- Left side is known to be existent function
-- Right side must be list of matching-type arguments
functionCall :: SutID -> [SutExpression] -> SutMonad SutExpression
functionCall id actualParams = do
  SutState{parserTable = table} <- get
  let SutSymbol{symType = tid, symOther = other} = head syms
        where syms  = filter isCat (lookupID table id) -- We know there's at least one
              isCat = (CatFunction ==) . symCat        -- There might be variables hiding the name
      formalParams = otherASTParams other
  formalPTypes <- mapM (findExistentType . paramType) formalParams
  returnType   <- findExistentType tid
  let (fpl, apl) = (length formalParams, length actualParams)
  when (fpl /= apl) $ argumentsNumberError id fpl apl
  checkParamTypes 1 formalPTypes actualParams
  return $ SutCall returnType id actualParams
