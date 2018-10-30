{-|
Description : Provides functions that check an expression, issue type errors if necessary, and
              return the (maybe modified as a type error) expressions
-}
module Sutori.Parser.TypeCheck where

import Sutori.AST   (SutExpression, expressionType, withPrimitiveType, asTypeError)
import Sutori.Types
  ( SutType(SutPrimitiveType), SutPrimitive
  , toTypeNum, toTypeWallet, toTypePhrase, toTypeLight, toTypeBag, toTypeSortable, toTypeEq)



-- Checks
-- ===============================================================================================
-- All functions convert the expression to either the most general wanted type... or a type error

-- |Takes a conversion function from primitive to primitive and checks/converts the given expression
checkPrimitiveType :: (SutPrimitive -> SutPrimitive) -> SutExpression -> SutExpression
checkPrimitiveType toType expr = let typeDef = expressionType expr
                                  in case typeDef of
                                      (SutPrimitiveType p) -> let newType = toType p
                                                               in withPrimitiveType newType expr
                                      _                    -> asTypeError expr

-- |Checks if expression is or can be converted to numeric
checkNumeric :: SutExpression -> SutExpression
checkNumeric = checkPrimitiveType toTypeNum

-- |Checks if expression is checked for equality
checkEq :: SutExpression -> SutExpression
checkEq = checkPrimitiveType toTypeEq

-- |Checks if expression is or can be converted to a sortable type
checkSortable :: SutExpression -> SutExpression
checkSortable = checkPrimitiveType toTypeSortable

-- |Checks if expression is or can be converted to boolean
checkBoolean :: SutExpression -> SutExpression
checkBoolean = checkPrimitiveType toTypeLight

-- |Checks if expression is or can be converted to an index type
checkIndex :: SutExpression -> SutExpression
checkIndex = checkInt

-- |Checks if expression is or can be converted to integer (index type)
checkInt :: SutExpression -> SutExpression
checkInt = checkPrimitiveType toTypeBag

-- |Checks if expression is or can be converted to float
checkFloat :: SutExpression -> SutExpression
checkFloat = checkPrimitiveType toTypeWallet

-- |Checks if expression is or can be converted to string for printing
checkPrintable :: SutExpression -> SutExpression
checkPrintable = checkPrimitiveType toTypePhrase
