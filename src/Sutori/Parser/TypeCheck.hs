module Sutori.Parser.TypeCheck where

import Sutori.Types.Constructors (SutType(SutPrimitiveType))
import Sutori.Types.Primitives   (SutPrimitive, toTypeNum, toTypeWallet, toTypePhrase, toTypeLight, toTypeBag)
import Sutori.AST                (SutExpression, expressionType, withPrimitiveType, asTypeError)

-- Checks
-- ===============================================================================================

-- Takes a conversion function from primitive to primitive and checks/converts the given expression
checkPrimitiveType :: (SutPrimitive -> SutPrimitive) -> SutExpression -> SutExpression
checkPrimitiveType toType expr = let typeDef = expressionType expr
                                  in case typeDef of
                                      (SutPrimitiveType p) -> let newType = toType p
                                                               in withPrimitiveType newType expr
                                      _                    -> asTypeError expr

-- Checks if expression is or can be converted to numeric
-- Returns the expression with its type converted if possible
checkNumeric :: SutExpression -> SutExpression
checkNumeric = checkPrimitiveType toTypeNum

-- Checks if expression is or can be converted to boolean
-- Returns the expression with its type converted if possible
checkBoolean :: SutExpression -> SutExpression
checkBoolean = checkPrimitiveType toTypeLight

-- Checks if expression is or can be converted to integer (index type)
-- Returns the expression with its type converted if possible
checkIndex :: SutExpression -> SutExpression
checkIndex = checkPrimitiveType toTypeBag

-- Checks if expression is or can be converted to string for printing
-- Returns the expression with its type converted if possible
checkPrintable :: SutExpression -> SutExpression
checkPrintable = checkPrimitiveType toTypePhrase
