{-|
Description : Defines extractor functions for AST nodes, and transformations
-}
module Sutori.AST.Utils
( expressionType
, withPrimitiveType
, asTypeError
) where

import Data.Maybe

import Sutori.Types     (SutType(SutPrimitiveType), SutPrimitive(SutTypeError))
import Sutori.AST.Nodes

-- |Extracts the type from any expression
expressionType :: SutExpression -> SutType
expressionType (ArrayGet t _ _)      = t
expressionType (BinaryOp t _ _ _)    = t
expressionType (UnaryOp t _ _)       = t
expressionType (SutCall t _ _)       = t
expressionType (CreatePointer t _)   = t
expressionType (ExprConstructor t _) = t
expressionType (ExprID t _)          = t
expressionType (ExprLiteral t _)     = t
expressionType (Dereference t _)     = t
expressionType (MemberGet t _ _ )    = t

-- |Clones the expression overriding the type for the given primitive type
-- This is used for type coersion, mostly
withPrimitiveType :: SutPrimitive -> SutExpression -> SutExpression
withPrimitiveType p (ArrayGet _ e1 e2)    = ArrayGet (SutPrimitiveType p) e1 e2
withPrimitiveType p (BinaryOp _ o e1 e2)  = BinaryOp (SutPrimitiveType p) o e1 e2
withPrimitiveType p (UnaryOp _ o e)       = UnaryOp (SutPrimitiveType p) o e
withPrimitiveType p (SutCall _ id ps)     = SutCall (SutPrimitiveType p) id ps
withPrimitiveType p (CreatePointer _ e)   = CreatePointer (SutPrimitiveType p) e
withPrimitiveType p (ExprConstructor _ c) = ExprConstructor (SutPrimitiveType p) c
withPrimitiveType p (ExprID _ id)         = ExprID (SutPrimitiveType p) id
withPrimitiveType p (ExprLiteral _ l)     = ExprLiteral (SutPrimitiveType p) l
withPrimitiveType p (Dereference _ e)     = Dereference (SutPrimitiveType p) e
withPrimitiveType p (MemberGet _ e id)    = MemberGet (SutPrimitiveType p) e id

-- |Clones the expression but sets the type as a TypeError
asTypeError :: SutExpression -> SutExpression
asTypeError = withPrimitiveType SutTypeError
