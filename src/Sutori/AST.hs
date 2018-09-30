module Sutori.AST
( SutID
, SutBlock
, SutModule(..)
, SutInstruction(..)
, SutExpression(..)
, SutLiteral(..)
, SutConstructor(..)
, SutOperator(..)
, expressionType
, withPrimitiveType
, asTypeError
) where

import Data.Maybe

import Sutori.Utils(SutID)
import Sutori.Types.Constructors (SutType(SutPrimitiveType))
import Sutori.Types.Primitives   (SutPrimitive(SutTypeError))

-- A SutBlock is a list of instructions
type SutBlock = [SutInstruction]

-- A Sutori Module has a name and a SutBlock
data SutModule = SutModule SutID SutBlock

-- A SutIntruction
data SutInstruction = InstAssignment SutExpression
                    | Selection      SutID SutExpression SutBlock SutBlock
                    | IterationU     SutID SutExpression SutBlock
                    | IterationB     SutID SutExpression SutBlock
                    | FreePointer    SutID SutExpression
                    | PrintVal       SutID SutExpression
                    | ReadVal              SutExpression
                    | ReturnVal            SutExpression

-- A SutExpression
data SutExpression = ArrayGet        SutType SutExpression SutExpression
                   | BinaryOp        SutType SutOperator SutExpression SutExpression
                   | UnaryOp         SutType SutOperator SutExpression
                   | SutCall         SutType SutID [SutExpression]
                   | CreatePointer   SutType SutExpression
                   | ExprConstructor SutType SutConstructor
                   | ExprID          SutType SutID
                   | ExprLiteral     SutType SutLiteral
                   | Pointed         SutType SutExpression
                   | MemberGet       SutType SutExpression SutID

expressionType :: SutExpression -> SutType
expressionType (ArrayGet t _ _)      = t
expressionType (BinaryOp t _ _ _)    = t
expressionType (UnaryOp t _ _)       = t
expressionType (SutCall t _ _)       = t
expressionType (CreatePointer t _)   = t
expressionType (ExprConstructor t _) = t
expressionType (ExprID t _)          = t
expressionType (ExprLiteral t _)     = t
expressionType (Pointed t _)         = t
expressionType (MemberGet t _ _ )    = t

withPrimitiveType :: SutPrimitive -> SutExpression -> SutExpression
withPrimitiveType p (ArrayGet _ e1 e2)    = ArrayGet (SutPrimitiveType p) e1 e2
withPrimitiveType p (BinaryOp _ o e1 e2)  = BinaryOp (SutPrimitiveType p) o e1 e2
withPrimitiveType p (UnaryOp _ o e)       = UnaryOp (SutPrimitiveType p) o e
withPrimitiveType p (SutCall _ id ps)     = SutCall (SutPrimitiveType p) id ps
withPrimitiveType p (CreatePointer _ e)   = CreatePointer (SutPrimitiveType p) e
withPrimitiveType p (ExprConstructor _ c) = ExprConstructor (SutPrimitiveType p) c
withPrimitiveType p (ExprID _ id)         = ExprID (SutPrimitiveType p) id
withPrimitiveType p (ExprLiteral _ l)     = ExprLiteral (SutPrimitiveType p) l
withPrimitiveType p (Pointed _ e)         = Pointed (SutPrimitiveType p) e
withPrimitiveType p (MemberGet _ e id)    = MemberGet (SutPrimitiveType p) e id

asTypeError :: SutExpression -> SutExpression
asTypeError = withPrimitiveType SutTypeError

-- A SutLiteral
data SutLiteral = SutString String
                | SutInt    Int
                | SutFloat  Float
                | SutChar   String
                | SutBool   Bool

-- Complex data structure constructors
data SutConstructor = SutArray [SutExpression]
                    | SutStruct [(SutID, SutExpression)]

-- Sutori operators
data SutOperator = SutOpPos
                 | SutOpNeg
                 | SutOpNot
                 | SutOpDer
                 | SutOpAdd
                 | SutOpSub
                 | SutOpMul
                 | SutOpDiv
                 | SutOpIntDiv
                 | SutOpMod
                 | SutOpPow
                 | SutOpAnd
                 | SutOpOr
                 | SutOpEqual
                 | SutOpNotEq
                 | SutOpGEq
                 | SutOpLEq
                 | SutOpGreater
                 | SutOpLess
                 | SutOpAssign
                 | SutOpIndex
                 | SutOpMember
