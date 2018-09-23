module Sutori.AST
( SutID
, SutBlock
, SutModule(..)
, SutInstruction(..)
, SutExpression(..)
, SutLiteral(..)
, SutConstructor(..)
, SutOperator(..)
) where

import System.IO
import Data.Maybe

import Sutori.Utils(SutID)
import Sutori.Types (SutTypeID)

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
                    | ReadVal        SutID SutExpression
                    | Return         SutID SutExpression

-- A SutExpression
data SutExpression = ArrayGet        SutTypeID SutExpression SutExpression
                   | BinaryOp        SutTypeID SutOperator SutExpression SutExpression
                   | UnaryOp         SutTypeID SutOperator SutExpression
                   | SutCall         SutTypeID SutID [SutExpression]
                   | CreatePointer   SutTypeID SutExpression
                   | ExprConstructor SutTypeID SutConstructor
                   | ExprID          SutTypeID SutID
                   | ExprLiteral     SutTypeID SutLiteral
                   | Pointed         SutTypeID SutExpression
                   | MemberGet       SutTypeID SutExpression SutID

-- A SutLiteral
data SutLiteral = SutString String
                | SutInt Int
                | SutFloat Float
                | SutChar String
                | SutBool Bool

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
