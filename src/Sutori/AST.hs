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
data SutInstruction = SutInstExpression SutExpression
                    | SutSelection      SutID SutExpression SutBlock SutBlock
                    | SutIterationU     SutID SutExpression SutBlock
                    | SutIterationB     SutID SutExpression SutBlock
                    | SutFreePointer    SutID SutExpression
                    | SutPrintVal       SutID SutExpression
                    | SutReadVal        SutID SutExpression
                    | SutReturn         SutID SutExpression

-- A SutExpression
data SutExpression = SutArrayItem       SutTypeID SutExpression SutExpression
                   | SutBinaryOp        SutTypeID SutOperator SutExpression SutExpression
                   | SutCall            SutTypeID SutID [SutExpression]
                   | SutCreatePointer   SutTypeID SutExpression
                   | SutExprConstructor SutTypeID SutConstructor
                   | SutExprID          SutTypeID SutID
                   | SutExprLiteral     SutTypeID SutLiteral
                   | SutPointed         SutTypeID SutExpression
                   | SutStructMember    SutTypeID SutExpression SutID
                   | SutUnaryOp         SutTypeID SutOperator SutExpression

-- A SutLiteral
data SutLiteral = SutString String
                | SutInt Int
                | SutFloat Float
                | SutChar Char
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
