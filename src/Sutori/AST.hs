module Sutori.AST where

import System.IO
import Data.Maybe
import Control.Monad
import Control.Monad.State

import Sutori.Utils
import Sutori.Types

type SutID = String
newtype SutModule = SutModule SutID SutBlock
type SutBlock = [SutInstruction]
type SutParam = (SutType, SutID)

data SutInstruction = SutInstExpression SutExpression
                    | SutSelection      SutID SutExpression SutBlock SutBlock
                    | SutIterationU     SutID SutExpression SutBlock
                    | SutIterationB     SutID SutExpression SutBlock
                    | SutCreatePointer  SutID SutType
                    | SutFreePointer    SutID SutExpression
                    | SutPrintVal       SutID SutExpression
                    | SutReadVal        SutID SutExpression
                    | SutReturn         SutID SutExpression
                    deriving (Show, Eq)

data SutExpression = SutExprLiteral     SutType SutLiteral
                   | SutExprConstructor SutType SutConstructor
                   | SutBinaryOp        SutType SutBiOp SutExpression SutExpression
                   | SutUnaryOp         SutType SutUnOp SutExpression
                   | SutCall            SutType SutID [SutExpression]
                   | SutExprID          SutType SutID
                   | SutArrayItem       SutType SutExpression SutExpression
                   | SutStructMember    SutType SutExpression SutID
                   deriving (Show, Eq)

data SutLiteral = SutString String
                | SutInt Int
                | SutFloat Float
                | SutChar Char
                | SutBool Bool
                deriving (Show, Eq)

data SutConstructor = SutArray [SutExpression]
                    | SutStruct [(SutID, SutExpression)]
                    deriving (Show, Eq)

data SutUnOp = SutOpPos
             | SutOpNeg
             | SutOpNot
             | SutOpDer
             deriving (Show, Eq)

data SutBiOp  = SutOpAdd
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
              deriving (Show, Eq)


identU = "|  "
ident n = replicateM_ n $ putStr identU

putStrWithIdent n s = ident n >> putStr s

putStrLnWithIdent n s = ident n >> putStrLn s


logByPerson n p = putStrWithIdent (n+1) "By Person: " >> putStrLn p

logID :: Int -> SutID -> IO()
logID n s = putStrLnWithIdent n $ "ID: " ++ s

logModule :: Int -> SutModule -> IO()
logModule n (s, b) = do
  putStrLnWithIdent n "Module"
  logID (n+1) s
  putStrLnWithIdent (n+1) "Block:"
  logBlock (n+2) b

logBlock :: Int -> SutBlock -> IO()
logBlock n [] = putStrLnWithIdent n "Code block (empty)"
logBlock n is = foldr ((>>) . logInstruction n) (return ()) is

logInstruction :: Int -> SutInstruction -> IO()
logInstruction n (SutInstExpression e) = do
  putStrLnWithIdent n "Expression Instruction"
  logExpression (n+1) e
logInstruction n (SutSelection p c i e) = do
  putStrLnWithIdent n "Selection"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Condition:" >> logExpression (n+2) c
  putStrLnWithIdent (n+1) "If-block:" >> logBlock (n+2) i
  putStrLnWithIdent (n+1) "Else-block:" >> logBlock (n+2) e
logInstruction n (SutIterationU p e b) = do
  putStrLnWithIdent n "Iteration (Unbounded)"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "While:" >> logExpression (n+2) e
  putStrLnWithIdent (n+1) "Block:" >> logBlock (n+2) b
logInstruction n (SutIterationB p e b) = do
  putStrLnWithIdent n "Iteration (Bounded)"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Repetitions:" >> logExpression (n+2) e
  putStrLnWithIdent (n+1) "Block:" >> logBlock (n+2) b
logInstruction n (SutCreatePointer p t) = do
  putStrLnWithIdent n "Pointer creation"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Type:"
logInstruction n (SutFreePointer p e) = do
  putStrLnWithIdent n "Pointer freeing"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Pointer:"
  logExpression (n+2) e
logInstruction n (SutPrintVal p e) = do
  putStrLnWithIdent n "Print Instruction"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Expression to print:"
  logExpression (n+2) e
logInstruction n (SutReadVal p e) = do
  putStrLnWithIdent n "Read Instruction"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Read into:"
  logExpression (n+2) e
logInstruction n (SutReturn p e) = do
  putStrLnWithIdent n "Return Instruction"
  logByPerson (n+1) p
  putStrLnWithIdent (n+1) "Returns:"
  logExpression (n+2) e

logExpression :: Int -> SutExpression -> IO()
logExpression n (SutExprLiteral t l) = logLiteral n l
logExpression n (SutExprConstructor t c) = do
  putStrLnWithIdent n "Constructor"
  putStrLnWithIdent (n+1) "Type:"
  logConstructor (n+1) c
logExpression n (SutBinaryOp t o e1 e2) = do
  putStrLnWithIdent n $ "Binary Operation (" ++ printSut o ++ ")"
  putStrLnWithIdent (n+1) "Type:"
  putStrLnWithIdent (n+1) "First operand:"
  logExpression (n+2) e1
  putStrLnWithIdent (n+1) "Second operand:"
  logExpression (n+2) e2
logExpression n (SutUnaryOp t o e) = do
  putStrLnWithIdent n $ "Unary Operation (" ++ printSut o ++ ")"
  putStrLnWithIdent (n+1) "Type:"
  putStrLnWithIdent (n+1) "Operand"
  logExpression (n+2) e
logExpression n (SutCall t i es) = do
  putStrLnWithIdent n "Function Call"
  putStrLnWithIdent (n+1) "Type:"
  logID (n+1) i
  putStrLnWithIdent (n+1) "Arguments:"
  foldr ((>>) . logExpression (n+2)) (return ()) es
logExpression n (SutExprID t i) = do
  putStrLnWithIdent n "Variable"
  putStrLnWithIdent (n+1) "Type:"
  logID (n+1) i
logExpression n (SutArrayItem t a i) = do
  putStrLnWithIdent n "Array Item"
  putStrLnWithIdent (n+1) "Type:"
  putStrLnWithIdent (n+1) "Array:"
  logExpression (n+2) a
  putStrLnWithIdent (n+1) "Index:"
  logExpression (n+2) i
logExpression n (SutStructMember t e i) = do
  putStrLnWithIdent n "Structure member"
  putStrLnWithIdent (n+1) "Type:"
  putStrLnWithIdent (n+1) "Structure:"
  logExpression (n+2) e
  logID (n+1) i

logLiteral :: Int -> SutLiteral -> IO()
logLiteral n (SutString s)  = putStrLnWithIdent n $ "Literal string: \"" ++ show s ++ "\""
logLiteral n (SutInt i)     = putStrLnWithIdent n $ "Literal integer: " ++ show i
logLiteral n (SutFloat f)   = putStrLnWithIdent n $ "Literal float: " ++ show f
logLiteral n (SutChar c)    = putStrLnWithIdent n $ "Literal character: '" ++ show c ++ "'"
logLiteral n (SutBool b)    = putStrLnWithIdent n $ "Literal boolean: " ++ show b

logConstructor :: Int -> SutConstructor -> IO()
logConstructor n (SutArray es) = do
  putStrLnWithIdent n "Constructed Array"
  foldr ((>>) . logExpression (n+1)) (return ()) es
logConstructor n (SutStruct nes) = do
  putStrLnWithIdent n "Constructed Structure"
  foldr ((>>) . logNamedExpression) (return ()) nes
    where
      logNamedExpression (i, e) = do
        putStrLnWithIdent (n+1) "Member"
        logID (n+2) i
        putStrLnWithIdent (n+1) "Value:"
        logExpression (n+2) e


instance SutPrint SutUnOp where
  printSut SutOpNeg     = "-negative"
  printSut SutOpNot     = "!negation"
  printSut SutOpDer     = "*dereference"

instance SutPrint SutBiOp where
  printSut SutOpAdd     = "+"
  printSut SutOpSub     = "-"
  printSut SutOpMul     = "*"
  printSut SutOpDiv     = "div"
  printSut SutOpIntDiv  = "/"
  printSut SutOpMod     = "%"
  printSut SutOpPow     = "^"
  printSut SutOpAnd     = "and"
  printSut SutOpOr      = "or"
  printSut SutOpEqual   = "=="
  printSut SutOpNotEq   = "/="
  printSut SutOpGEq     = ">="
  printSut SutOpLEq     = "<="
  printSut SutOpGreater = ">"
  printSut SutOpLess    = "<"
  printSut SutOpAssign  = "="
  printSut SutOpIndex   = "indexation[i]"
  printSut SutOpMember  = "memberGet->a"
