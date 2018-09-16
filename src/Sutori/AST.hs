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

import Sutori.Utils(SutShow(showSut))
import Sutori.Types(SutType(..), SutTypedExpression(getExpressionType), showMember)

-- A Sutori ID is a string
type SutID = String

-- A SutBlock is a list of instructions
type SutBlock = [SutInstruction]

-- A Sutori Module has a name and a SutBlock
data SutModule = SutModule SutID SutBlock

instance SutShow SutModule where
  showSut (SutModule id b) = "Module:" ++ show id ++ "(" ++ concatMap showSut b ++ ")"

-- A SutIntruction
data SutInstruction = SutInstExpression SutExpression
                    | SutSelection      SutID SutExpression SutBlock SutBlock
                    | SutIterationU     SutID SutExpression SutBlock
                    | SutIterationB     SutID SutExpression SutBlock
                    | SutFreePointer    SutID SutExpression
                    | SutPrintVal       SutID SutExpression
                    | SutReadVal        SutID SutExpression
                    | SutReturn         SutID SutExpression

instance SutShow SutInstruction where
  showSut (SutInstExpression e)             = "Expression(" ++ showSut e ++ ")"
  showSut (SutSelection who cond ifb elseb) = ""
  showSut (SutIterationU who cond b)        = ""
  showSut (SutIterationB who cond b)        = ""
  showSut (SutFreePointer who e)            = ""
  showSut (SutPrintVal who e)               = ""
  showSut (SutReadVal who e)                = ""
  showSut (SutReturn who e)                 = ""


-- A SutExpression
data SutExpression = SutArrayItem       SutType SutExpression SutExpression
                   | SutBinaryOp        SutType SutOperator SutExpression SutExpression
                   | SutCall            SutType SutID [SutExpression]
                   | SutCreatePointer   SutType SutID
                   | SutExprConstructor SutType SutConstructor
                   | SutExprID          SutType SutID
                   | SutExprLiteral     SutType SutLiteral
                   | SutPointed         SutType SutExpression
                   | SutStructMember    SutType SutExpression SutID
                   | SutUnaryOp         SutType SutOperator SutExpression

instance SutShow SutExpression where
  showSut (SutArrayItem t ae ie)    = "ArrayIndexation(Type: (" ++ showSut t ++ ") ArrayExpr: (" ++ showSut ae ++ ") IndexExpr:(" ++ showSut ie ++ "))"
  showSut (SutBinaryOp t op e1 e2)  = "BinaryOperation(Type: (" ++ showSut t ++ ") Operator: (" ++ showSut op ++ ") LeftExpr: (" ++ showSut e1 ++ ") RightExpr: (" ++ showSut e2 ++ ")))"
  showSut (SutCall t id es)         = "FunctionCall:" ++ show id ++ "(Type: ("++ showSut t++") Args: (" ++ concatMap showSut es ++ "))"
  showSut (SutCreatePointer t id)   = "NewPointer:" ++ show id ++ "(Type: (" ++ showSut t ++ "))"
  showSut (SutExprConstructor t c)  = "DataStructure(Type: (" ++ showSut t ++ ") Constructor: (" ++ showSut c ++ "))"
  showSut (SutExprID t id)          = "ExpressionID:" ++ show id ++ "(Type: (" ++ showSut t ++ "))"
  showSut (SutExprLiteral t l)      = "Literal(Type: (" ++ showSut t ++ ") Value: (" ++ showSut l ++ "))"
  showSut (SutStructMember t se id) = "MemberAccess(Type: (" ++ showSut t ++ ") StructExpr: (" ++ showSut se ++ ") Member:(" ++ show id ++ "))"
  showSut (SutUnaryOp t op e)       = "UnaryOperation(Type: (" ++ showSut t ++ ") Operator: (" ++ showSut op ++ ") Operand: (" ++ showSut e ++"))"

-- Define complex expressions as typed
instance SutTypedExpression SutExpression where
  getExpressionType (SutArrayItem t _ _)     = t
  getExpressionType (SutBinaryOp t _ _ _)    = t
  getExpressionType (SutCall t _ _)          = t
  getExpressionType (SutCreatePointer t _)   = t
  getExpressionType (SutExprConstructor t _) = t
  getExpressionType (SutExprID t _)          = t
  getExpressionType (SutExprLiteral t _)     = t
  getExpressionType (SutStructMember t _ _)  = t
  getExpressionType (SutUnaryOp t _ _)       = t


-- A SutLiteral
data SutLiteral = SutString String
                | SutInt Int
                | SutFloat Float
                | SutChar Char
                | SutBool Bool

instance SutShow SutLiteral where
  showSut (SutString s)  = "Phrase(" ++ show s ++ ")"
  showSut (SutInt i)     = "Bag(" ++ show i ++")"
  showSut (SutFloat f)   = "Waller(" ++ show f ++")"
  showSut (SutChar c)    = "Letter(" ++ show c ++")"
  showSut (SutBool b)    = "Light(" ++ show b ++")"

-- Define literals as typed
instance SutTypedExpression SutLiteral where
  getExpressionType (SutString _) = SutTypeString
  getExpressionType (SutInt _)    = SutTypeInt
  getExpressionType (SutFloat _)  = SutTypeFloat
  getExpressionType (SutChar _)   = SutTypeChar
  getExpressionType (SutBool _)   = SutTypeBool



-- Complex data structure constructors
data SutConstructor = SutArray [SutExpression]
                    | SutStruct [(SutID, SutExpression)]

instance SutShow SutConstructor where
  showSut (SutArray es)  = "Chain(" ++ concatMap showSut es ++")"
  showSut (SutStruct es) = "Machine(" ++ concatMap showMember es ++")"

-- Define contructors as typed
instance SutTypedExpression SutConstructor where
  getExpressionType (SutArray (e:es)) = SutTypeArray  (getExpressionType e) (1 + length es)
  getExpressionType (SutStruct es)  = SutTypeStruct (getMembersType es)

getMembersType ((id, e):[]) = [(id, getExpressionType e)]
getMembersType ((id, e):es) = (id, getExpressionType e) : getMembersType es



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

-- Provide showSut for operators
instance SutShow SutOperator where
  showSut SutOpNeg     = "-negative"
  showSut SutOpNot     = "!negation"
  showSut SutOpDer     = "*dereference"
  showSut SutOpAdd     = "+"
  showSut SutOpSub     = "-"
  showSut SutOpMul     = "*"
  showSut SutOpDiv     = "div"
  showSut SutOpIntDiv  = "/"
  showSut SutOpMod     = "%"
  showSut SutOpPow     = "^"
  showSut SutOpAnd     = "and"
  showSut SutOpOr      = "or"
  showSut SutOpEqual   = "=="
  showSut SutOpNotEq   = "/="
  showSut SutOpGEq     = ">="
  showSut SutOpLEq     = "<="
  showSut SutOpGreater = ">"
  showSut SutOpLess    = "<"
  showSut SutOpAssign  = "="
  showSut SutOpIndex   = "indexation[i]"
  showSut SutOpMember  = "memberGet->a"


--
-- identU = "|  "
-- ident n = replicateM_ n $ putStr identU
--
-- putStrWithIdent n s = ident n >> putStr s
--
-- putStrLnWithIdent n s = ident n >> putStrLn s
--
--
-- logByPerson n p = putStrWithIdent (n + 1) "By Person: " >> putStrLn p
--
-- logID :: Int -> SutID -> IO ()
-- logID n s = putStrLnWithIdent n $ "ID: " ++ s
--
-- logModule :: Int -> SutModule -> IO ()
-- logModule n (SutModule s b) = do
--   putStrLnWithIdent n       "Module"
--   logID             (n + 1) s
--   putStrLnWithIdent (n + 1) "Block:"
--   logBlock          (n + 2) b
--
-- logBlock :: Int -> SutBlock -> IO ()
-- logBlock n [] = putStrLnWithIdent n "Code block (empty)"
-- logBlock n is = foldr ((>>) . logInstruction n) (return ()) is
--
-- logInstruction :: Int -> SutInstruction -> IO ()
-- logInstruction n (SutInstExpression e) = do
--   putStrLnWithIdent n       "Expression Instruction"
--   logExpression     (n + 1) e
-- logInstruction n (SutSelection p c i e) = do
--   putStrLnWithIdent n       "Selection"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Condition:" >> logExpression (n + 2) c
--   putStrLnWithIdent (n + 1) "If-block:" >> logBlock (n + 2) i
--   putStrLnWithIdent (n + 1) "Else-block:" >> logBlock (n + 2) e
-- logInstruction n (SutIterationU p e b) = do
--   putStrLnWithIdent n       "Iteration (Unbounded)"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "While:" >> logExpression (n + 2) e
--   putStrLnWithIdent (n + 1) "Block:" >> logBlock (n + 2) b
-- logInstruction n (SutIterationB p e b) = do
--   putStrLnWithIdent n       "Iteration (Bounded)"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Repetitions:" >> logExpression (n + 2) e
--   putStrLnWithIdent (n + 1) "Block:" >> logBlock (n + 2) b
-- logInstruction n (SutFreePointer p e) = do
--   putStrLnWithIdent n       "Pointer freeing"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Pointer:"
--   logExpression     (n + 2) e
-- logInstruction n (SutPrintVal p e) = do
--   putStrLnWithIdent n       "Print Instruction"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Expression to print:"
--   logExpression     (n + 2) e
-- logInstruction n (SutReadVal p e) = do
--   putStrLnWithIdent n       "Read Instruction"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Read into:"
--   logExpression     (n + 2) e
-- logInstruction n (SutReturn p e) = do
--   putStrLnWithIdent n       "Return Instruction"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Returns:"
--   logExpression     (n + 2) e
--
-- logExpression :: Int -> SutExpression -> IO ()
-- logExpression n (SutExprLiteral     t l) = logLiteral n l
-- logExpression n (SutExprConstructor t c) = do
--   putStrLnWithIdent n       "Constructor"
--   putStrLnWithIdent (n + 1) "Type:"
--   logConstructor    (n + 1) c
-- logExpression n (SutBinaryOp t o e1 e2) = do
--   putStrLnWithIdent n $ "Binary Operation (" ++ showSut o ++ ")"
--   putStrLnWithIdent (n + 1) "Type:"
--   putStrLnWithIdent (n + 1) "First operand:"
--   logExpression     (n + 2) e1
--   putStrLnWithIdent (n + 1) "Second operand:"
--   logExpression     (n + 2) e2
-- logExpression n (SutUnaryOp t o e) = do
--   putStrLnWithIdent n $ "Unary Operation (" ++ showSut o ++ ")"
--   putStrLnWithIdent (n + 1) "Type:"
--   putStrLnWithIdent (n + 1) "Operand"
--   logExpression     (n + 2) e
-- logExpression n (SutCall t i es) = do
--   putStrLnWithIdent n       "Function Call"
--   putStrLnWithIdent (n + 1) "Type:"
--   logID             (n + 1) i
--   putStrLnWithIdent (n + 1) "Arguments:"
--   foldr ((>>) . logExpression (n + 2)) (return ()) es
-- logExpression n (SutExprID t i) = do
--   putStrLnWithIdent n       "Variable"
--   putStrLnWithIdent (n + 1) "Type:"
--   logID             (n + 1) i
-- logExpression n (SutCreatePointer t p) = do
--   putStrLnWithIdent n       "Pointer creation"
--   logByPerson       (n + 1) p
--   putStrLnWithIdent (n + 1) "Type:"
-- logExpression n (SutArrayItem t a i) = do
--   putStrLnWithIdent n       "Array Item"
--   putStrLnWithIdent (n + 1) "Type:"
--   putStrLnWithIdent (n + 1) "Array:"
--   logExpression     (n + 2) a
--   putStrLnWithIdent (n + 1) "Index:"
--   logExpression     (n + 2) i
-- logExpression n (SutStructMember t e i) = do
--   putStrLnWithIdent n       "Structure member"
--   putStrLnWithIdent (n + 1) "Type:"
--   putStrLnWithIdent (n + 1) "Structure:"
--   logExpression     (n + 2) e
--   logID             (n + 1) i
--
-- logLiteral :: Int -> SutLiteral -> IO ()
-- logLiteral n (SutString s) =
--   putStrLnWithIdent n $ "Literal string: \"" ++ show s ++ "\""
-- logLiteral n (SutInt   i) = putStrLnWithIdent n $ "Literal integer: " ++ show i
-- logLiteral n (SutFloat f) = putStrLnWithIdent n $ "Literal float: " ++ show f
-- logLiteral n (SutChar c) =
--   putStrLnWithIdent n $ "Literal character: '" ++ show c ++ "'"
-- logLiteral n (SutBool b) = putStrLnWithIdent n $ "Literal boolean: " ++ show b
--
-- logConstructor :: Int -> SutConstructor -> IO ()
-- logConstructor n (SutArray es) = do
--   putStrLnWithIdent n "Constructed Array"
--   foldr ((>>) . logExpression (n + 1)) (return ()) es
-- logConstructor n (SutStruct nes) = do
--   putStrLnWithIdent n "Constructed Structure"
--   foldr ((>>) . logNamedExpression) (return ()) nes
--  where
--   logNamedExpression (i, e) = do
--     putStrLnWithIdent (n + 1) "Member"
--     logID             (n + 2) i
--     putStrLnWithIdent (n + 1) "Value:"
--     logExpression     (n + 2) e
