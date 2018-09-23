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

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))
import Sutori.Types(SutTypeID, SutTypedExpression(getExpressionType), showMember)

-- A SutBlock is a list of instructions
type SutBlock = [SutInstruction]

-- A Sutori Module has a name and a SutBlock
data SutModule = SutModule SutID SutBlock

instance SutShow SutModule where
  showSut (SutModule id b) = SutLogNode ("Module: " ++ show id) (map showSut b)

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
  showSut (SutInstExpression e)             = SutLogNode "Expression as instruction:" [showSut e]
  showSut (SutSelection who cond ifb elseb) = let condition = SutLogNode "Condition:" [showSut cond]
                                                  ifblock   = SutLogNode "If Block:" (map showSut ifb)
                                                  elseblock = SutLogNode "Else Block:" (map showSut elseb)
                                               in SutLogNode ("Selection by " ++ show who) [condition, ifblock, elseblock]
  showSut (SutIterationU who cond b)        = let condition = SutLogNode "Condition:" [showSut cond]
                                                  block     = SutLogNode "Block:" (map showSut b)
                                               in SutLogNode ("Unbounded Iteration by " ++ show who) [condition, block]
  showSut (SutIterationB who cond b)        = let condition = SutLogNode "Condition:" [showSut cond]
                                                  block     = SutLogNode "Block:" (map showSut b)
                                               in SutLogNode ("Bounded Iteration by " ++ show who) [condition, block]
  showSut (SutFreePointer who e)            = SutLogNode ("Pointer freed by " ++ show who ++ " from expr:") [showSut e]
  showSut (SutPrintVal who e)               = SutLogNode ("Printed value by " ++ show who ++ " from expr:") [showSut e]
  showSut (SutReadVal who e)                = SutLogNode ("Read value by " ++ show who ++ " from expr:") [showSut e]
  showSut (SutReturn who e)                 = SutLogNode ("Returned value by " ++ show who ++ " from expr:") [showSut e]


-- A SutExpression
data SutExpression = SutArrayItem       SutType SutExpression SutExpression
                   | SutBinaryOp        SutType SutOperator SutExpression SutExpression
                   | SutCall            SutType SutID [SutExpression]
                   | SutCreatePointer   SutType SutExpression
                   | SutExprConstructor SutType SutConstructor
                   | SutExprID          SutType SutID
                   | SutExprLiteral     SutType SutLiteral
                   | SutPointed         SutType SutExpression
                   | SutStructMember    SutType SutExpression SutID
                   | SutUnaryOp         SutType SutOperator SutExpression

instance SutShow SutExpression where
  showSut (SutArrayItem t ae ie)    = let etype = SutLogNode "Type:" [showSut t]
                                          array = SutLogNode "Array:" [showSut ae]
                                          index = SutLogNode "Index:" [showSut ie]
                                       in SutLogNode "ArrayIndexation" [etype, array, index]
  showSut (SutBinaryOp t op e1 e2)  = let etype    = SutLogNode "Type:" [showSut t]
                                          operator = showSut op
                                          loperand = SutLogNode "Left Operand:" [showSut e1]
                                          roperand = SutLogNode "Right Operand:" [showSut e2]
                                       in SutLogNode "BinaryOperation" [etype, operator, loperand, roperand]
  showSut (SutCall t id es)         = let etype = SutLogNode "Type:" [showSut t]
                                          args  = SutLogNode "Arguments:" (map showSut es)
                                       in SutLogNode ("Function call to `" ++ show id ++ "` :") [etype, args]
  showSut (SutCreatePointer t e)    = let etype = SutLogNode "Type:" [showSut t]
                                          expr  = SutLogNode "Expression:" [showSut e]
                                       in SutLogNode "NewPointer" [etype, expr]
  showSut (SutExprConstructor t c)  = let etype  = SutLogNode "Type:" [showSut t]
                                          constr = SutLogNode "Constructor:" [showSut c]
                                       in SutLogNode "DataStructure" [etype, constr]
  showSut (SutExprID t id)          = let etype = SutLogNode "Type:" [showSut t]
                                       in SutLogNode ("ID Expression : " ++ show id) [etype]
  showSut (SutExprLiteral t l)      = showSut l
  showSut (SutStructMember t se id) = let etype = SutLogNode "Type:" [showSut t]
                                          struct = SutLogNode "Struct:" [showSut se]
                                       in SutLogNode ("Acces to member: " ++ show id) [etype, struct]
  showSut (SutUnaryOp t op e)       = let etype = SutLogNode "Type:" [showSut t]
                                          operator = showSut op
                                          expr = SutLogNode "Operand: " [showSut e]
                                       in SutLogNode "UnaryOperation" [etype, operator, expr]

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
  showSut (SutString s)  = SutLogLeave $ "Literal: Phrase(" ++ show s ++ ")"
  showSut (SutInt i)     = SutLogLeave $ "Literal: Bag(" ++ show i ++")"
  showSut (SutFloat f)   = SutLogLeave $ "Literal: Waller(" ++ show f ++")"
  showSut (SutChar c)    = SutLogLeave $ "Literal: Letter(" ++ show c ++")"
  showSut (SutBool b)    = SutLogLeave $ "Literal: Light(" ++ show b ++")"

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
  showSut (SutArray es)  = SutLogNode "Chain:" (map showSut es)
  showSut (SutStruct es) = SutLogNode "Machine:" (map showMember es)

-- Define contructors as typed
instance SutTypedExpression SutConstructor where
  getExpressionType (SutArray (e:es)) = SutTypeArray  (getExpressionType e) (1 + length es)
  getExpressionType (SutStruct es)  = SutTypeStruct (getMembersType es)

getMembersType [(id, e)]    = [(id, getExpressionType e)]
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
  showSut SutOpNeg     = SutLogLeave "Operator: -negative"
  showSut SutOpNot     = SutLogLeave "Operator: !negation"
  showSut SutOpDer     = SutLogLeave "Operator: *dereference"
  showSut SutOpAdd     = SutLogLeave "Operator: +"
  showSut SutOpSub     = SutLogLeave "Operator: -"
  showSut SutOpMul     = SutLogLeave "Operator: *"
  showSut SutOpDiv     = SutLogLeave "Operator: div"
  showSut SutOpIntDiv  = SutLogLeave "Operator: /"
  showSut SutOpMod     = SutLogLeave "Operator: %"
  showSut SutOpPow     = SutLogLeave "Operator: ^"
  showSut SutOpAnd     = SutLogLeave "Operator: and"
  showSut SutOpOr      = SutLogLeave "Operator: or"
  showSut SutOpEqual   = SutLogLeave "Operator: =="
  showSut SutOpNotEq   = SutLogLeave "Operator: /="
  showSut SutOpGEq     = SutLogLeave "Operator: >="
  showSut SutOpLEq     = SutLogLeave "Operator: <="
  showSut SutOpGreater = SutLogLeave "Operator: >"
  showSut SutOpLess    = SutLogLeave "Operator: <"
  showSut SutOpAssign  = SutLogLeave "Operator: ="
  showSut SutOpIndex   = SutLogLeave "Operator: indexation[i]"
  showSut SutOpMember  = SutLogLeave "Operator: memberGet->a"


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
