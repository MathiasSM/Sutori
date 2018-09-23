module Sutori.AST.Logger() where

import Sutori.Logger (SutShow(showSut), SutLog(SutLogLeave, SutLogNode))
import Sutori.AST


instance SutShow SutModule where
  showSut (SutModule id b) = SutLogNode ("Module: " ++ show id) (map showSut b)


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


instance SutShow SutExpression where
  showSut (SutArrayItem t ae ie)    = let etype = SutLogLeave $ "Type: " ++ show t
                                          array = SutLogNode "Array:" [showSut ae]
                                          index = SutLogNode "Index:" [showSut ie]
                                       in SutLogNode "ArrayIndexation" [etype, array, index]
  showSut (SutBinaryOp t op e1 e2)  = let etype    = SutLogLeave $ "Type: " ++ show t
                                          operator = showSut op
                                          loperand = SutLogNode "Left Operand:" [showSut e1]
                                          roperand = SutLogNode "Right Operand:" [showSut e2]
                                       in SutLogNode "BinaryOperation" [etype, operator, loperand, roperand]
  showSut (SutCall t id es)         = let etype = SutLogLeave $ "Type: " ++ show t
                                          args  = SutLogNode "Arguments:" (map showSut es)
                                       in SutLogNode ("Function call to `" ++ show id ++ "` :") [etype, args]
  showSut (SutCreatePointer t e)    = let etype = SutLogLeave $ "Type: " ++ show t
                                          expr  = SutLogNode "Expression:" [showSut e]
                                       in SutLogNode "NewPointer" [etype, expr]
  showSut (SutExprConstructor t c)  = let etype  = SutLogLeave $ "Type: " ++ show t
                                          constr = SutLogNode "Constructor:" [showSut c]
                                       in SutLogNode "DataStructure" [etype, constr]
  showSut (SutExprID t id)          = let etype = SutLogLeave $ "Type: " ++ show t
                                       in SutLogNode ("ID Expression : " ++ show id) [etype]
  showSut (SutExprLiteral t l)      = showSut l
  showSut (SutStructMember t se id) = let etype = SutLogLeave $ "Type: " ++ show t
                                          struct = SutLogNode "Struct:" [showSut se]
                                       in SutLogNode ("Acces to member: " ++ show id) [etype, struct]
  showSut (SutUnaryOp t op e)       = let etype = SutLogLeave $ "Type: " ++ show t
                                          operator = showSut op
                                          expr = SutLogNode "Operand: " [showSut e]
                                       in SutLogNode "UnaryOperation" [etype, operator, expr]


instance SutShow SutLiteral where
  showSut (SutString s)  = SutLogLeave $ "Literal: Phrase(" ++ show s ++ ")"
  showSut (SutInt i)     = SutLogLeave $ "Literal: Bag(" ++ show i ++")"
  showSut (SutFloat f)   = SutLogLeave $ "Literal: Waller(" ++ show f ++")"
  showSut (SutChar c)    = SutLogLeave $ "Literal: Letter(" ++ show c ++")"
  showSut (SutBool b)    = SutLogLeave $ "Literal: Light(" ++ show b ++")"


instance SutShow SutConstructor where
  showSut (SutArray es)  = SutLogNode "Chain:" (map showSut es)
  showSut (SutStruct es) = SutLogNode "Machine:" (map showMember es)

showMember (id, e) = SutLogNode ("Member: " ++ show id) [showSut e]


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
