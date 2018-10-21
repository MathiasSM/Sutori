{-|
Description : Provides 'ShowSut' instances for "Sutori.AST"
-}
module Sutori.AST.Logger() where

import Sutori.Logger       (SutShow(showSut), SutLog(SutLogLeave, SutLogNode), fromLeave)
import Sutori.Types

import Sutori.AST.Nodes


-- |Modules can be printed nicely
instance SutShow SutModule where
  showSut (SutModule id b) = SutLogNode ("Module: " ++ show id) (map showSut b)


-- |Instructions of the AST can be printed nicely
instance SutShow SutInstruction where
  showSut (InstAssignment e)             = SutLogNode "Expression as instruction:" [showSut e]
  showSut (Selection who cond ifb elseb) = let condition = SutLogNode "Condition:" [showSut cond]
                                               ifblock   = SutLogNode "If Block:" (map showSut ifb)
                                               elseblock = SutLogNode "Else Block:" (map showSut elseb)
                                            in SutLogNode ("Selection by " ++ show who) [condition, ifblock, elseblock]
  showSut (IterationU who cond b)        = let condition = SutLogNode "Condition:" [showSut cond]
                                               block     = SutLogNode "Block:" (map showSut b)
                                            in SutLogNode ("Unbounded Iteration by " ++ show who) [condition, block]
  showSut (IterationB who cond b)        = let condition = SutLogNode "Condition:" [showSut cond]
                                               block     = SutLogNode "Block:" (map showSut b)
                                            in SutLogNode ("Bounded Iteration by " ++ show who) [condition, block]
  showSut (FreePointer who e)            = SutLogNode ("Pointer freed by " ++ show who ++ " from expr:") [showSut e]
  showSut (PrintVal who e)               = SutLogNode ("Printed value by " ++ show who ++ " from expr:") [showSut e]
  showSut (ReadVal who e)                = SutLogNode ("Read value by " ++ show who ++ " into:") [showSut e]
  showSut (ReturnVal e)                  = SutLogNode "Returned value:" [showSut e]


-- |Expressions of the AST can be printed nicely
instance SutShow SutExpression where
  showSut (ArrayGet t ae ie)     = let etype = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       array = SutLogNode "Array:" [showSut ae]
                                       index = SutLogNode "Index:" [showSut ie]
                                    in SutLogNode "ArrayIndexation" [etype, array, index]
  showSut (BinaryOp t op e1 e2)  = let etype    = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       operator = showSut op
                                       loperand = SutLogNode "Left Operand:" [showSut e1]
                                       roperand = SutLogNode "Right Operand:" [showSut e2]
                                    in SutLogNode "BinaryOperation" [etype, operator, loperand, roperand]
  showSut (SutCall t id es)      = let etype = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       args  = SutLogNode "Arguments:" (map showSut es)
                                    in SutLogNode ("Function call to `" ++ show id ++ "` :") [etype, args]
  showSut (CreatePointer t p)    = let etype = SutLogLeave $ "Type:  " ++ fromLeave (showSut t)
                                       expr  = SutLogLeave $ "Owner: " ++ show p
                                       in SutLogNode "NewPointer" [etype, expr]
  showSut (ExprConstructor t c)  = let etype  = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       constr = SutLogNode "Constructor:" [showSut c]
                                       in SutLogNode "DataStructure" [etype, constr]
  showSut (ExprID t id)          = let etype = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       in SutLogNode ("ID Expression : " ++ show id) [etype]
  showSut (ExprLiteral t l)      = showSut l
  showSut (MemberGet t se id)    = let etype = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       struct = SutLogNode "Struct:" [showSut se]
                                    in SutLogNode ("Acces to member: " ++ show id) [etype, struct]
  showSut (UnaryOp t op e)       = let etype = SutLogLeave $ "Type: " ++ fromLeave (showSut t)
                                       operator = showSut op
                                       expr = SutLogNode "Operand: " [showSut e]
                                    in SutLogNode "UnaryOperation" [etype, operator, expr]


-- |Literals can be printed nicely
instance SutShow SutLiteral where
  showSut (SutString s)  = SutLogLeave $ "Literal: Phrase(" ++ show s ++ ")"
  showSut (SutInt i)     = SutLogLeave $ "Literal: Bag(" ++ show i ++")"
  showSut (SutFloat f)   = SutLogLeave $ "Literal: Waller(" ++ show f ++")"
  showSut (SutChar c)    = SutLogLeave $ "Literal: Letter(" ++ show c ++")"
  showSut (SutBool b)    = SutLogLeave $ "Literal: Light(" ++ show b ++")"

-- |Constructs can be printed nicely
instance SutShow SutConstructor where
  showSut (SutArray es)  = SutLogNode "Chain:" (map showSut es)
  showSut (SutStruct es) = SutLogNode "Machine:" (map showMember es)

showMember (id, e) = SutLogNode ("Member: " ++ show id) [showSut e]

-- |Operators can be printed nicely
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
