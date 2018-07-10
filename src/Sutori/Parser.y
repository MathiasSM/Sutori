{
-- # PRE CODE (Module and imports)
module Sutori.Parser where

import Data.Maybe
import Sutori.Lexer
import Sutori.AST
import Sutori.Monad
import Sutori.Types

}

%name      parse
%tokentype { SutToken }
%monad     { SutMonad }

%token
    EOF                 { SutToken $$ SutTkEOF          }

    BLOCK_OPEN          { SutToken $$ BLOCK_OPEN        }
    BLOCK_CLOSE         { SutToken $$ BLOCK_CLOSE       }

    PROGRAM_INI         { SutToken $$ PROGRAM_INI       }
    PROGRAM_FIN         { SutToken $$ PROGRAM_FIN       }
    FUNCTION_INI        { SutToken $$ FUNCTION_INI      }
    FUNCTION_FIN        { SutToken $$ FUNCTION_FIN      }

    S_andthatswhere     { SutToken $$ S_andthatswhere   }
    S_therewas          { SutToken $$ S_therewas        }
    S_brokea            { SutToken $$ S_brokea          }
    S_broughta          { SutToken $$ S_broughta        }
    S_comesfrom         { SutToken $$ S_comesfrom       }
    S_dreamsof          { SutToken $$ S_dreamsof        }
    S_keepsdreamingof   { SutToken $$ S_keepsdreamingof }
    S_madeof            { SutToken $$ S_madeof          }
    S_madea             { SutToken $$ S_madea           }
    S_invented          { SutToken $$ S_invented        }
    S_therewasa         { SutToken $$ S_therewasa       }
    S_toldthatstory     { SutToken $$ S_toldthatstory   }

    TYPE_INT            { SutToken $$ TYPE_INT          }
    TYPE_FLOAT          { SutToken $$ TYPE_FLOAT        }
    TYPE_CHAR           { SutToken $$ TYPE_CHAR         }
    TYPE_BOOL           { SutToken $$ TYPE_BOOL         }
    TYPE_ARRAY          { SutToken $$ TYPE_ARRAY        }
    TYPE_STRUCT         { SutToken $$ TYPE_STRUCT       }
    TYPE_UNION          { SutToken $$ TYPE_UNION        }
    TYPE_STRING         { SutToken $$ TYPE_STRING       }
    TYPE_POINTER        { SutToken $$ TYPE_POINTER      }

    '('                 { SutToken $$ OPEN_PAREN        }
    '['                 { SutToken $$ OPEN_BRACKETS     }
    '{'                 { SutToken $$ OPEN_BRACES       }
    ')'                 { SutToken $$ CLOSE_PAREN       }
    ']'                 { SutToken $$ CLOSE_BRACKETS    }
    '}'                 { SutToken $$ CLOSE_BRACES      }
    '.'                 { SutToken $$ PERIOD            }
    ','                 { SutToken $$ COMMA             }
    ':'                 { SutToken $$ COLON             }
    ';'                 { SutToken $$ SEMICOLON         }
    '!'                 { SutToken $$ EXCLAMATION       }
    '->'                { SutToken $$ ARROW_RIGHT       }
    '+'                 { SutToken $$ PLUS              }
    '-'                 { SutToken $$ MINUS             }
    '=='                { SutToken $$ EQUAL             }
    '='                 { SutToken $$ ASSIGNMENT        }
    '*'                 { SutToken $$ ASTERISK          }
    '%'                 { SutToken $$ PERCENT           }
    '/'                 { SutToken $$ SLASH             }
    div                 { SutToken $$ DIV               }
    '/='                { SutToken $$ NOT_EQUAL         }
    '>='                { SutToken $$ GREATER_EQUAL     }
    '<='                { SutToken $$ LESS_EQUAL        }
    '>'                 { SutToken $$ GREATER           }
    '<'                 { SutToken $$ LESS              }
    '^'                 { SutToken $$ POWER             }
    and                 { SutToken $$ AND               }
    or                  { SutToken $$ OR                }
    WITH                { SutToken $$ WITH              }
    YOUR                { SutToken $$ YOUR              }
    OF                  { SutToken $$ OF                }
    EITHER              { SutToken $$ EITHER            }
    TO                  { SutToken $$ TO                }
    WHEN                { SutToken $$ WHEN              }
    OTHERWISE           { SutToken $$ OTHERWISE         }
    TIMES               { SutToken $$ TIMES             }

    LITERAL_BOOL        { SutToken _ (SutTkBool $$)       }
    LITERAL_CHAR        { SutToken _ (SutTkChar $$)       }
    LITERAL_FLOAT       { SutToken _ (SutTkFloat $$)      }
    LITERAL_INT         { SutToken _ (SutTkInt $$)        }
    LITERAL_STRING      { SutToken _ (SutTkString $$)     }
    ID                  { SutToken _ (SutTkId $$)         }


%right    '='
%left     or
%left     and
%left     '==' '/='
%nonassoc '>'  '<'  '>=' '<='
%left     '+'  '-'
%left     '*'  '/'  '%'  div
%left     '^'
%right    '!'  IND  POS  NEG
%left     '['
%left     '->'

%%

-- Program
-----------------------------------------------------------------------------------------------------------------------
Source                   : PROGRAM_INI ID Block PROGRAM_FIN EOF  { % addInitialTypes >> return (SutModule $2 $3) }

-- Expressions
-- ====================================================================================================================
Expression               : LeftSide           { $1 }
                         | Literal            { SutExprLiteral $1 }
                         | Constructor        { SutExprConstructor $1 }
                         | UnaryOperation     { $1 }
                         | BinaryOperation    { $1 }
                         | FunctionCall       { SutCall $1 }
                         | '(' Expression ')' { $2 }


Literal                  : LITERAL_INT        { SutLitInt $1 }
                         | LITERAL_CHAR       { SutLitChar $1 }
                         | LITERAL_FLOAT      { SutLitFloat $1 }
                         | LITERAL_STRING     { SutLitString $1 }
                         | LITERAL_BOOL       { SutLitBool $1 }


-- Constructors
-----------------------------------------------------------------------------------------------------------------------
Constructor              : ConstructorArray                            { $1 }
                         | ConstructorStruct                           { $1 }

ConstructorArray         : '[' ConstructorArrayList ']'                { $2 }
ConstructorArrayList     : Expression                                  { [ $1 ] }
                         | Expression ';' ConstructorArrayList         { $1:$3 }

ConstructorStruct        : '{' ConstructorStructList '}'               { $2 }
ConstructorStructList    : ID ':' Expression                           { [ ($1,$3) ] }
                         | ID ':' Expression ';' ConstructorStructList { ($1,$3): $5 }


-- Operators
-----------------------------------------------------------------------------------------------------------------------
Operation                : UnaryOperation             { $1 }
                         | BinaryOperation            { $1 }

UnaryOperation           : NumericalUnaryOperation    { $1 }
                         | LogicalUnaryOperation      { $1 }

BinaryOperation          : NumericalBinaryOperation   { $1 }
                         | LogicalBinaryOperation     { $1 }
                         | Assignment                 { $1 }

NumericalUnaryOperation  : '+' Expression %prec POS   { % getNumericType SutOpPos $2 $2 >>= (\t -> return (SutUnaryOp t SutOpPos $2)) }
                         | '-' Expression %prec NEG   { % getNumericType SutOpNeg $2 $2 >>= (\t -> return (SutUnaryOp t SutOpNeg $2)) }

LogicalUnaryOperation    : '!' Expression             { % getLogicalType SutOpNot $2 $2 >>= (\t -> return (SutUnaryOp t SutOpNot $2)) }


NumericalBinaryOperation : Expression '+' Expression  { % getNumericType SutOpAdd $1 $3 >>= (\t -> return (SutBinaryOp t SutOpAdd $1 $3) ) }
                         | Expression '-' Expression  { % getNumericType SutOpSub $1 $3 >>= (\t -> return (SutBinaryOp t SutOpSub $1 $3) ) }
                         | Expression '*' Expression  { % getNumericType SutOpMul $1 $3 >>= (\t -> return (SutBinaryOp t SutOpMul $1 $3) ) }
                         | Expression '/' Expression  { % getNumericType SutOpIntDiv $1 $3 >>= (\t -> return (SutBinaryOp t SutOpIntDiv $3) ) }
                         | Expression div Expression  { % getNumericType SutOpDiv $1 $3 >>= (\t -> return (SutBinaryOp t SutOpDiv $1 $3)) }
                         | Expression '%' Expression  { % getNumericType SutOpMod $1 $3 >>= (\t -> return (SutBinaryOp t SutOpMod $1 $3)) }
                         | Expression '^' Expression  { % getNumericType SutOpPow $1 $3 >>= (\t -> return (SutBinaryOp t SutOpPow $1 $3)) }

LogicalBinaryOperation   : Expression and  Expression { % getLogicalType SutOpAnd $1 $3 >>= (\t -> return (SutBinaryOp t SutOpAnd $1 $3)) }
                         | Expression or   Expression { % getLogicalType SutOpOr $1 $3 >>= (\t -> return (SutBinaryOp t SutOpOr $1 $3)) }
                         | Expression '==' Expression { % getEqualityType SutOpEqual $1 $3 >>= (\t -> return (SutBinaryOp t SutOpEqual $1 $3)) }
                         | Expression '/=' Expression { % getEqualityType SutOpNotEqual $1 $3 >>= (\t -> return (SutBinaryOp t SutOpNotEqual $1 $3)) }
                         | Expression '>=' Expression { % getComparisonType SutOpGEqual $1 $3 >>= (\t -> return (SutBinaryOp t SutOpGEqual $1 $3)) }
                         | Expression '<=' Expression { % getComparisonType SutOpLEqual $1 $3 >>= (\t -> return (SutBinaryOp t SutOpLEqual $1 $3)) }
                         | Expression '>'  Expression { % getComparisonType SutOpGreater $1 $3 >>= (\t -> return (SutBinaryOp t SutOpGreater $1 $3)) }
                         | Expression '<'  Expression { % getComparisonType SutOpLess $1 $3 >>= (\t -> return (SutBinaryOp t SutOpLess $1 $3)) }


--Dereference              : '*' Expression %prec IND           { % getPointerType $2 >>= (\x -> return (DUT $2 x)) }

GetArrayItem             : LeftSide '[' Expression ']' { % checkIndexType $3 >> extArrayType $1 >>= (\t -> return (SutArrayItem t $1 $3)) }

GetProp                  : LeftSide '->' ID            { SutStructMember t $1 $3 }

Assignment               : LeftSide '=' Expression     { % getEqualityType SutOpAssign $1 $3 >>= (\t -> return (SutBinaryOp SutOpAssign (getExpressionType $1) $1 $3)) }

LeftSide                 : ID                          { % checkId $1 Var >>= (\s -> return (SutExprID (fromJust (getType' s)) $1)) }
                         | GetArrayItem                { $1 }
                         | GetProp                     { $1 }
--                         | Dereference                        { OpT $1 }

FunctionCall             : ID                                   { % checkId $1 Function >>= (\s -> return (SutCall (fromJust (getType' s)) $1 [])) }
                         | ID '(' WITH FunctionActualParams ')' { % checkId $1 Function >>= (\s -> checkParams $4 s  >> return (SutCall (fromJust (getType' s)) $1 $4)) }

FunctionActualParams     : Expression                           { [$1] }
                         | Expression ',' FunctionActualParams  { $1:$3 }




-- Declaration
-- =====================================================================================================================
Declaration              : PersonDeclaration                     { % addToSymTablePerson $1}
                         | FunctionDeclaration                   { $1 }
                         | VariableDeclaration                   { $1 }
                         | TypeDeclaration                       { $1 }


PersonDeclaration        : S_therewas PersonNames                { $2 }

PersonNames              : ID                                    { [$1] }
                         | ID ',' PersonNames                    { $1:$3 }
                         | ID and PersonNames                    { $1:$3 }

FunctionDeclaration      : FUNCTION_INI IdentificadorFun ',' S_therewasa Type FunctionBlock FUNCTION_FIN
                           { % checkType $5 >> modifyFunction $2 $6 (LFDP []) $5 >> return () }

                         | FUNCTION_INI IdentificadorFun ',' S_therewasa Type '(' S_madeof StackParams ')' FunctionBlock FUNCTION_FIN
                           { % checkType $5 >> modifyFunction $2 $10 $8 $5 >> return () }

IdentificadorFun         : ID                                    { % addFuncToSymTable $1 }

StackParams              : FunctionFormalParams                  { % addParamsFuncToSymTable $1 }

FunctionFormalParams     : Type ID                               { [($1,$2,0)] }
                         | Type ID ',' FunctionFormalParams      { ($1,$2,0):$4 }
                         | YOUR Type ID                          { [($2,$3,1)] }
                         | YOUR Type ID ',' FunctionFormalParams { ($2,$3,1):$5 }


VariableDeclaration      : ID S_broughta Type ':' VariableList   { % checkId $1 Person >> checkType $3 >> addToSymTableVar $3 $5 }

VariableList             : ID ',' VariableList                   { ($1,Nothing):$3 }
                         | ID '=' Expression ',' VariableList    { ($1,Just $3):$5 }
                         | ID '=' Expression                     { [($1,Just $3)] }
                         | ID                                    { [($1,Nothing)] }

TypeDeclaration          : ID S_invented ID                      { % checkId $1 Person >> addTypeToSymTable TDT $3 }



-- Types
-- ====================================================================================================================
Type                     : TYPE_INT                                    { SutInt }
                         | TYPE_FLOAT                                  { SutFloat }
                         | TYPE_CHAR                                   { SutChar }
                         | TYPE_BOOL                                   { SutBool }
                         | TYPE_STRING                                 { SutString }
                         | TYPE_ARRAY '(' OF LITERAL_INT Type ')'      { SutArray $4 $5 }
                         | TYPE_STRUCT '(' WITH StructTyping ')'       { SutStruct $4 }
                         | TYPE_UNION '(' EITHER UnionTyping ')'       { SutUnion $4 }
                         | TYPE_POINTER '(' TO Type ')'                { SutPointer $4 }
                         | ID                                          { % getNamedType $1 }

StructTyping             : Type ID                                     { [($1,$2)] }
                         | Type ID and StructTyping                    { ($1,$2):$4 }

UnionTyping              : Type ID                                     { [($1,$2)] }
                         | Type ID or UnionTyping                      { ($1,$2):$4 }


-- Blocks
-- ====================================================================================================================
AddScope                 : {-empty-}                                   { % addInstructionScope }
RemoveScope              : {-empty-}                                   { % removeLastScope }

Block                    : BLOCK_OPEN AddScope BlockContent RemoveScope BLOCK_CLOSE         { $3 }

BlockContent             : Statement BlockContent                      { $1: $2 }
                         | {-empty-}                                   { [] }

FunctionBlock            : BLOCK_OPEN AddScope FunctionBlockContent RemoveScope BLOCK_CLOSE { $3 }

FunctionBlockContent     : Statement FunctionBlockContent              { $1:$2 }
                         | ReturnStatement FunctionBlockContent        { $1:$2 }
                         | {-empty-}                                   { [] }

Statement                : Instruction                                 { $1 }
                         | Declaration                                 { $1 }


ReturnStatement          : S_andthatswhere Expression S_comesfrom      { SutReturn $2 }


-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction              : Expression '.'                                                  { SutInstExpression $1 }
                         | Selection                                                       { $1 }
                         | UnboundedIteration                                              { $1 }
                         | BoundedIteration                                                { $1 }
                         | ManageMemory '.'                                                { $1 }
                         | Print '.'                                                       { $1 }

ManageMemory             : CreatePointer                                                   { $1 }
                         | FreePointer                                                     { $1 }

CreatePointer            : ID S_madea Type                                                 { % checkId $1 Var >> checkType $3 >> return (SutCreatePointer $1 $3) }
FreePointer              : ID S_brokea ID                                                  { % checkId $1 Var >> checkId $3 Var >> return (SutFreePointer $1 $3) }

Selection                : IdToken S_dreamsof Block WHEN Expression                        { % getLogicalType "condition" $5 $5 >> return (SutSelection $1 $3 $5 []) }
                         | IdToken S_dreamsof Block WHEN Expression ';' OtherwiseTok Block { % getLogicalType "condition" $5 $5 >> return (SutSelection $1 $3 $5 $8) }

OtherwiseTok             : OTHERWISE                                                       { % addInstructionScope }


IdToken                  : ID                                                              { % checkId $1 Person >> addInstructionScope >> return $1 }

UnboundedIteration       : IdToken S_keepsdreamingof Expression Block      { % getLogicalType "condition" $3 $3 >> return (SutIterationU $1 $3 $4) }

BoundedIteration         : Block ID S_toldthatstory Expression TIMES       { % checkIndexType $5 >> checkId $3 Person >> return (SutIterationB $2 $3 $5) }


Print                    : ID ':' Expression                                               { % checkId $1 Person >>  return (SutPrintVal $1 $3) }
-- Read                     : LeftSide '?'                                                  { % checkId $1 Person >>  return (SutPrintVal $1 $3) }



{


happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
    lcn = case tks of
            []    -> "end of file"
            ((SutToken (AlexPn _ l c) _):_)  -> "line " ++ show l ++ ", column " ++ show c

}
