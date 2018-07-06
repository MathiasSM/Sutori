{
module Parser where
import Lexer
import AST
import Data.Maybe
import OurMonad
}

%name      calc
%tokentype { Token }
%monad { OurMonad } 

%token
    EOF                 { Token $$ EOF               }
    PROGRAM_INI         { Token $$ PROGRAM_INI       }
    PROGRAM_FIN         { Token $$ PROGRAM_FIN       }
    FUNCTION_INI        { Token $$ FUNCTION_INI      }
    FUNCTION_FIN        { Token $$ FUNCTION_FIN      }
    S_Andthatswhere     { Token $$ S_Andthatswhere   }
    S_Therewas          { Token $$ S_Therewas        }
    S_brokea            { Token $$ S_brokea          }
    S_broughta          { Token $$ S_broughta        }
    S_comesfrom         { Token $$ S_comesfrom       }
    S_dreamsof          { Token $$ S_dreamsof        }
    S_keepsdreamingof   { Token $$ S_keepsdreamingof }
    S_madeof            { Token $$ S_madeof          }
    S_madea             { Token $$ S_madea           }
    Invented            { Token $$ Invented          }
    S_therewasa         { Token $$ S_therewasa       }
    S_toldthatstory     { Token $$ S_toldthatstory   }
    TYPE_INT            { Token $$ TYPE_INT          }
    TYPE_FLOAT          { Token $$ TYPE_FLOAT        }
    TYPE_CHAR           { Token $$ TYPE_CHAR         }
    TYPE_BOOL           { Token $$ TYPE_BOOL         }
    TYPE_ARRAY          { Token $$ TYPE_ARRAY        }
    TYPE_STRUCT         { Token $$ TYPE_STRUCT       }
    TYPE_UNION          { Token $$ TYPE_UNION        }
    TYPE_STRING         { Token $$ TYPE_STRING       }
    TYPE_POINTER        { Token $$ TYPE_POINTER      }
    WITH                { Token $$ WITH              }
    YOUR                { Token $$ YOUR              }
    OF                  { Token $$ OF                }
    EITHER              { Token $$ EITHER            }
    TO                  { Token $$ TO                }
    WHEN                { Token $$ WHEN              }
    OTHERWISE           { Token $$ OTHERWISE         }
    TIMES               { Token $$ TIMES             }
    TRUE                { Token $$ TRUE              }
    FALSE               { Token $$ FALSE             }
    BLOCK_OPEN          { Token $$ BLOCK_OPEN        }
    BLOCK_CLOSE         { Token $$ BLOCK_CLOSE       }
    '('                 { Token $$ OPEN_PAREN        }
    '['                 { Token $$ OPEN_BRACKETS     }
    '{'                 { Token $$ OPEN_BRACES       }
    ')'                 { Token $$ CLOSE_PAREN       }
    ']'                 { Token $$ CLOSE_BRACKETS    }
    '}'                 { Token $$ CLOSE_BRACES      }
    '.'                 { Token $$ PERIOD            }
    ','                 { Token $$ COMMA             }
    ':'                 { Token $$ COLON             }
    ';'                 { Token $$ SEMICOLON         }
    '!'                 { Token $$ EXCLAMATION       }
    '->'                { Token $$ ARROW_RIGHT       }
    '+'                 { Token $$ PLUS              }
    '-'                 { Token $$ MINUS             }
    '=='                { Token $$ EQUAL             }
    '='                 { Token $$ ASSIGNMENT        }
    '*'                 { Token $$ ASTERISK          }
    '%'                 { Token $$ PERCENT           }
    '/'                 { Token $$ SLASH             }
    div                 { Token $$ DIV               }
    '/='                { Token $$ NOT_EQUAL         }
    '>='                { Token $$ GREATER_EQUAL     }
    '<='                { Token $$ LESS_EQUAL        }
    '>'                 { Token $$ GREATER           }
    '<'                 { Token $$ LESS              }
    '^'                 { Token $$ POWER             }
    and                 { Token $$ AND               }
    or                  { Token $$ OR                }
    LITERAL_CHAR        { Token _ (Character $$)    }
    LITERAL_FLOAT       { Token _ (Float' $$)       }
    LITERAL_INT         { Token _ (Integer' $$)     }
    LITERAL_STRING      { Token _ (String' $$)      }
    ID_FUNCTION         { Token _ (FunctionID $$)   }
    ID                  { Token _ (ID $$)           }


%right IND
%right '='
%left or
%left and
%left '==' '/='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/' '%' div
%left '^'
%right '!' POS NEG
%left '['
%left '->' 

%% 

-- Program
-----------------------------------------------------------------------------------------------------------------------
Source                  : PROGRAM_INI ID Block PROGRAM_FIN EOF                                   { % addInitialTypes >> return (PT $2 $3) }

-- Expressions
----------------------------------------------------------------------------------------------------------------------
Expression               : LeftSide                                                             { $1 }
                         | Literal                                                              { LitT $1 }
                         | Constructor                                                          { ConsT $1 }
                         | Operation                                                            { OpT $1 }
                         | FunctionCall                                                         { FunCT $1 }
                         | '(' Expression ')'                                                   { $2 }


Literal                  : LITERAL_INT                                                           { IT $1 }
                         | LITERAL_CHAR                                                          { CT $1 }
                         | LITERAL_FLOAT                                                         { FT $1 }
                         | LITERAL_STRING                                                        { ST $1 }
                         | TRUE                                                                  { TBT   }
                         | FALSE                                                                 { FBT   }


-- Constructors
-----------------------------------------------------------------------------------------------------------------------
Constructor              : ConstructorArray                                                      { $1 }
                         | ConstructorStruct                                                     { $1 }

ConstructorArray         : '[' ConstructorArrayList ']'                                          { $2 }
ConstructorArrayList     : Expression                                                            { LCAT [ $1 ] }
                         | Expression ';' ConstructorArrayList                                   { LCAT $ $1: listLCAT $3 }

ConstructorStruct        : '{' ConstructorStructList '}'                                         { $2 }
ConstructorStructList    : ID ':' Expression                                                     { LCST [ ($1,$3) ] }
                         | ID ':' Expression ';' ConstructorStructList                           { LCST $ ($1,$3): listLCST $5 }


-- Operators
-----------------------------------------------------------------------------------------------------------------------
Operation                : UnaryOperation                                                                       { $1 }
                         | BinaryOperation                                                                      { $1 }

UnaryOperation           : NumericalUnaryOperation                                                              { $1 }
                         | LogicalUnaryOperation                                                                { $1 }

BinaryOperation          : NumericalBinaryOperation                                                             { $1 }
                         | LogicalBinaryOperation                                                               { $1 }
                         | Assignment                                                                           { $1 }

NumericalUnaryOperation  : '+' Expression %prec POS           { % getNumericType "+" $2 $2 $1 >>= (\x -> return (PUT $2 x)) }
                         | '-' Expression %prec NEG           { % getNumericType "-" $2 $2 $1 >>= (\x -> return (MUT $2 x)) }

LogicalUnaryOperation    : '!' Expression                     { % getLogicalType "!" $2 $2 $1 >>= (\x -> return (NUT $2 x)) }


NumericalBinaryOperation : Expression '+' Expression          { % getNumericType "+" $1 $3 $2 >>= (\x -> return (ABT $1 "+" $3 x ) ) }
                         | Expression '-' Expression          { % getNumericType "-" $1 $3 $2 >>= (\x -> return (ABT $1 "-" $3 x ) ) }
                         | Expression '*' Expression          { % getNumericType "*" $1 $3 $2 >>= (\x -> return (ABT $1 "*" $3 x ) ) }
                         | Expression '/' Expression          { % getNumericType "/" $1 $3 $2 >>= (\x -> return (ABT $1 "/" $3 x ) ) }
                         | Expression div Expression          { % getNumericType "div" $1 $3 $2 >>= (\x -> return (ABT $1 "div" $3  x )) }
                         | Expression '%' Expression          { % getNumericType "%" $1 $3 $2 >>= (\x -> return (ABT $1 "%" $3 x )) }
                         | Expression '^' Expression          { % getNumericType "^" $1 $3 $2 >>= (\x -> return (ABT $1 "^" $3 x )) }

LogicalBinaryOperation   : Expression and  Expression         { % getLogicalType "and" $1 $3 $2 >>= (\x -> return (LBT $1 "and" $3 x )) }
                         | Expression or   Expression         { % getLogicalType "or" $1 $3 $2 >>= (\x -> return (LBT $1 "or" $3 x )) }
                         | Expression '==' Expression         { % getEqualityType "==" $1 $3 >>= (\x -> return (LBT $1 "==" $3 x )) }
                         | Expression '/=' Expression         { % getEqualityType "/=" $1 $3 >>= (\x -> return (LBT $1 "/=" $3 x )) }
                         | Expression '>=' Expression         { % getComparisonType ">=" $1 $3 >>= (\x -> return (LBT $1 ">=" $3 x )) }
                         | Expression '<=' Expression         { % getComparisonType "<=" $1 $3 >>= (\x -> return (LBT $1 "<=" $3 x )) }
                         | Expression '>'  Expression         { % getComparisonType ">" $1 $3 >>= (\x -> return (LBT $1 ">" $3 x )) }
                         | Expression '<'  Expression         { % getComparisonType "<" $1 $3 >>= (\x -> return (LBT $1 "<" $3 x )) }


Dereference              : '*' Expression %prec IND           { % getPointerType $2 >>= (\x -> return (DUT $2 x)) }

GetArrayItem             : LeftSide '[' Expression ']'              { % checkIndexType $3 >> extArrayType $1 >>= (\x -> return (GAT $1 $3 x)) }

GetProp                  : LeftSide '->' ID                 { GPT $1 $3 }

Assignment               : LeftSide '=' Expression              { % getEqualityType "=" $1 $3 >>= (\x -> return (AT $1 $3 (getExpressionType $1))) }

LeftSide                 : ID                                   { % checkId $1 Var Nothing >>= (\s -> return (IdT $1 (fromJust (getType' s)))) }
                         | GetArrayItem                         { OpT $1 }
                         | GetProp                              { OpT $1 }
                         | Dereference                        { OpT $1 }

FunctionCall             : ID_FUNCTION                                         { % checkId $1 Function Nothing >>= (\s -> return (FCAT $1 (fromJust (getType' s)))) }
                         | ID_FUNCTION '(' WITH FunctionActualParams ')'       { % checkId $1 Function (Just $2) >>= (\s -> checkParams $4 s  >> return (FCNT $1 $4 (fromJust (getType' s)))) }

FunctionActualParams     : Expression                                                         { LFCP [$1] }
                         | Expression ',' FunctionActualParams                                { LFCP $ $1: listLFCP $3 }  


-- Declaration
-----------------------------------------------------------------------------------------------------------------------
Declaration              : PersonDeclaration                                                             { % addToSymTablePerson PDT $1}
                         | FunctionDeclaration                                                           { % return $1 }
                         | VariableDeclaration                                                           { % return $1 }
                         | TypeDeclaration                                                               { % return $1 }


PersonDeclaration        : S_Therewas PersonNames                                                        { $2 }

PersonNames              : ID                                                                            { LPD [$1] }
                         | ID ',' PersonNames                                                            { LPD $ $1: listLPD $3 }
                         | ID and PersonNames                                                            { LPD $ $1: listLPD $3 }

FunctionDeclaration      :: { Declaration }
FunctionDeclaration      : FUNCTION_INI IdentificadorFun ',' S_therewasa Type FunctionBlock FUNCTION_FIN 
                            { % removeLastScope >> checkType $5 $4 >> modifyFunction $2 $6 (LFDP []) $5 >> return FDT }
                         | FUNCTION_INI IdentificadorFun ',' S_therewasa Type '(' S_madeof StackParams ')' FunctionBlock FUNCTION_FIN 
                            { % removeLastScope >> checkType $5 $6 >> modifyFunction $2 $10 $8 $5 >> return FDAT }

IdentificadorFun : ID_FUNCTION                                                                           { % addFuncToSymTable $1 }

StackParams              : FunctionFormalParams                                                          {% addParamsFuncToSymTable $1 }

FunctionFormalParams     : Type ID                                                                       { LFDP [($1,$2,0)] }
                         | Type ID ',' FunctionFormalParams                                              { LFDP $ ($1,$2,0): listLFDP $4 }
                         | YOUR Type ID                                                                  { LFDP [($2,$3,1)] }
                         | YOUR Type ID ',' FunctionFormalParams                                         { LFDP $ ($2,$3,1): listLFDP $5 }  


VariableDeclaration      : ID S_broughta Type ':' VariableList  { % checkId $1 Person (Just $2) >> checkType $3 $4 >> checkTypesExp $3 $5 >> addToSymTableVar VDT $3 $5 $4 }

VariableList             : ID ',' VariableList                                                         { LDV $ ($1,Nothing): listLDV $3 }
                         | ID '=' Expression ',' VariableList                                          { LDV $ ($1,Just $3): listLDV $5 }
                         | ID '=' Expression                                                           { LDV [($1,Just $3)] }
                         | ID                                                                          { LDV [($1,Nothing)] }

TypeDeclaration          : ID Invented ID                           { % checkId $1 Person (Just $2) >> addTypeToSymTable TDT $3 }

-- Types
-----------------------------------------------------------------------------------------------------------------------
Type                     : TYPE_INT                                                                             { TI }
                         | TYPE_FLOAT                                                                           { TF }
                         | TYPE_CHAR                                                                            { TC }
                         | TYPE_BOOL                                                                            { TB }
                         | TYPE_STRING                                                                          { TS }
                         | TYPE_ARRAY '(' OF LITERAL_INT Type ')'                                               { TA $4 $5 }
                         | TYPE_STRUCT '(' WITH StructTyping ')'                                                { TST $4 }
                         | TYPE_UNION '(' EITHER UnionTyping ')'                                                { TU $4 }
                         | TYPE_POINTER '(' TO Type ')'                                                         { TP $4 }
                         | ID                                                                                   { TID $1 }

StructTyping             : Type ID                                                                           { LSRT [($1,$2)] }
                         | Type ID and StructTyping                                                          { LSRT $ ($1,$2): listLSRT $4 }

UnionTyping              : Type ID                                                                              { LUT [($1,$2)] }
                         | Type ID or UnionTyping                                                               { LUT $ ($1,$2): listLUT $4 }


-- Blocks
-----------------------------------------------------------------------------------------------------------------------
Block                    : BLOCK_OPEN BlockContent BLOCK_CLOSE                                                  { $2 }

BlockContent             : Statement '.' BlockContent                                                           { LST $ $1: listLST $3 }
                         | {-empty-}                                                                            { LST [] }

FunctionBlock            : BLOCK_OPEN FunctionBlockContent BLOCK_CLOSE                                          { $2 }

FunctionBlockContent     : Statement '.' FunctionBlockContent                                                   { LFBT $ (StaT $1): listLFBT $3 }
                         | ReturnStatement '.' FunctionBlockContent                                             { LFBT $ $1: listLFBT $3 }
                         | {-empty-}                                                                            { LFBT [] }

Statement                : Instruction                                                                          { InsT $1 }
                         | Declaration                                                                          { DecT $1 }


ReturnStatement          : S_Andthatswhere Expression S_comesfrom                                               { RT $2 }


-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction              : Expression                                                                           { ExprT $1 }
                         | Selection                                                                            { $1 }
                         | UnboundedIteration                                                                   { $1 }
                         | BoundedIteration                                                                     { $1 }
                         | ManageMemory                                                                         { $1 }
                         | Print                                                                                { $1 }

ManageMemory             : CreatePointer                                                                        { $1 }
                         | FreePointer                                                                          { $1 }

CreatePointer            : ID S_madea Type                                  { % checkId $1 Var (Just $2) >> checkType $3 $2 >> return (CPT $1 $3) }
FreePointer              : ID S_brokea ID                                   { % checkId $1 Var (Just $2) >> checkId $3 Var (Just $2) >> return (FPT $1 $3) }

Selection                : IdToken S_dreamsof Block WHEN Expression                            { % getLogicalType "condition" $5 $5 $4 >> removeLastScope >> return (IFT $1 $3 $5) }
                         | IdToken S_dreamsof Block WHEN Expression ';' OtherwiseTok Block     { % getLogicalType "condition" $5 $5 $4 >> removeLastScope >> return (IFET $1 $3 $5 $8) }

OtherwiseTok             : OTHERWISE   { % removeLastScope >> addInstructionScope }


IdToken                  : ID                                              { % checkId $1 Person Nothing >> addInstructionScope >> return $1 }

UnboundedIteration       : IdToken S_keepsdreamingof Expression Block      { % getLogicalType "condition" $3 $3 $2 >> removeLastScope >> return (UIT $1 $3 $4) }

BoundedIteration         : AddScope Block ID S_toldthatstory Expression TIMES  { % checkIndexType $5 >> removeLastScope >> checkId $3 Person (Just $4) >> return (BIT $2 $3 $5) }

AddScope                 : {-empty-}                        { % addInstructionScope }

Print                    : ID ':' Expression                {  % checkId $1 Person (Just $2) >>  return (PRT $1 $3) }



{


happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
  lcn = case tks of
          []    -> "end of file"
          ((Token (AlexPn _ l c) _):_)  -> "line " ++ show l ++ ", column " ++ show c
}
