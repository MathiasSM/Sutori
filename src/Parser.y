{
module Parser where

import Lexer
import AST
}

%name      calc
%tokentype { Token }

%token
    EOF                 { Token _ EOF               }
    PROGRAM_INI         { Token _ PROGRAM_INI       }
    PROGRAM_FIN         { Token _ PROGRAM_FIN       }
    FUNCTION_INI        { Token _ FUNCTION_INI      }
    FUNCTION_FIN        { Token _ FUNCTION_FIN      }
    S_Andthatswhere     { Token _ S_Andthatswhere   }
    S_Therewas          { Token _ S_Therewas        }
    S_brokea            { Token _ S_brokea          }
    S_broughta          { Token _ S_broughta        }
    S_comesfrom         { Token _ S_comesfrom       }
    S_dreamsof          { Token _ S_dreamsof        }
    S_keepsdreamingof   { Token _ S_keepsdreamingof }
    S_madeof            { Token _ S_madeof          }
    S_madea             { Token _ S_madea           }
    S_therewasa         { Token _ S_therewasa       }
    S_toldthatstory     { Token _ S_toldthatstory   }
    TYPE_INT            { Token _ TYPE_INT          }
    TYPE_FLOAT          { Token _ TYPE_FLOAT        }
    TYPE_CHAR           { Token _ TYPE_CHAR         }
    TYPE_BOOL           { Token _ TYPE_BOOL         }
    TYPE_ARRAY          { Token _ TYPE_ARRAY        }
    TYPE_STRUCT         { Token _ TYPE_STRUCT       }
    TYPE_UNION          { Token _ TYPE_UNION        }
    TYPE_STRING         { Token _ TYPE_STRING       }
    TYPE_POINTER        { Token _ TYPE_POINTER      }
    WITH                { Token _ WITH              }
    YOUR                { Token _ YOUR              }
    OF                  { Token _ OF                }
    EITHER              { Token _ EITHER            }
    TO                  { Token _ TO                }
    WHEN                { Token _ WHEN              }
    OTHERWISE           { Token _ OTHERWISE         }
    TIMES               { Token _ TIMES             }
    TRUE                { Token _ TRUE              }
    FALSE               { Token _ FALSE             }
    BLOCK_OPEN          { Token _ BLOCK_OPEN        }
    BLOCK_CLOSE         { Token _ BLOCK_CLOSE       }
    '('                 { Token _ OPEN_PAREN        }
    '['                 { Token _ OPEN_BRACKETS     }
    '{'                 { Token _ OPEN_BRACES       }
    ')'                 { Token _ CLOSE_PAREN       }
    ']'                 { Token _ CLOSE_BRACKETS    }
    '}'                 { Token _ CLOSE_BRACES      }
    '.'                 { Token _ PERIOD            }
    ','                 { Token _ COMMA             }
    ':'                 { Token _ COLON             }
    ';'                 { Token _ SEMICOLON         }
    '!'                 { Token _ EXCLAMATION       }
    '->'                { Token _ ARROW_RIGHT       }
    '+'                 { Token _ PLUS              }
    '-'                 { Token _ MINUS             }
    '=='                { Token _ EQUAL             }
    '='                 { Token _ ASSIGNMENT        }
    '*'                 { Token _ ASTERISK          }
    '%'                 { Token _ PERCENT           }
    '/'                 { Token _ SLASH             }
    div                 { Token _ DIV               }
    '/='                { Token _ NOT_EQUAL         }
    '>='                { Token _ GREATER_EQUAL     }
    '<='                { Token _ LESS_EQUAL        }
    '>'                 { Token _ GREATER           }
    '<'                 { Token _ LESS              }
    '^'                 { Token _ POWER             }
    and                 { Token _ AND               }
    or                  { Token _ OR                }
    LITERAL_CHAR        { Token _ (Character $$)    }
    LITERAL_FLOAT       { Token _ (Float' $$)       }
    LITERAL_INT         { Token _ (Integer' $$)     }
    LITERAL_STRING      { Token _ (String' $$)      }
    ID_FUNCTION         { Token _ (FunctionID $$)   }
    ID                  { Token _ (ID $$)           }


%right '='
%left or
%left and
%left '==' '/='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/' '%' div
%left '^'
%right '!' IND POS NEG
%left '->'


%%

-- Program
-----------------------------------------------------------------------------------------------------------------------
Source                  : PROGRAM_INI ID Block PROGRAM_FIN EOF                      { PT $2 $3 }

-- Expressions
----------------------------------------------------------------------------------------------------------------------
Expression               : ID                                                       { IdT $1 }
                         | Literal                                                  { LitT $1 }
                         | Constructor                                              { ConsT $1 }
                         | Operation                                                { OpT $1 }
                         | FunctionCall                                             { FunCT $1 }
                         | '(' Expression ')'                                       { PET $2 }

Literal                  : LITERAL_INT                                              { IT $1 }
                         | LITERAL_CHAR                                             { CT $1 }
                         | LITERAL_FLOAT                                            { FT $1 }
                         | LITERAL_STRING                                           { ST $1 }
                         | TRUE                                                     { TBT }
                         | FALSE                                                    { FBT }


-- Constructors
-----------------------------------------------------------------------------------------------------------------------
Constructor              : ConstructorArray                                         { $1 }
                         | ConstructorStruct                                        { $1 }

ConstructorArray         : '[' ConstructorArrayList ']'                             { $2 }
ConstructorArrayList     : Expression                                               { LCAT [ $1 ] }
                         | Expression ';' ConstructorArrayList                      { LCAT $ $1: listLCAT $3 }

ConstructorStruct        : '{' ConstructorStructList '}'                            { $2 }
ConstructorStructList    : ID ':' Expression                                        { LCST [ ($1,$3) ] }
                         | ID ':' Expression ';' ConstructorStructList              { LCST $ ($1,$3): listLCST $5 }


-- Operators
-----------------------------------------------------------------------------------------------------------------------
Operation                : UnaryOperation                                           { $1 }
                         | BinaryOperation                                          { $1 }

UnaryOperation           : NumericalUnaryOperation                                  { $1 }
                         | LogicalUnaryOperation                                    { $1 }
                         | Dereference                                              { $1 }

BinaryOperation          : NumericalBinaryOperation                                 { $1 }
                         | LogicalBinaryOperation                                   { $1 }
                         | GetProp                                                  { $1 }
                         | GetArrayItem                                             { $1 }
                         | Assignment                                               { $1 }

NumericalUnaryOperation  : '+' Expression %prec POS                                 { PUT $2 }
                         | '-' Expression %prec NEG                                 { MUT $2 }

LogicalUnaryOperation    : '!' Expression                                           { NUT $2 }

Dereference              : '*' Expression %prec IND                                 { DUT $2 }

NumericalBinaryOperation : Expression '+' Expression                                { ABT $1 "+" $3 }
                         | Expression '-' Expression                                { ABT $1 "-" $3 }
                         | Expression '*' Expression                                { ABT $1 "*" $3 }
                         | Expression '/' Expression                                { ABT $1 "/" $3 }
                         | Expression div Expression                                { ABT $1 "div" $3 }
                         | Expression '%' Expression                                { ABT $1 "%" $3 }
                         | Expression '^' Expression                                { ABT $1 "^" $3 }

LogicalBinaryOperation   : Expression and  Expression                               { LBT $1 "and" $3 }
                         | Expression or   Expression                               { LBT $1 "or" $3 }
                         | Expression '==' Expression                               { LBT $1 "==" $3 }
                         | Expression '/=' Expression                               { LBT $1 "/=" $3 }
                         | Expression '>=' Expression                               { LBT $1 ">=" $3 }
                         | Expression '<=' Expression                               { LBT $1 "<=" $3 }
                         | Expression '>'  Expression                               { LBT $1 ">" $3 }
                         | Expression '<'  Expression                               { LBT $1 "<" $3 }

GetArrayItem             : ID '[' Expression ']'                                    { GAT $1 $3 }

GetProp                  : Expression '->' ID                                       { GPT $1 $3 }

Assignment               : ID '=' Expression                                        { AT $1 $3 }

FunctionCall             : ID_FUNCTION                                              { FCAT $1 }
                         | ID_FUNCTION '(' WITH FunctionActualParams ')'            { FCNT $1 $4 }

FunctionActualParams     : Expression                                               { LFCP [$1] }
                         | Expression ',' FunctionActualParams                      { LFCP $ $1: listLFCP $3 }


-- Declaration
-----------------------------------------------------------------------------------------------------------------------
Declaration              : PersonDeclaration                                        { PDT $1 }
                         | FunctionDeclaration                                      { $1 }
                         | VariableDeclaration                                      { $1 }

PersonDeclaration        : S_Therewas PersonNames                                   { $2 }

PersonNames              : ID                                                       { LPD [$1] }
                         | ID ',' PersonNames                                       { LPD $ $1: listLPD $3 }
                         | ID and PersonNames                                       { LPD $ $1: listLPD $3 }

FunctionDeclaration      : FUNCTION_INI ID_FUNCTION ',' S_therewasa Type FunctionBlock FUNCTION_FIN                                        { FDT $2 $5 $6  }
                         | FUNCTION_INI ID_FUNCTION ',' S_therewasa Type '(' S_madeof FunctionFormalParams ')' FunctionBlock FUNCTION_FIN  { FDAT $2 $5 $8 $10 }

FunctionFormalParams     : Type ID                                                  { LFDP [($1,$2,0)] }
                         | Type ID ',' FunctionFormalParams                         { LFDP $ ($1,$2,0): listLFDP $4 }
                         | YOUR Type ID                                             { LFDP [($2,$3,1)] }
                         | YOUR Type ID ',' FunctionFormalParams                    { LFDP $ ($2,$3,1): listLFDP $5 }


VariableDeclaration      : ID S_broughta Type ':' VariableList                      { VDT $1 $3 $5 }

VariableList             : ID ',' VariableList                                      { LDV $ ($1,Nothing): listLDV $3 }
                         | ID '=' Expression ',' VariableList                       { LDV $ ($1,Just $3): listLDV $5 }
                         | ID '=' Expression                                        { LDV [($1,Just $3)] }
                         | ID                                                       { LDV [($1,Nothing)] }
-- Types
-----------------------------------------------------------------------------------------------------------------------
Type                     : TYPE_INT                                                 { TI }
                         | TYPE_FLOAT                                               { TF }
                         | TYPE_CHAR                                                { TC }
                         | TYPE_BOOL                                                { TB }
                         | TYPE_STRING                                              { TS }
                         | TYPE_ARRAY '(' OF LITERAL_INT Type ')'                   { TA $4 $5 }
                         | TYPE_STRUCT '(' WITH StructTyping ')'                    { TST $4 }
                         | TYPE_UNION '(' EITHER UnionTyping ')'                    { TU $4 }
                         | TYPE_POINTER '(' TO Type ')'                             { TP $4 }
                         | ID                                                       { TID $1 }

StructTyping             : Type ID                                                  { LSRT [($1,$2)] }
                         | Type ID and StructTyping                                 { LSRT $ ($1,$2): listLSRT $4 }

UnionTyping              : Type ID                                                  { LUT [($1,$2)] }
                         | Type ID or UnionTyping                                   { LUT $ ($1,$2): listLUT $4 }


-- Blocks
-----------------------------------------------------------------------------------------------------------------------
Block                    : BLOCK_OPEN BlockContent BLOCK_CLOSE                      { $2 }

BlockContent             : Statement '.' BlockContent                               { LST $ $1: listLST $3 }
                         | {-empty-}                                                { LST [] }

FunctionBlock            : BLOCK_OPEN FunctionBlockContent BLOCK_CLOSE              { $2 }

FunctionBlockContent     : Statement '.' FunctionBlockContent                       { LFBT $ (StaT $1): listLFBT $3 }
                         | ReturnStatement '.' FunctionBlockContent                 { LFBT $ $1: listLFBT $3 }
                         | {-empty-}                                                { LFBT [] }

Statement                : Instruction                                              { InsT $1 }
                         | Declaration                                              { DecT $1 }


ReturnStatement          : S_Andthatswhere Expression S_comesfrom                   { RT $2 }


-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction              : Expression                                               { ExprT $1 }
                         | Selection                                                { $1 }
                         | UnboundedIteration                                       { $1 }
                         | BoundedIteration                                         { $1 }
                         | ManageMemory                                             { $1 }
                         | Print                                                    { $1 }

ManageMemory             : CreatePointer                                            { $1 }
                         | FreePointer                                              { $1 }

CreatePointer            : ID S_madea Type                                          { CPT $1 $3 }
FreePointer              : ID S_brokea ID                                           { FPT $1 $3 }

Selection                : ID S_dreamsof Block WHEN Expression                      { IFT $1 $3 $5 }
                         | ID S_dreamsof Block WHEN Expression ';' OTHERWISE Block  { IFET $1 $3 $5 $8 }

UnboundedIteration       : ID S_keepsdreamingof Expression Block                    { UIT $1 $3 $4 }

BoundedIteration         : Block ID S_toldthatstory Expression TIMES                { BIT $1 $2 $4 }

Print                    : ID ':' Expression                                        { PRT $1 $3 }


{

happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
  lcn = case tks of
          []    -> "end of file"
          ((Token (AlexPn _ l c) _):_)  -> "line " ++ show l ++ ", column " ++ show c
}
