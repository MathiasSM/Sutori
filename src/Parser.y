{
module Parser where

import Lexer
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
Source                   : PROGRAM_INI ID Block PROGRAM_FIN EOF                                                 { putStrLn "Source" }

-- Expressions
----------------------------------------------------------------------------------------------------------------------
Expression               : ID                                                                                   { putStrLn "Expression" }
                         | Literal                                                                              { putStrLn "Expression" }
                         | Constructor                                                                          { putStrLn "Expression" }
                         | FunctionCall                                                                         { putStrLn "Expression" }
                         | Operation                                                                            { putStrLn "Expression" }
                         | '(' Expression ')'                                                                   { putStrLn "Expression" }

Literal                  : LITERAL_INT                                                                          { putStrLn "NoTerminal" }
                         | LITERAL_CHAR                                                                         { putStrLn "NoTerminal" }
                         | LITERAL_FLOAT                                                                        { putStrLn "NoTerminal" }
                         | LITERAL_STRING                                                                       { putStrLn "NoTerminal" }
                         | TRUE                                                                                 { putStrLn "NoTerminal" }
                         | FALSE                                                                                { putStrLn "NoTerminal" }

-- Constructors
-----------------------------------------------------------------------------------------------------------------------
Constructor              : ConstructorArray                                                                     { putStrLn "NoTerminal" }
                         | ConstructorStruct                                                                    { putStrLn "NoTerminal" }

ConstructorArray         : '[' ConstructorArrayList ']'                                                         { putStrLn "NoTerminal" }
ConstructorArrayList     : Expression                                                                           { putStrLn "NoTerminal" }
                         | Expression ';' ConstructorArrayList                                                  { putStrLn "NoTerminal" }

ConstructorStruct        : '{' ConstructorStructList '}'                                                        { putStrLn "NoTerminal" }
ConstructorStructList    : ID ':' Expression                                                                    { putStrLn "NoTerminal" }
                         | ID ':' Expression ';' ConstructorStructList                                          { putStrLn "NoTerminal" }


-- Operators
-----------------------------------------------------------------------------------------------------------------------
Operation                : UnaryOperation                                                                       { putStrLn "NoTerminal" }
                         | BinaryOperation                                                                      { putStrLn "NoTerminal" }

UnaryOperation           : NumericalUnaryOperation                                                              { putStrLn "NoTerminal" }
                         | LogicalUnaryOperation                                                                { putStrLn "NoTerminal" }
                         | Dereference                                                                          { putStrLn "NoTerminal" }

BinaryOperation          : NumericalBinaryOperation                                                             { putStrLn "NoTerminal" }
                         | LogicalBinaryOperation                                                               { putStrLn "NoTerminal" }
                         | GetProp                                                                              { putStrLn "NoTerminal" }
                         | GetArrayItem                                                                         { putStrLn "NoTerminal" }
                         | Assignment                                                                           { putStrLn "NoTerminal" }

NumericalUnaryOperation  : '+' Expression %prec POS                                                             { putStrLn "NoTerminal" }
                         | '-' Expression %prec NEG                                                             { putStrLn "NoTerminal" }

LogicalUnaryOperation    : '!' Expression                                                                       { putStrLn "NoTerminal" }

Dereference              : '*' Expression %prec IND                                                             { putStrLn "NoTerminal" }

NumericalBinaryOperation : Expression '+' Expression                                                            { putStrLn "NoTerminal" }
                         | Expression '-' Expression                                                            { putStrLn "NoTerminal" }
                         | Expression '*' Expression                                                            { putStrLn "NoTerminal" }
                         | Expression '/' Expression                                                            { putStrLn "NoTerminal" }
                         | Expression div Expression                                                            { putStrLn "NoTerminal" }
                         | Expression '%' Expression                                                            { putStrLn "NoTerminal" }
                         | Expression '^' Expression                                                            { putStrLn "NoTerminal" }

LogicalBinaryOperation   : Expression and  Expression                                                           { putStrLn "NoTerminal" }
                         | Expression or   Expression                                                           { putStrLn "NoTerminal" }
                         | Expression '==' Expression                                                           { putStrLn "NoTerminal" }
                         | Expression '/=' Expression                                                           { putStrLn "NoTerminal" }
                         | Expression '>=' Expression                                                           { putStrLn "NoTerminal" }
                         | Expression '<=' Expression                                                           { putStrLn "NoTerminal" }
                         | Expression '>'  Expression                                                           { putStrLn "NoTerminal" }
                         | Expression '<'  Expression                                                           { putStrLn "NoTerminal" }

GetArrayItem             : ID '[' Expression ']'                                                                { putStrLn "NoTerminal" }

GetProp                  : Expression '->' ID                                                                   { putStrLn "NoTerminal" }

Assignment               : ID '=' Expression                                                                    { putStrLn "NoTerminal" }

FunctionCall             : ID_FUNCTION                                                                          { putStrLn "NoTerminal" }
                         | ID_FUNCTION '(' WITH FunctionActualParams ')'                                        { putStrLn "NoTerminal" }

FunctionActualParams     : Expression                                                                           { putStrLn "NoTerminal" }
                         | Expression ',' FunctionActualParams                                                  { putStrLn "NoTerminal" }


-- Declaration
-----------------------------------------------------------------------------------------------------------------------
Declaration              : PersonDeclaration                                                                    { putStrLn "NoTerminal" }
                         | FunctionDeclaration                                                                  { putStrLn "NoTerminal" }
                         | VariableDeclaration                                                                  { putStrLn "NoTerminal" }

PersonDeclaration        : S_Therewas PersonNames                                                               { putStrLn "NoTerminal" }

PersonNames              : ID                                                                            { putStrLn "NoTerminal" }
                         | ID ',' PersonNames                                                            { putStrLn "NoTerminal" }
                         | ID and PersonNames                                                            { putStrLn "NoTerminal" }


FunctionDeclaration      : FUNCTION_INI ID_FUNCTION ',' S_therewasa Type FunctionBlock FUNCTION_FIN             { putStrLn "NoTerminal" }
                         | FUNCTION_INI ID_FUNCTION ',' S_therewasa Type '(' S_madeof FunctionFormalParams ')' FunctionBlock FUNCTION_FIN  { putStrLn "NoTerminal" }

FunctionFormalParams     : Type ID                                                                              { putStrLn "NoTerminal" }
                         | Type ID ',' FunctionFormalParams                                                     { putStrLn "NoTerminal" }
                         | YOUR Type ID                                                                         { putStrLn "NoTerminal" }
                         | YOUR Type ID ',' FunctionFormalParams                                                { putStrLn "NoTerminal" }

VariableDeclaration      : ID S_broughta Type ':' VariableList                                           { putStrLn "NoTerminal" }

VariableList             : ID ',' VariableList                                                                  { putStrLn "NoTerminal" }
                         | ID '=' Expression ',' VariableList                                                   { putStrLn "NoTerminal" }
                         | ID '=' Expression                                                                    { putStrLn "NoTerminal" }
                         | ID                                                                                   { putStrLn "NoTerminal" }

-- Types
-----------------------------------------------------------------------------------------------------------------------
Type                     : TYPE_INT                                                                             { putStrLn "NoTerminal" }
                         | TYPE_FLOAT                                                                           { putStrLn "NoTerminal" }
                         | TYPE_CHAR                                                                            { putStrLn "NoTerminal" }
                         | TYPE_BOOL                                                                            { putStrLn "NoTerminal" }
                         | TYPE_STRING                                                                          { putStrLn "NoTerminal" }
                         | TYPE_ARRAY '(' OF LITERAL_INT Type ')'                                               { putStrLn "NoTerminal" }
                         | TYPE_STRUCT '(' WITH StructTyping ')'                                                { putStrLn "NoTerminal" }
                         | TYPE_UNION '(' EITHER UnionTyping ')'                                                { putStrLn "NoTerminal" }
                         | TYPE_POINTER '(' TO Type ')'                                                         { putStrLn "NoTerminal" }
                         | ID                                                                                   { putStrLn "NoTerminal" }

StructTyping             : Type ID                                                                              { putStrLn "NoTerminal" }
                         | Type ID and StructTyping                                                             { putStrLn "NoTerminal" }

UnionTyping              : Type ID                                                                              { putStrLn "NoTerminal" }
                         | Type ID or UnionTyping                                                               { putStrLn "NoTerminal" }


-- Blocks
-----------------------------------------------------------------------------------------------------------------------
Block                    : BLOCK_OPEN BlockContent BLOCK_CLOSE                                                  { putStrLn "NoTerminal" }

BlockContent             : Statement '.' BlockContent                                                           { putStrLn "NoTerminal" }
                         | {-empty-}                                                                            { putStrLn "NoTerminal" }

FunctionBlock            : BLOCK_OPEN FunctionBlockContent BLOCK_CLOSE                                          { putStrLn "NoTerminal" }

FunctionBlockContent     : Statement '.' FunctionBlockContent                                                   { putStrLn "NoTerminal" }
                         | ReturnStatement '.' FunctionBlockContent                                             { putStrLn "NoTerminal" }
                         | {-empty-}                                                                            { putStrLn "NoTerminal" }

Statement                : Instruction                                                                          { putStrLn "NoTerminal" }
                         | Declaration                                                                          { putStrLn "NoTerminal" }
                         | {-empty-}                                                                            { putStrLn "NoTerminal" }

ReturnStatement          : S_Andthatswhere Expression S_comesfrom                                               { putStrLn "NoTerminal" }



-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction              : Expression                                                                           { putStrLn "NoTerminal" }
                         | Selection                                                                            { putStrLn "NoTerminal" }
                         | UnboundedIteration                                                                   { putStrLn "NoTerminal" }
                         | BoundedIteration                                                                     { putStrLn "NoTerminal" }
                         | ManageMemory                                                                         { putStrLn "NoTerminal" }
                         | Print                                                                                { putStrLn "NoTerminal" }

ManageMemory             : CreatePointer                                                                        { putStrLn "NoTerminal" }
                         | FreePointer                                                                          { putStrLn "NoTerminal" }

CreatePointer            : ID S_madea Type                                                                      { putStrLn "NoTerminal" }
FreePointer              : ID S_brokea ID                                                                       { putStrLn "NoTerminal" }

Selection                : ID S_dreamsof Block WHEN Expression                                           { putStrLn "NoTerminal" }
                         | ID S_dreamsof Block WHEN Expression ';' OTHERWISE Block                       { putStrLn "NoTerminal" }

UnboundedIteration       : ID S_keepsdreamingof Expression Block                                         { putStrLn "NoTerminal" }

BoundedIteration         : Block ID S_toldthatstory Expression TIMES                                     { putStrLn "NoTerminal" }

Print                    : ID ':' Expression                                                             { putStrLn "NoTerminal" }


{

happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
  lcn = case tks of
          []    -> "end of file"
          ((Token (AlexPn _ l c) _):_)  -> "line " ++ show l ++ ", column " ++ show c
}
