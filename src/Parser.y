{
module Parser where

import Lexer
}

%name      calc
%tokentype { Lexeme }
%error     {  }

%token
    EOF                 { EOF _               }
    PROGRAM_INI         { PROGRAM_INI _       }
    PROGRAM_FIN         { PROGRAM_FIN _       }
    FUNCTION_INI        { FUNCTION_INI _      }
    FUNCTION_FIN        { FUNCTION_FIN _      }
    S_Andthatswhere     { S_Andthatswhere _   }
    S_Therewas          { S_Therewas _        }
    S_brokea            { S_brokea _          }
    S_broughta          { S_broughta _        }
    S_comesfrom         { S_comesfrom _       }
    S_dreamsof          { S_dreamsof _        }
    S_keepsdreamingof   { S_keepsdreamingof _ }
    S_madeof            { S_madeof _          }
    S_madea             { S_madea _           }
    S_therewasa         { S_therewasa _       }
    S_toldthatstory     { S_toldthatstory _   }
    TYPE_INT            { TYPE_INT _          }
    TYPE_FLOAT          { TYPE_FLOAT _        }
    TYPE_CHAR           { TYPE_CHAR _         }
    TYPE_BOOL           { TYPE_BOOL _         }
    TYPE_ARRAY          { TYPE_ARRAY _        }
    TYPE_STRUCT         { TYPE_STRUCT _       }
    TYPE_UNION          { TYPE_UNION _        }
    TYPE_STRING         { TYPE_STRING _       }
    TYPE_POINTER        { TYPE_POINTER _      }
    WITH                { WITH _              }
    YOUR                { YOUR _              }
    OF                  { OF _                }
    EITHER              { EITHER _            }
    TO                  { TO _                }
    WHEN                { WHEN _              }
    OTHERWISE           { OTHERWISE _         }
    TIMES               { TIMES _             }
    TRUE                { TRUE _              }
    FALSE               { FALSE _             }
    BLOCK_OPEN          { BLOCK_OPEN _        }
    BLOCK_CLOSE         { BLOCK_CLOSE _       }
    '('                 { OPEN_PAREN _        }
    '['                 { OPEN_BRACKETS _     }
    '{'                 { OPEN_BRACES _       }
    ')'                 { CLOSE_PAREN _       }
    ']'                 { CLOSE_BRACKETS _    }
    '}'                 { CLOSE_BRACES _      }
    '.'                 { PERIOD _            }
    ','                 { COMMA _             }
    ':'                 { COLON _             }
    ';'                 { SEMICOLON _         }
    '!'                 { EXCLAMATION _       }
    '->'                { ARROW_RIGHT _       }
    '+'                 { PLUS _              }
    '-'                 { MINUS _             }
    '=='                { EQUAL _             }
    '='                 { ASSIGNMENT _        }
    '*'                 { ASTERISK _          }
    '%'                 { PERCENT _           }
    '/'                 { SLASH _             }
    div                 { DIV _               }
    '/='                { NOT_EQUAL _         }
    '>='                { GREATER_EQUAL _     }
    '<='                { LESS_EQUAL _        }
    '>'                 { GREATER _           }
    '<'                 { LESS _              }
    '^'                 { POWER _             }
    and                 { AND _               }
    or                  { OR _                }
    LITERAL_CHAR        { Character _ $$      }
    LITERAL_FLOAT       { Float' _ $$         }
    LITERAL_INT         { Integer' _ $$       }
    LITERAL_STRING      { String' _ $$        }
    ID_FUNCTION         { FunctionID _ $$     }
    ID_PERSON           { PersonID _ $$       }
    ID                  { ID _ $$             }


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
Source                   : PROGRAM_INI ID Block PROGRAM_FIN EOF                                                 { PN $1 }

-- Expressions
-----------------------------------------------------------------------------------------------------------------------
Expression               : ID                                                                                   { IdN (tokenString $1) (tokenPos $1) }
                         | Literal                                                                              { $1 }
                         | Constructor                                                                          { $1 }
                         | FunctionCall                                                                         { $1 }
                         | Operation                                                                            { $1 }
                         | '(' Expression ')'                                                                   { ParentExp $2 }

Literal                  : LITERAL_INT                                                                          { NumberLiteralN (tokenString $1) (tokenPos $1) }
                         | LITERAL_CHAR                                                                         { NumberLiteralN (tokenString $1) (tokenPos $1) }
                         | LITERAL_FLOAT                                                                        { NumberLiteralN (tokenString $1) (tokenPos $1) }
                         | LITERAL_STRING                                                                       { NumberLiteralN (tokenString $1) (tokenPos $1) }
                         | TRUE                                                                                 { trueN }
                         | FALSE                                                                                { falseN }

-- Constructors
-----------------------------------------------------------------------------------------------------------------------
Constructor              : ConstructorArray                                                                     { $1 }
                         | ConstructorStruct                                                                    { $1 }

ConstructorArray         : '[' ConstructorArrayList ']'                                                         { CN $2 }
ConstructorArrayList     : Expression                                                                           { LCAN $ [$1] }
                         | Expression ';' ConstructorArrayList                                                  { LCAN $ $1: listLCA $3 }

ConstructorStruct        : '{' ConstructorStructList '}'                                                        { CN $2 }
ConstructorStructList    : ID ':' Expression                                                                    { LCSN $ [($1,$3)] }
                         | ID ':' Expression ';' ConstructorStructList                                          { LCSN $ ($1,$3): listLCA $5 }


-- Operators
-----------------------------------------------------------------------------------------------------------------------
Operation                : UnaryOperation                                                                       { $1 }
                         | BinaryOperation                                                                      { $1 }

UnaryOperation           : NumericalUnaryOperation                                                              { $1 }
                         | LogicalUnaryOperation                                                                { $1 }
                         | Dereference                                                                          { $1 }

BinaryOperation          : NumericalBinaryOperation                                                             { $1 }
                         | LogicalBinaryOperation                                                               { $1 }
                         | GetProp                                                                              { $1 }
                         | GetArrayItem                                                                         { $1 }
                         | Assignment                                                                           { $1 }

NumericalUnaryOperation  : '+' Expression %prec POS                                                             { PlusN $2 }
                         | '-' Expression %prec NEG                                                             { MinusN $2 }

LogicalUnaryOperation    : '!' Expression                                                                       { NotN $2 }

Dereference              : '*' Expression %prec IND                                                             { DeferenceN $2 }

NumericalBinaryOperation : Expression '+' Expression                                                            { BinaryN $1 "+" $3 }
                         | Expression '-' Expression                                                            { BinaryN $1 "-" $3 }
                         | Expression '*' Expression                                                            { BinaryN $1 "*" $3 }
                         | Expression '/' Expression                                                            { BinaryN $1 "/" $3 }
                         | Expression div Expression                                                            { BinaryN $1 "div" $3 }
                         | Expression '%' Expression                                                            { BinaryN $1 "%" $3 }
                         | Expression '^' Expression                                                            { BinaryN $1 "^" $3 }

LogicalBinaryOperation   : Expression and  Expression                                                           { CompareN $1 "and" $3 }
                         | Expression or   Expression                                                           { CompareN $1 "or" $3 }
                         | Expression '==' Expression                                                           { CompareN $1 "==" $3 }
                         | Expression '/=' Expression                                                           { CompareN $1 "/=" $3 }
                         | Expression '>=' Expression                                                           { CompareN $1 ">=" $3 }
                         | Expression '<=' Expression                                                           { CompareN $1 "<=" $3 }
                         | Expression '>'  Expression                                                           { CompareN $1 ">" $3 }
                         | Expression '<'  Expression                                                           { CompareN $1 "<" $3 }

GetArrayItem             : ID '[' Expression ']'                                                                { GetArrayItem $1 $3 }

GetProp                  : Expression '->' ID                                                                   { GetPropN $1 $3 }

Assignment               : ID '=' Expression                                                                    { Assignment $1 $3 }

FunctionCall             : ID_FUNCTION                                                                          { FCN $1 [] }
                         | ID_FUNCTION '(' WITH FunctionActualParams ')'                                        { FCN $1 $4 }

FunctionActualParams     : Expression                                                                           { FPN [$1] }
                         | Expression ',' FunctionActualParams                                                  { FPN $ $1: ListFPN $3 }


-- Declaration
-----------------------------------------------------------------------------------------------------------------------
Declaration              : PersonDeclaration                                                                    { $1 }
                         | FunctionDeclaration                                                                  { $1 }
                         | VariableDeclaration                                                                  { $1 }

PersonDeclaration        : S_Therewas PersonNames                                                               { PDN $1 }

PersonNames              : ID_PERSON                                                                            { LDPN [tokenString $1] }
                         | ID_PERSON ',' PersonNames                                                            { LPDN $ (tokenString $1) : listLDPN $3 }
                         | ID_PERSON and PersonNames                                                            { LPDN $ (tokenString $1) : listLDPN $3 }


FunctionDeclaration      : FUNCTION_INI ID_FUNCTION ',' S_therewasa Type FunctionBlock FUNCTION_FIN             { DFN (tokenString $2) $5 $6 }
                         | FUNCTION_INI ID_FUNCTION ',' S_therewasa Type '(' S_madeof FunctionFormalParams ')' FunctionBlock FUNCTION_FIN  { PDFN (tokenString $2) $5 $8 $10 }

FunctionFormalParams     : Type ID                                                                              { LPN [($1, (tokenString $2),1)] }
                         | Type ID ',' FunctionFormalParams                                                     { LPN $ ($1, (tokenString $2),1) : listLPN $4 }
                         | YOUR Type ID                                                                         { LPN [($2, (tokenString $3),0)] }
                         | YOUR Type ID ',' FunctionFormalParams                                                { LPN $ ($2, (tokenString $3),1) : listLPN $5 }

VariableDeclaration      : ID_PERSON S_broughta Type ':' VariableList                                           { VDN $3 $5 }

VariableList             : ID ',' VariableList                                                                  { LVN $ (tokenString $1,undefined) : listLVN $3 }
                         | ID '=' Expression ',' VariableList                                                   { LVN $ (tokenString $1,$3) : listLVN $5 }
                         | ID '=' Expression                                                                    { LVN [(tokenString $1,$3)] }
                         | ID                                                                                   { LVN [(tokenString $1,undefined)] }

-- Types
-----------------------------------------------------------------------------------------------------------------------
Type                     : TYPE_INT                                                                             { IntNumN }
                         | TYPE_FLOAT                                                                           { FloatNumN }
                         | TYPE_CHAR                                                                            { CharcN }
                         | TYPE_BOOL                                                                            { BooleanN }
                         | TYPE_STRING                                                                          { StringN }
                         | TYPE_ARRAY '(' OF LITERAL_INT Type ')'                                               { ArrayN $4 $5 }
                         | TYPE_STRUCT '(' WITH StructTyping ')'                                                { StructN $4 }
                         | TYPE_UNION '(' EITHER UnionTyping ')'                                                { UnionN $4 }
                         | TYPE_POINTER '(' TO Type ')'                                                         { PointerN $4 }
                         | ID                                                                                   { }

StructTyping             : Type ID                                                                              { STN [($1, (tokenString $2))]}
                         | Type ID and StructTyping                                                             { STN $ ($1, (tokenString $2)) : listSTN $4 }

UnionTyping              : Type ID                                                                              { UTN [($1, (tokenString $2))] }
                         | Type ID or UnionTyping                                                               { UTN $ ($1, (tokenString $2)) : listUTN $4 }


-- Blocks
-----------------------------------------------------------------------------------------------------------------------
Block                    : BLOCK_OPEN BlockContent BLOCK_CLOSE                                                  { BN $2 }

BlockContent             : Statement '.' BlockContent                                                           { BCN $ $1 : listBCN $2 }
                         | {-empty-}                                                                            { BCN [] }

FunctionBlock            : BLOCK_OPEN FunctionBlockContent BLOCK_CLOSE                                          { FBN $1 }

FunctionBlockContent     : Statement '.' FunctionBlockContent                                                   { FBCN $ $1 : ListFBCN $3 }
                         | ReturnStatement '.' FunctionBlockContent                                             { FBCN $ $1 : ListFBCN $3 }
                         | {-empty-}                                                                            { FBCN [] }

Statement                : Instruction                                                                          { $1 }
                         | Declaration                                                                          { $1 }
                         | {-empty-}                                                                            { EmptyStatement }

ReturnStatement          : S_Andthatswhere Expression S_comesfrom                                               { REN $1 }



-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction              : Expression                                                                           { ExprN $1 }
                         | Selection                                                                            { $1 }
                         | UnboundedIteration                                                                   { $1 }
                         | BoundedIteration                                                                     { $1 }
                         | ManageMemory                                                                         { $1 }
                         | Print                                                                                { $1 }

ManageMemory             : CreatePointer                                                                        { $1 }
                         | FreePointer                                                                          { $1 }

CreatePointer            : ID S_madea Type                                                                      { CreatePointerN $1 $3 }
FreePointer              : ID S_brokea ID                                                                       { FreePointerN $1 $3 }

Selection                : ID_PERSON S_dreamsof Block WHEN Expression                                           { IfThenN $1 $3 $5 }
                         | ID_PERSON S_dreamsof Block WHEN Expression ';' OTHERWISE Block                       { IfThenElseN $1 $3 $5 $8 }

UnboundedIteration       : ID_PERSON S_keepsdreamingof Expression Block                                         { WhileN $1 $3 $4 }

BoundedIteration         : Block ID_PERSON S_toldthatstory Expression TIMES                                     { ForN $2 $1 $4 }

Print                    : ID_PERSON ':' Expression                                                             { }


{

main:: IO ()
main = interact (show . runCalc)

trans :: [Token] -> [Lexeme]
trans ((Token (EOF _))         :[])   = []
trans ((Token p (Float' f))    :toks) = (Float' p f)     : (trans toks)
trans ((Token p (Integer' f))  :toks) = (Integer' p f)   : (trans toks)
trans ((Token p (ID f))        :toks) = (ID p f)         : (trans toks)
trans ((Token p (FunctionID f)):toks) = (FunctionID p f) : (trans toks)
trans ((Token p (String' f))   :toks) = (String' p f)    : (trans toks)
trans ((Token p cl)            :toks) = (cl (np p))      : (trans toks)
  where np (AlexPn _ f c) = (f, c)

runCalc = calc . trans .  alexScanTokens

data Lexeme = EOF        { tokenPos :: (Int, Int) }
     | PROGRAM_INI       { tokenPos :: (Int, Int) }
     | PROGRAM_FIN       { tokenPos :: (Int, Int) }
     | FUNCTION_INI      { tokenPos :: (Int, Int) }
     | FUNCTION_FIN      { tokenPos :: (Int, Int) }
     | S_Andthatswhere   { tokenPos :: (Int, Int) }
     | S_Therewas        { tokenPos :: (Int, Int) }
     | S_brokea          { tokenPos :: (Int, Int) }
     | S_broughta        { tokenPos :: (Int, Int) }
     | S_comesfrom       { tokenPos :: (Int, Int) }
     | S_dreamsof        { tokenPos :: (Int, Int) }
     | S_keepsdreamingof { tokenPos :: (Int, Int) }
     | S_madeof          { tokenPos :: (Int, Int) }
     | S_madea           { tokenPos :: (Int, Int) }
     | S_therewasa       { tokenPos :: (Int, Int) }
     | S_toldthatstory   { tokenPos :: (Int, Int) }
     | TYPE_INT          { tokenPos :: (Int, Int) }
     | TYPE_FLOAT        { tokenPos :: (Int, Int) }
     | TYPE_CHAR         { tokenPos :: (Int, Int) }
     | TYPE_BOOL         { tokenPos :: (Int, Int) }
     | TYPE_ARRAY        { tokenPos :: (Int, Int) }
     | TYPE_STRUCT       { tokenPos :: (Int, Int) }
     | TYPE_UNION        { tokenPos :: (Int, Int) }
     | TYPE_STRING       { tokenPos :: (Int, Int) }
     | TYPE_POINTER      { tokenPos :: (Int, Int) }
     | WITH              { tokenPos :: (Int, Int) }
     | YOUR              { tokenPos :: (Int, Int) }
     | OF                { tokenPos :: (Int, Int) }
     | EITHER            { tokenPos :: (Int, Int) }
     | TO                { tokenPos :: (Int, Int) }
     | WHEN              { tokenPos :: (Int, Int) }
     | OTHERWISE         { tokenPos :: (Int, Int) }
     | TIMES             { tokenPos :: (Int, Int) }
     | TRUE              { tokenPos :: (Int, Int) }
     | FALSE             { tokenPos :: (Int, Int) }
     | BLOCK_OPEN        { tokenPos :: (Int, Int) }
     | BLOCK_CLOSE       { tokenPos :: (Int, Int) }
     | OPEN_PAREN        { tokenPos :: (Int, Int) }
     | OPEN_BRACKETS     { tokenPos :: (Int, Int) }
     | OPEN_BRACES       { tokenPos :: (Int, Int) }
     | CLOSE_PAREN       { tokenPos :: (Int, Int) }
     | CLOSE_BRACKETS    { tokenPos :: (Int, Int) }
     | CLOSE_BRACES      { tokenPos :: (Int, Int) }
     | ELLIPSIS          { tokenPos :: (Int, Int) }
     | PERIOD            { tokenPos :: (Int, Int) }
     | COMMA             { tokenPos :: (Int, Int) }
     | COLON             { tokenPos :: (Int, Int) }
     | SEMICOLON         { tokenPos :: (Int, Int) }
     | DOLLAR            { tokenPos :: (Int, Int) }
     | INTERROGATION     { tokenPos :: (Int, Int) }
     | EXCLAMATION       { tokenPos :: (Int, Int) }
     | ARROW_RIGHT       { tokenPos :: (Int, Int) }
     | PLUS              { tokenPos :: (Int, Int) }
     | MINUS             { tokenPos :: (Int, Int) }
     | EQUAL             { tokenPos :: (Int, Int) }
     | ASSIGNMENT        { tokenPos :: (Int, Int) }
     | ASTERISK          { tokenPos :: (Int, Int) }
     | PERCENT           { tokenPos :: (Int, Int) }
     | SLASH             { tokenPos :: (Int, Int) }
     | DIV               { tokenPos :: (Int, Int) }
     | NOT_EQUAL         { tokenPos :: (Int, Int) }
     | GREATER_EQUAL     { tokenPos :: (Int, Int) }
     | LESS_EQUAL        { tokenPos :: (Int, Int) }
     | GREATER           { tokenPos :: (Int, Int) }
     | LESS              { tokenPos :: (Int, Int) }
     | POWER             { tokenPos :: (Int, Int) }
     | AND               { tokenPos :: (Int, Int) }
     | OR                { tokenPos :: (Int, Int) }
     | InvalidToken      { tokenPos :: (Int, Int) }
     | Character         { tokenPos :: (Int, Int), char :: String }
     | Float'            { tokenPos :: (Int, Int), float :: Float  }
     | Integer'          { tokenPos :: (Int, Int), int :: Int    }
     | String'           { tokenPos :: (Int, Int), string :: String }
     | FunctionID        { tokenPos :: (Int, Int), id :: String }
     | PersonID          { tokenPos :: (Int, Int), id :: String }
     | ID                { tokenPos :: (Int, Int), id :: String }

  deriving (Eq)


happyError :: [Lexeme] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
  lcn = case tks of
          []    -> "end of file"
          tk:_  -> "line " ++ show l ++ ", column " ++ show c
            where
              AlexPn _ l c = token_posn tk
}
