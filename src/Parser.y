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
%nonassoc '>' '<' '==' '/=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%' mod div
%left '^'
%nonassoc '!'
%left '->'


%% 

-- Program
P : begin id B end { PN $1 }

B : '...(' BC ')...' { BN $2 }

-- List of Blocks
BC  : {-empty-}         { BCN [] }
    | S '.' BC          { BCN $ $1 : listBCN $2}

-- Statement 
S : DF  { SN $1 }
  | PD  { SN $1 }
  | VD  { SN $1 }
  | I   { SN $1 }

-- Function Block
FB : '...(' FBC ')...' { FBN $1 }

-- Function Block Content
FBC : S '.' FBC           { FBCN $ $1 : ListFBCN $3 } 
  | RF '.' FBC        { FBCN $ $1 : ListFBCN $3 }
  | {-empty-}         { FBCN [] }

-- Return Function
RF : andThat E comes { REN $1 }

-- Function Declaration
DF  : '(' fBegin fid ',' thereWas T FB fEnd ')' { DFN (tokenValue $3) $6 $7 $1 }
  | '(' fBegin fid ',' thereWas T '(' from LP ')' FB fEnd ')' { PDFN (tokenValue $3) $6 $9 $11 $1 }

-- List of params    
LP : T id             { LPN [($1, (tokenValue $2),1)] }
   | T your id         { LPN [($1, (tokenValue $3),0)] }
   | T id ',' LP      { LPN $ ($1, (tokenValue $2),1) : listLPN $4 }
   | T your id ',' LP      { LPN $ ($1, (tokenValue $3),0)) : listLPN $5 }

--Types 
T : intNum            { IntNumN }
  | floatNum          { FloatNumN }
  | charc             { CharcN }
  | boolean           { BooleanN }
  | arrayT '(' of n T ')'   { ArrayN $4 $5 }
  | structT '(' with ST ')'   { StructN $4 }
  | unionT '(' either UT ')'    { UnionN $4 }
  | pointerT '(' to T ')'       { PointerN $4 }

--Struct Type
ST : T id       { STN [($1, (tokenValue $2))]} 
   | T id and ST  { STN $ ($1, (tokenValue $2)) : listSTN $4 }

--Union Type
UT : T id       { UTN [($1, (tokenValue $2))] } 
   | T id or UT   { UTN $ ($1, (tokenValue $2)) : listUTN $4 }

-- Person Declaration
PD : thereWas LPD   { PDN $ 1 }

-- List of Person Declaration
LPD : id      { LDPN [tokenValue $1] }        
  | id ',' LPD  { LPDN $ (tokenValue $1) : listLDPN $3 }
  | id and LPD  { LPDN $ (tokenValue $1) : listLDPN $3 }

-- Variable Declaration
VD : id broughta T ':' LV  { VDN $3 $5 }

-- List of Variable Declaration
LV : id '=' E        { LVN [(tokenValue $1,$3)] }
   | id            { LVN [(tokenValue $1,undefined)] }
   | id '=' E ',' LV   { LVN $ (tokenValue $1,$3) : listLVN $5 } 
   | id E ',' LV       { LVN $ (tokenValue $1,undefined) : listLVN $4 }

-- List of Instructions
I : E                       { ExprN $1 }
  | E '->' id                 { GetPropN $1 (tokenValue $3) $2 }
  | id '=' E                { AssignN $1 $3 (tokenPos $1) }
  | id dreamsof B when E          { IfThenN $1 $3 $5 (tokenPos $1) }
  | id dreamsof B when E ';' otherwise B    { IfThenElseN $1 $3 $5 $8 (tokenPos $1) }
  | id keeps E B              { WhileN $1 $3 $4 (tokenPos $1) }
  | B id told E times                       { ForN $1 $2 $4 (tokenPos $2)} 
  | id made T                 { CreatePointerN $1 $3 (tokenPos $1)}
  | id broke id                           { FreePointerN $1 $3 (tokenPos $1) }

-- Constructor Array 
LCA : E       { LCAN $ [$1] }
  | E ',' LCA     { LCAN $ $1: listLCA $3 }

-- Constructor Struct
LCS : id ':' E        { LCSN $ [(tokenValue $1,$3)] }
  | id ':' E ',' LCS      { LCSN $ (tokenValue $1,$3): listLCA $5 }

-- Function call parameters xd  
FP : E      { FPN [$1] }
   | E ',' FP   { FPN $ $1: listFPN $3 }

-- Expressions
E : id           { IdN (tokenValue $1) (tokenPos $1) }
  | '[' LCA ']'      { CN $2 }
  | '{' LCS '}'      { CN $2 }
  | fid          { FCN (tokenValue $1) [] (tokenPos $1) }
  | fid '(' with FP ')'  { FCN (tokenValue $1) $4 (tokenPos $1) }
  | '!' E          { NotN $2 $1 }
  | '-' E          { MinusN $2 $1 }
  | '*' E         { DeferenceN $2 $1 }
  | E '+' E        { BinaryN $1 "+" $3 $2 }
  | E '-' E        { BinaryN $1 "-" $3 $2 }
  | E '*' E        { BinaryN $1 "*" $3 $2 }
  | E '/' E        { BinaryN $1 "/" $3 $2 }
  | E div E        { BinaryN $1 "div" $3 $2}
  | E '%' E        { BinaryN $1 "%" $3 $2 }
  | E '^' E        { BinaryN $1 "^" $3 $2}
  | E and E          { LogicN $1 "and" $3 $2 } 
  | E or E           { LogicN $1 "or" $3 $2}
  | E '==' E         { CompareN $1 "==" $3 $2}
  | E '/=' E         { CompareN $1 "/=" $3 $2 }
  | E '>=' E         { CompareN $1 ">=" $3 $2}
  | E '<=' E         { CompareN $1 "<=" $3 $2}
  | E '>' E          { CompareN $1 ">" $3 $2}
  | E '<' E          { CompareN $1 "<" $3 $2}
  | id '[' E ']'     { GetArrayItem (tokenValue $1) $3 }
  | '(' E ')'        { ParentExp $2 $1 }
  | n            { IntegerLiteralN (tokenValue $1) (tokenPos $1) }
  | f            { FloatLiteralN (tokenValue $1) (tokenPos $1) }
  | c            { CharLiteralN (tokenValue $1) (tokenPos $1) }
  | s            { StringLiteralN (tokenValue $1) (tokenPos $1) }
  | on           { TrueN (tokenPos $1) }
  | off          { FalseN (tokenPos $1) }



{


happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where
  lcn = case tks of
          []    -> "end of file"
          ((Token (AlexPn _ l c) _):_)  -> "line " ++ show l ++ ", column " ++ show c
}
