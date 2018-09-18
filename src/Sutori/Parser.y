{
module Sutori.Parser (parseSut) where

import Data.Maybe

import Sutori.Lexer
import Sutori.Monad
import Sutori.Logger
import Sutori.Instructions(..)
import Sutori.Expressions(..)
import Sutori.Declarations(..)
import Sutori.TypeConstruction(..)

}

%name      parseSut
%tokentype { SutToken }
%monad     { SutMonad }
%lexer     { lexwrap } { SutTkEOF }
%error     { sutoriError SutParserError }

%token
    EOF                 { $$ }

    BLOCK_OPEN          { $$ }
    BLOCK_CLOSE         { $$ }

    PROGRAM_INI         { $$ }
    PROGRAM_FIN         { $$ }
    FUNCTION_INI        { $$ }
    FUNCTION_FIN        { $$ }

    S_andthatswhere     { $$ }
    S_therewas          { $$ }
    S_brokea            { $$ }
    S_broughta          { $$ }
    S_comesfrom         { $$ }
    S_dreamsof          { $$ }
    S_keepsdreamingof   { $$ }
    S_madeof            { $$ }
    S_madea             { $$ }
    S_invented          { $$ }
    S_therewasa         { $$ }
    S_toldthatstory     { $$ }
    S_itsa              { $$ }

    TYPE_INT            { $$ }
    TYPE_FLOAT          { $$ }
    TYPE_CHAR           { $$ }
    TYPE_BOOL           { $$ }
    TYPE_ARRAY          { $$ }
    TYPE_STRUCT         { $$ }
    TYPE_UNION          { $$ }
    TYPE_STRING         { $$ }
    TYPE_POINTER        { $$ }

    '('                 { $$ }
    '['                 { $$ }
    '{'                 { $$ }
    ')'                 { $$ }
    ']'                 { $$ }
    '}'                 { $$ }
    '.'                 { $$ }
    ','                 { $$ }
    ':'                 { $$ }
    ';'                 { $$ }
    '!'                 { $$ }
    '?'                 { $$ }
    '->'                { $$ }
    '+'                 { $$ }
    '-'                 { $$ }
    '=='                { $$ }
    '='                 { $$ }
    '*'                 { $$ }
    '%'                 { $$ }
    '/'                 { $$ }
    div                 { $$ }
    '/='                { $$ }
    '>='                { $$ }
    '<='                { $$ }
    '>'                 { $$ }
    '<'                 { $$ }
    '^'                 { $$ }
    and                 { $$ }
    or                  { $$ }
    WITH                { $$ }
    YOUR                { $$ }
    OF                  { $$ }
    EITHER              { $$ }
    TO                  { $$ }
    WHEN                { $$ }
    OTHERWISE           { $$ }
    TIMES               { $$ }

    LITERAL_BOOL        { $$ }
    LITERAL_CHAR        { $$ }
    LITERAL_FLOAT       { $$ }
    LITERAL_INT         { $$ }
    LITERAL_STRING      { $$ }
    ID                  { $$ }


%right    ASG
%right    '='
%left     or
%left     and
%left     '==' '/='
%nonassoc '>'  '<'  '>=' '<='
%left     '+'  '-'
%left     '*'  '/'  '%'  div
%left     '^'
%right    '!'  IND  POS  NEG
%left     '[' '->' PAR

%%


-- Program
---------------------------------------------------------------------------------------------------
Source            : InitModule EOF
                  {% beforeStart $1 }



-- Modules
-- ================================================================================================
InitModule        : PROGRAM_INI ID Block PROGRAM_FIN {% defModule $1 $2 }



-- Expressions
-- ===============================================================================================
Expression        : Assignable %prec ASG { $1 }
                  | Assignment           { $1 }
                  | Literal              { $1 }
                  | Array                { $1 }
                  | Struct               { $1 }
                  | UnaryOp              { $1 }
                  | BinaryOp             { $1 }
                  | Call                 { $1 }
                  | NewPointer           { $1 }
                  | '(' Expression ')'   { $2 }

Literal           : LITERAL_INT        { literalInt    $1 }
                  | LITERAL_BOOL       { literalBool   $1 }
                  | LITERAL_CHAR       { literalChar   $1 }
                  | LITERAL_FLOAT      { literalFloat  $1 }
                  | LITERAL_STRING     { literalString $1 }

Assignable        : ID             { $1 }
                  | GetArrayItem   { $1 }
                  | GetMember      { $1 }
                  | Dereference    { $1 }


-- Structures
---------------------------------------------------------------------------------------------------
Array             : '[' ArrayList ']'                 {% constructedArray $2 }
ArrayList         : Expression                        { [ $1 ] }
                  | Expression ';' ArrayList          { $1 : $3 }

Struct            : '{' StructList '}'                {% constructedStruct $2 }
StructList        : ID ':' Expression                 { [ ($1, $3) ] }
                  | ID ':' Expression ';' StructList  { ($1, $3): $5 }


-- Operations
---------------------------------------------------------------------------------------------------
UnaryOp           : '+' Expression %prec POS   { unaryPlus   $2 }
                  | '-' Expression %prec NEG   { unaryMinus  $2 }
                  | '!' Expression             { unaryNot    $2 }


BinaryOp          : Expression '+' Expression  { opAddition       $1 $3 }
                  | Expression '-' Expression  { opSubstraction   $1 $3 }
                  | Expression '*' Expression  { opMultiplication $1 $3 }
                  | Expression '%' Expression  { opModulo         $1 $3 }
                  | Expression '/' Expression  { opDivision       $1 $3 }
                  | Expression div Expression  { opIntDivision    $1 $3 }
                  | Expression '^' Expression  { opPower          $1 $3 }

                  | Expression and  Expression { opAnd          $1 $3 }
                  | Expression or   Expression { opOr           $1 $3 }
                  | Expression '==' Expression { opEqual        $1 $3 }
                  | Expression '/=' Expression { opNotEqual     $1 $3 }
                  | Expression '>=' Expression { opGreaterEqual $1 $3 }
                  | Expression '<=' Expression { opLessEqual    $1 $3 }
                  | Expression '>'  Expression { opGreater      $1 $3 }
                  | Expression '<'  Expression { opLess         $1 $3 }


-- Compex operations
Dereference       : '*' Expression %prec IND      {% dereference $2 }
Assignment        : Assignable '=' Expression     {% assignment $1 $3 }
GetArrayItem      : Assignable '[' Expression ']' {% arrayGet   $1 $3 }
GetMember         : Assignable '->' ID            {% memberGet  $1 $3 }

NewPointer        : PersonID S_madea Type         {}

Call              : ID '(' WithCallParams ')' %prec PAR {% functionCall $1 $3 }
WithCallParams    : WITH CallParams               { $2 }
                  | {-empty-}                     { [] }
CallParams        : Expression                    { [$1]  }
                  | Expression ',' CallParams     { $1:$3 }



-- Declaration
-- ================================================================================================
Declaration       : PersonDef    { $1 }
                  | FunctionDef  { $1 }
                  | VariableDef  { $1 }
                  | TypeDef      { $1 }


PersonDef         : S_therewas PersonNames {% mapM_ defPerson $2 }
PersonNames       : ID                     { [$1] }
                  | ID ',' PersonNames     { $1:$3 }
                  | ID and PersonNames     { $1:$3 }

FunctionDef       : FUNCTION_INI addFunctionID ',' FunctionWithP FUNCTION_FIN    { }
                  | FUNCTION_INI addFunctionID ',' FunctionWOutP FUNCTION_FIN    { }

FunctionWOutP     : S_therewasa Type BlockF                                {% defFunction' $2 $3 }
FunctionWithP     : S_therewasa Type '(' S_madeof pushParams ')' BlockF    {% defFunction' $2 $7 }
addFunctionID     : ID                                                     {% insertFunctionID $1 }
pushParams        : ParamsDef                                              {% mapM_ insertParam $1 }

ParamsDef         : Type ID                    { [(SutParamVal, $1, $2)] }
                  | Type ID ',' ParamsDef      { (SutParamVal, $1,$2):$4 }
                  | YOUR Type ID               { [(SutParamRef, $2, $3)] }
                  | YOUR Type ID ',' ParamsDef { (SutParamVal, $2, $3):$5 }


VariableDef       : ID S_broughta Type ':' VariableList   {% mapM_ (defVariable $1 $3) $5 }

VariableList      : ID ',' VariableList                   { ($1,Nothing):$3 }
                  | ID '=' Expression ',' VariableList    { ($1,Just $3):$5 }
                  | ID '=' Expression                     { [($1,Just $3)] }
                  | ID                                    { [($1,Nothing)] }

TypeDef           : ID S_invented ID ';' S_itsa Type      {% defType $1 $3 $6 }


-- Types
-- ====================================================================================================================
Type              : TYPE_INT                                    { SutTypeInt }
                  | TYPE_FLOAT                                  { SutTypeFloat }
                  | TYPE_CHAR                                   { SutTypeChar }
                  | TYPE_BOOL                                   { SutTypeBool }
                  | TYPE_STRING                                 { SutTypeString }
                  | TYPE_ARRAY '(' OF LITERAL_INT Type ')'      { SutTypeArray $4 $5 }
                  | TYPE_STRUCT '(' WITH StructTyping ')'       { SutTypeStruct $4 }
                  | TYPE_UNION '(' EITHER UnionTyping ')'       { SutTypeUnion $4 }
                  | TYPE_POINTER '(' TO Type ')'                { SutTypePointer $4 }
                  | ID                                          { % checkId' $1 TypeSym >>= getType }

StructTyping      : Type ID                                     { [($1,$2)] }
                  | Type ID and StructTyping                    { ($1,$2):$4 }

UnionTyping       : Type ID                                     { [($1,$2)] }
                  | Type ID or UnionTyping                      { ($1,$2):$4 }


-- Blocks
-- ====================================================================================================================
InsertScope       : BLOCK_OPEN                                  { % insertScope }
RemoveScope       : BLOCK_CLOSE                                 { % removeScope }

Block             : InsertScope Statements RemoveScope           { $3 }

Statements        : Statement Statements                      { $1: $2 }
                  | {-empty-}                                   { [] }

BlockF            : InsertScope FunctionBlockCont RemoveScope   { $3 }

FunctionBlockCont : Statement FunctionBlockCont              { $1:$2 }
                  | Return FunctionBlockCont        { $1:$2 }
                  | {-empty-}                                   { [] }

Statement         : Instruction                                 { $1 }
                  | Declaration                                 { $1 }

Return            : S_andthatswhere Expression S_comesfrom      { SutReturn $2 }


-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction       : Assignment '.'           { SutInstExpression $1 }
                  | FreePointer        { $1 }
                  | Print              { $1 }
                  | Read              { $1 }
                  | Selection              { $1 }
                  | IterationU     { $1 }
                  | IterationB       { $1 }

FreePointer       : PersonID S_brokea Assignable '.'
                  { % checkId' $2 PersonSym >> return (SutFreePointer $1 $3) }

Selection         : PersonID S_dreamsof Block WHEN Expression '.'
                  { % checkConditionalType $5 >> return (SutSelection $1 $3 $5 []) }
                  | PersonID S_dreamsof Block WHEN Expression OTHERWISE Block
                  { % checkConditionalType $5 >> return (SutSelection $1 $3 $5 $7) }

PersonID          : ID
                  { % checkId' $1 PersonSym >> return $1 }

IterationU        : PersonID S_keepsdreamingof Expression Block
                  { % checkConditionalType $3 >> return (SutIterationU $1 $3 $4) }

IterationB        : Block PersonID S_toldthatstory Expression TIMES '.'
                  { % checkIndexType $4 >> return (SutIterationB $2 $4 $1) }


Print             : PersonID ':' Expression '.'
                  { SutPrintVal $1 $3 }
Read              : Assignable '?'
                  { SutReadVal $1 }




{}
