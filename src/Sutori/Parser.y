{
module Sutori.Parser (parseSut) where

import Data.Maybe


import Sutori.Lexer.Tokens(SutToken(SutTkEOF))
import Sutori.Monad(SutMonad)
import Sutori.Monad.Logger(SutError(..), logError)

import Sutori.Parser.Instructions
import Sutori.Parser.Expressions
import Sutori.Parser.Declarations

}

%name      parseSut
%tokentype { SutToken }
%monad     { SutMonad }
%lexer     { lexwrap } { SutTkEOF }
%error     { logError GrammaticalError }

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
Source :: { SutMonad SutModule }
Source            : InitModule EOF
                  {% beforeStart $1 }



-- Modules
-- ================================================================================================
InitModule :: { SutMonad SutModule }
InitModule        : PROGRAM_INI ID Block PROGRAM_FIN {% defModule $1 $2 }



-- Expressions
-- ===============================================================================================
Expression :: { SutExpression }
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

Literal :: { SutExpression }
Literal           : LITERAL_INT        { literalInt    $1 }
                  | LITERAL_BOOL       { literalBool   $1 }
                  | LITERAL_CHAR       { literalChar   $1 }
                  | LITERAL_FLOAT      { literalFloat  $1 }
                  | LITERAL_STRING     { literalString $1 }

Assignable :: { SutExpression }
Assignable        : VariableID     { $1 }
                  | GetArrayItem   { $1 }
                  | GetMember      { $1 }
                  | Dereference    { $1 }


-- Structures
---------------------------------------------------------------------------------------------------
Array :: { SutMonad SutExpression }
Array             : '[' ArrayList ']'                 {% constructArray $2 }

ArrayList         : ArrayList_                        { reverse $1 }
ArrayList_        : Expression                        { [ $1 ] }
                  | ArrayList ';' Expression          { $3 : $1 }

Struct :: { SutMonad SutExpression }
Struct            : '{' StructList '}'                {% constructStruct $2 }

StructList        : StructList_                       { reverse $1 }
StructList_       : ID ':' Expression                 { [ ($1, $3) ] }
                  | StructList  ';' ID ':' Expression { ($3, $5) : $1 }


-- Operations
---------------------------------------------------------------------------------------------------
UnaryOp :: { SutMonad SutExpression }
UnaryOp           : '+' NumericExpr %prec POS   { unaryPlus   $2 }
                  | '-' NumericExpr %prec NEG   { unaryMinus  $2 }
                  | '!' BooleanExpr             { unaryNot    $2 }


BinaryOp :: { SutMonad SutExpression }
BinaryOp          : NumericExpr '+' NumericExpr  { opAddition       $1 $3 }
                  | NumericExpr '-' NumericExpr  { opSubstraction   $1 $3 }
                  | NumericExpr '*' NumericExpr  { opMultiplication $1 $3 }
                  | NumericExpr '%' NumericExpr  { opModulo         $1 $3 }
                  | NumericExpr '/' NumericExpr  { opDivision       $1 $3 }
                  | NumericExpr div NumericExpr  { opIntDivision    $1 $3 }
                  | NumericExpr '^' NumericExpr  { opPower          $1 $3 }

                  | BooleanExpr and  BooleanExpr { opAnd          $1 $3 }
                  | BooleanExpr or   BooleanExpr { opOr           $1 $3 }
                  | Expression  '==' Expression  { opEqual        $1 $3 }
                  | Expression  '/=' Expression  { opNotEqual     $1 $3 }
                  | NumericExpr '>=' NumericExpr { opGreaterEqual $1 $3 }
                  | NumericExpr '<=' NumericExpr { opLessEqual    $1 $3 }
                  | NumericExpr '>'  NumericExpr { opGreater      $1 $3 }
                  | NumericExpr '<'  NumericExpr { opLess         $1 $3 }


-- Complex operations
-- Dereference, Assignment, GetArrayItem, GetMember, NewPointer, Call :: { SutMonad SutExpression }

Dereference       : '*' Expression %prec IND      {% dereference $2 }
Assignment        : Assignable '=' Expression     {% assignment $1 $3 }
GetArrayItem      : Assignable '[' IndexExpr ']'  {% arrayGet   $1 $3 }
GetMember         : Assignable '->' ID            {% memberGet  $1 $3 }

NewPointer        : PersonID S_madea TypeExpr     {% createPointer $1 $3 }

Call              : FunctionID '(' WithParams ')' %prec PAR {% functionCall $1 $3 }

WithParams        : WITH CallParams               { reverse $2 }
                  | {-empty-}                     { [] }

CallParams        : Expression                    { [$1]  }
                  | CallParams ',' Expression     { $1:$3 }



-- Declarations
-- ================================================================================================
Declaration       : PersonDef    { $1 }
                  | FunctionDef  { $1 }
                  | VariableDef  { $1 }
                  | TypeDef      { $1 }

PersonDef         : S_therewas PersonNames {% mapM_ defPerson $2 }

PersonNames       : PersonNames_           { reverse $1 }
PersonNames_      : ID                     { [$1] }
                  | ID ',' PersonNames     { $1:$3 }
                  | ID and PersonNames     { $1:$3 }

FunctionDef       : FUNCTION_INI addFunctionID ',' FunctionWithP FUNCTION_FIN    { }
                  | FUNCTION_INI addFunctionID ',' FunctionWOutP FUNCTION_FIN    { }

FunctionWOutP     : S_therewasa TypeExpr BlockF                                {% defFunction' $2 $3 }
FunctionWithP     : S_therewasa TypeExpr '(' S_madeof pushParams ')' BlockF    {% defFunction' $2 $7 }
addFunctionID     : ID                                                     {% insertFunctionID $1 }
pushParams        : ParamsDef                                              {% mapM_ insertParam $1 }

ParamsDef         : ParamsDef_                       { reverse $1 }
ParamsDef_        : TypeExpr ID                      { [(SutParamVal, $1, $2)] }
                  | YOUR TypeExpr ID                 { [(SutParamRef, $2, $3)] }
                  | ParamsDef ',' TypeExpr ID        {  (SutParamVal, $3, $4) : $1 }
                  | ParamsDef ',' YOUR TypeExpr ID   {  (SutParamRef, $4, $5) : $1 }


VariableDef       : PersonID S_broughta TypeExpr ':' VariableList   {% mapM_ (defVariable $1 $3) $5 }

VariableList      : VariableList_                               { reverse $1 }
VariableList_     : VariableList ',' ID                         {  ($3, Nothing) : $1 }
                  | VariableList ',' ID '=' Expression          {  ($3, Just $5) : $1 }
                  | ID '=' Expression                           { [($1, Just $3)] }
                  | ID                                          { [($1, Nothing)] }

TypeDef           : PersonID S_invented ID ';' S_itsa TypeExpr  {% defType $1 $3 $6 }


-- Type Expressions
-- ================================================================================================
TypeExpr :: { SutTypeID }
TypeExpr          : TYPE_INT                                    {% createType (SutPrimitiveType SutBag) }
                  | TYPE_FLOAT                                  {% createType (SutPrimitiveType SutWallet) }
                  | TYPE_CHAR                                   {% createType (SutPrimitiveType SutLetter) }
                  | TYPE_BOOL                                   {% createType (SutPrimitiveType SutLight) }
                  | TYPE_STRING                                 {% createType (SutPrimitiveType SutPhrase) }
                  | TYPE_POINTER '(' TO TypeExpr ')'            {% createType (SutDirection $4) }
                  | TYPE_ARRAY '(' OF LITERAL_INT TypeExpr ')'  {% createType (SutChain $4 $5) }
                  | TYPE_STRUCT '(' WITH StructTyping ')'       {% createType (SutMachine $4) }
                  | TYPE_UNION '(' EITHER UnionTyping ')'       {% createType (SutThing $4) }
                  | TypeID                                      {% findType $1 }

StructTyping      : StructTyping_                               { reverse $1 }
StructTyping_     : TypeExpr ID                                 { [($1, $2)] }
                  | StructTyping and TypeExpr ID                {  ($2, $4) : $1 }

UnionTyping       : UnionTyping_                                { reverse $1 }
UnionTyping_      : TypeExpr ID                                 { [($1, $2)] }
                  | UnionTyping or TypeExpr ID                  {  ($2, $4) : $1 }


-- Blocks
-- ================================================================================================
InsertScope       : BLOCK_OPEN                                  {% insertScope }
RemoveScope       : BLOCK_CLOSE                                 {% removeScope }

Block             : InsertScope Statements RemoveScope          { $2 }

Statements        : Statements_                                 { reverse $1 }
Statements_       : Statements Statement                        { $2 : $1 }
                  | {-empty-}                                   { [] }

BlockF            : InsertScope FunctionBlockCont RemoveScope   { $3 }

FunctionBlockCont : FunctionBlockCont_                          { reverse $1 }
FunctionBlockCont_: FunctionBlockCont Statement                 { $2 : $1 }
                  | FunctionBlockCont Return                    { $2 : $1 }
                  | {-empty-}                                   { [] }

Statement         : Instruction                                 { $1 }
                  | Declaration                                 { $1 }

Return            : S_andthatswhere Expression S_comesfrom      { Return $2 }


-- Instructions
---------------------------------------------------------------------------------------------------
Instruction       : Assignment '.'     { InstAssignment $1 }
                  | FreePointer        { $1 }
                  | Print              { $1 }
                  | Read               { $1 }
                  | Selection          { $1 }
                  | IterationU         { $1 }
                  | IterationB         { $1 }

FreePointer       : PersonID S_brokea Assignable '.'                          { FreePointer $1 $3 }

Selection         : PersonID S_dreamsof Block WHEN Expression '.'             { Select $1 $3 $5 [] }
                  | PersonID S_dreamsof Block WHEN Expression OTHERWISE Block { Select $1 $3 $5 %7 }

IterationU        : PersonID S_keepsdreamingof BooleanExpr Block              { IterationU $1 $3 $4 }

IterationB        : Block PersonID S_toldthatstory IndexExpr TIMES '.'        { IterationU $2 $1 $4}

Print             : PersonID ':' Expression '.'                               { Print $1 }

Read              : Assignable '?'                                            { Read $1 }


-- Checks to the SymTable
---------------------------------------------------------------------------------------------------
PersonID          : ID         {% findPersonID $1 }
FunctionID        : ID         {% findFunctionID $1 }
TypeID            : ID         {% findTypeID $1 }
VariableID        : ID         {% findVariableID $1 }

IndexExpr         : Expression { checkIndex   $1 }
NumericExpr       : Expression { checkNumeric $1 }
BooleanExpr       : Expression { checkBoolean $1 }


{}
