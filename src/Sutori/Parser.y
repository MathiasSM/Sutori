{
module Sutori.Parser (parseSut) where

import Data.Maybe


import Sutori.Utils               (SutID)

import Sutori.Types               (SutType(..), SutTypeID)
import Sutori.Types.Primitives    (SutPrimitive(..))
import Sutori.AST                 (SutExpression(..), SutModule)

import Sutori.Lexer.Tokens        (SutToken(SutTkEOF))

import Sutori.Monad               (SutMonad)
import Sutori.Monad.Logger        (SutError(..), logError)

import Sutori.Parser.Definitions
import Sutori.Parser.Expressions
import Sutori.Parser.Instructions
import Sutori.Parser.Symbols
import Sutori.Parser.TypeCheck

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
-- ================================================================================================
Source            :: { SutMonad SutModule }
Source            : InitModule EOF {% beforeStart $1 }



-- Modules
-- ================================================================================================
InitModule        :: { SutMonad SutModule }
InitModule        : PROGRAM_INI ID BlockGlobal PROGRAM_FIN      {% defModule $1 $2 }



-- Blocks
-- ================================================================================================
InsertScope       :: { SutMonad () }
InsertScope       : BLOCK_OPEN                                  {% insertScope }

RemoveScope       :: { SutMonad () }
RemoveScope       : BLOCK_CLOSE                                 {% removeScope }

-- Local statements: We don't allow function or type definitions here
BlockLocal        :: { [SutInstruction] }
BlockLocal        : InsertScope LocalStatements RemoveScope     { $3 }

LocalStatements   :: { [SutInstruction] }
LocalStatements   : LocalStatements_                            { reverse $1 }

LocalStatements_  :: { [SutInstruction] }
LocalStatements_  : LocalStatements_ Instruction                { $2 : $1 }
                  | LocalStatements_ Return                     { $2 : $1 }
                  | LocalStatements_ VariableDef                { $1 }
                  | LocalStatements_ PersonDef                  { $1 }
                  | {- noop statement -}                        { [] }

-- GlobalStatements: We allow any kind of instruction and definition
BlockGlobal       :: { [SutInstruction] }
BlockGlobal       : InsertScope GlobalStatements RemoveScope    { $2 }

GlobalStatements  :: { [SutInstruction] }
GlobalStatements  : GlobalStatements_                           { reverse $1 }

GlobalStatements_ :: { [SutInstruction] }
GlobalStatements_ : GlobalStatements_ Instruction               { $2 : $1 }
                  | GlobalStatements_ PersonDef                 { $1 }
                  | GlobalStatements_ VariableDef               { $1 }
                  | GlobalStatements_ TypeDef                   { $1 }
                  | GlobalStatements_ FunctionDef               { $1 }
                  | {- noop statement -}                        { [] }



-- Expressions
-- ===============================================================================================
Expression        :: { SutExpression }
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

Literal           :: { SutExpression }
Literal           : LITERAL_INT        { literalInt    $1 }
                  | LITERAL_BOOL       { literalBool   $1 }
                  | LITERAL_CHAR       { literalChar   $1 }
                  | LITERAL_FLOAT      { literalFloat  $1 }
                  | LITERAL_STRING     { literalString $1 }

Assignable        :: { SutExpression }
Assignable        : VariableID         { $1 }
                  | GetArrayItem       { $1 }
                  | GetMember          { $1 }
                  | Dereference        { $1 }


-- Structures
---------------------------------------------------------------------------------------------------
Array             :: { SutMonad SutExpression }
Array             : '[' ArrayList ']'                 {% constructArray $2 }

ArrayList         :: { [SutExpression] }
ArrayList         : ArrayList_                        { reverse $1 }
ArrayList_        :: { [SutExpression] }
ArrayList_        : Expression                        { [ $1 ] }
                  | ArrayList ';' Expression          { $3 : $1 }

Struct            :: { SutMonad SutExpression }
Struct            : '{' StructList '}'                {% constructStruct $2 }

StructList        :: { [SutExpression] }
StructList        : StructList_                       { reverse $1 }
StructList_       :: { [SutExpression] }
StructList_       : ID ':' Expression                 { [ ($1, $3) ] }
                  | StructList  ';' ID ':' Expression { ($3, $5) : $1 }


-- Operations
---------------------------------------------------------------------------------------------------
UnaryOp           :: { SutMonad SutExpression }
UnaryOp           : '+' Expression %prec POS      { unaryPlus   $2 }
                  | '-' Expression %prec NEG      { unaryMinus  $2 }
                  | '!' Expression                { unaryNot    $2 }


BinaryOp          :: { SutMonad SutExpression }
BinaryOp          : Expression '+' Expression     { opAddition       $1 $3 }
                  | Expression '-' Expression     { opSubstraction   $1 $3 }
                  | Expression '*' Expression     { opMultiplication $1 $3 }
                  | Expression '%' Expression     { opModulo         $1 $3 }
                  | Expression '/' Expression     { opDivision       $1 $3 }
                  | Expression div Expression     { opIntDivision    $1 $3 }
                  | Expression '^' Expression     { opPower          $1 $3 }

                  | Expression and  Expression    { opAnd            $1 $3 }
                  | Expression or   Expression    { opOr             $1 $3 }
                  | Expression '==' Expression    { opEqual          $1 $3 }
                  | Expression '/=' Expression    { opNotEqual       $1 $3 }
                  | Expression '>=' Expression    { opGreaterEqual   $1 $3 }
                  | Expression '<=' Expression    { opLessEqual      $1 $3 }
                  | Expression '>'  Expression    { opGreater        $1 $3 }
                  | Expression '<'  Expression    { opLess           $1 $3 }


-- Complex operations
Dereference       :: { SutMonad SutExpression }
Dereference       : '*' Expression %prec IND                 {% dereference $2 }

Assignment        :: { SutMonad SutExpression }
Assignment        : Assignable '=' Expression                {% assignment $1 $3 }

GetArrayItem      :: { SutMonad SutExpression }
GetArrayItem      : Assignable '[' IndexExpr ']'             {% arrayGet   $1 $3 }

GetMember         :: { SutMonad SutExpression }
GetMember         : Assignable '->' ID                       {% memberGet  $1 $3 }

NewPointer        :: { SutMonad SutExpression }
NewPointer        : PersonID S_madea TypeExpr                {% createPointer $1 $3 }

Call              :: { SutMonad SutExpression }
Call              : FunctionID '(' WithParams ')' %prec PAR  {% functionCall $1 $3 }

WithParams        :: { [SutExpression] }
WithParams        : WITH CallParams                          { reverse $2 }
                  | {-no parameters-}                        { [] }

CallParams        :: { [SutExpression] }
CallParams        : Expression                               { [$1]  }
                  | CallParams ',' Expression                { $1:$3 }



-- Declarations
-- ================================================================================================

-- Person Definition
PersonDef         :: { SutMonad () }
PersonDef         : S_therewas PersonNames        {% mapM_ defPerson $2 }

PersonNames       :: { [SutID] }
PersonNames       : PersonNames_                  { reverse $1 }

PersonNames_      :: { [SutID] }
PersonNames_      : ID                            { [$1] }
                  | PersonNames ',' ID            { $3 : $1 }
                  | PersonNames and ID            { $3 : $1 }


-- Function Definition
FunctionDef       :: { () }
FunctionDef       : FUNCTION_INI NewFunctionID ',' FunctionWithP FUNCTION_FIN    { }
                  | FUNCTION_INI NewFunctionID ',' FunctionWOutP FUNCTION_FIN    { }

FunctionWOutP     :: { SutMonad () }
FunctionWOutP     : S_therewasa TypeExpr BlockLocal                              {% defFunction' $2 $3 }

FunctionWithP     :: { SutMonad () }
FunctionWithP     : S_therewasa TypeExpr '(' S_madeof ParamsDef ')' BlockLocal   {% defFunction' $2 $7 }

-- Function actions: Register ID and params
NewFunctionID     :: { SutMonad () }
NewFunctionID     : ID                                          {% insertFunctionID $1 }

ParamsDef         :: { SutMonad () }
ParamsDef         : ParamsDef_                                  {% mapM_ insertParam (reverse $1) }

ParamsDef_        :: { [(SutParamType, SutTypeID, SutID)] }
ParamsDef_        : TypeExpr ID                                 { [(SutParamVal, $1, $2)] }
                  | YOUR TypeExpr ID                            { [(SutParamRef, $2, $3)] }
                  | ParamsDef ',' TypeExpr ID                   {  (SutParamVal, $3, $4) : $1 }
                  | ParamsDef ',' YOUR TypeExpr ID              {  (SutParamRef, $4, $5) : $1 }


-- Variable Definition
VariableDef       :: { SutMonad () }
VariableDef       : PersonID S_broughta TypeExpr ':' VariableList   {% mapM_ (defVariable $1 $3) $5 }

VariableList      :: { [(SutID, Maybe SutExpression)] }
VariableList      : VariableList_                               { reverse $1 }

VariableList_     :: { [(SutID, Maybe SutExpression)] }
VariableList_     : VariableList ',' ID                         {  ($3, Nothing) : $1 }
                  | VariableList ',' ID '=' Expression          {  ($3, Just $5) : $1 }
                  | ID '=' Expression                           { [($1, Just $3)] }
                  | ID                                          { [($1, Nothing)] }


-- Type Definition (see Type Expressions, below)
TypeDef           :: { SutMonad () }
TypeDef           : PersonID S_invented ID ';' S_itsa TypeExpr  {% defType $1 $3 $6 }



-- Type Expressions
-- ================================================================================================
TypeExpr          :: { SutTypeID }
TypeExpr          : TYPE_INT                                    {% createType (SutPrimitiveType SutBag) }
                  | TYPE_FLOAT                                  {% createType (SutPrimitiveType SutWallet) }
                  | TYPE_CHAR                                   {% createType (SutPrimitiveType SutLetter) }
                  | TYPE_BOOL                                   {% createType (SutPrimitiveType SutLight) }
                  | TYPE_STRING                                 {% createType (SutPrimitiveType SutPhrase) }
                  | TYPE_POINTER '(' TO TypeExpr ')'            {% createType (SutDirection $4) }
                  | TYPE_ARRAY '(' OF LITERAL_INT TypeExpr ')'  {% createType (SutChain $4 $5) }
                  | TYPE_STRUCT '(' WITH TypeMapping ')'        {% createType (SutMachine $4) }
                  | TYPE_UNION '(' EITHER TypeMapping ')'       {% createType (SutThing $4) }
                  | TypeID                                      {% findType $1 }

-- List of (id, type) for union/structs
TypeMapping       :: { [(SutID, SutTypeID)] }
TypeMapping       : TypeMapping_                                { reverse $1 }

TypeMapping_      :: { [(SutID, SutTypeID)] }
TypeMapping_      : TypeExpr ID                                 { [($2, $1)] }
                  | TypeMapping_ or TypeExpr ID                 {  ($4, $2) : $1 }



-- Instructions
-- =================================================================================================
Instruction       :: { SutInstruction }
Instruction       : Assignment '.'     { InstAssignment $1 }
                  | FreePointer        { $1 }
                  | Print              { $1 }
                  | Read               { $1 }
                  | Selection          { $1 }
                  | IterationU         { $1 }
                  | IterationB         { $1 }

-- Flow-control
IterationU        :: { SutInstruction }
IterationU        : PersonID S_keepsdreamingof CondExpr BlockGlobal                     { IterationU $1 $3 $4 }

IterationB        :: { SutInstruction }
IterationB        : BlockGlobal PersonID S_toldthatstory IndexExpr TIMES '.'            { IterationU $2 $1 $4}

Selection         :: { SutInstruction }
Selection         : PersonID S_dreamsof BlockGlobal WHEN CondExpr '.'                   { Select $1 $3 $5 [] }
                  | PersonID S_dreamsof BlockGlobal WHEN CondExpr OTHERWISE BlockGlobal { Select $1 $3 $5 %7 }

-- TODO: Add break, continue, etc

-- IO
Print             :: { SutInstruction }
Print             : PersonID ':' PrintableExpr '.'              { Print $1 }

Read              :: { SutInstruction }
Read              : Assignable '?'                              { Read $1 }

-- Memory
FreePointer       :: { SutInstruction }
FreePointer       : PersonID S_brokea Assignable '.'            { FreePointer $1 $3 }

-- Function-only
Return            :: { SutInstruction }
Return            : S_andthatswhere Expression S_comesfrom      { Return $2 }


-- Checks to the SymTable
---------------------------------------------------------------------------------------------------
PersonID          :: { SutMonad SutID }
PersonID          : ID                            {% findPerson $1 }

FunctionID        :: { SutMonad SutID }
FunctionID        : ID                            {% findFunction $1 }

VariableID        :: { SutMonad SutID }
VariableID        : ID                            {% findVariable $1 }

TypeID            :: { SutMonad SutTypeID }
TypeID            : ID                            {% findTypeID $1 }

IndexExpr         :: { SutMonad SutExpression }
IndexExpr         : Expression                    {% checkIndex   $1 }

CondExpr          :: { SutMonad SutExpression }
CondExpr          : Expression                    {% checkBoolean $1 }

PrintableExpr     :: { SutMonad SutExpression }
PrintableExpr     : Expression                    {% checkPrintable $1 }


{}
