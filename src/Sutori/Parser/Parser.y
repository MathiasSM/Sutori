{
{-|
Description : Happy-generated parser for Sutori language
-}
module Sutori.Parser.Parser
( parseModule
, parseExpression
, parseType
) where

import Data.Maybe


import Sutori.Types     (SutType(..), SutTypeID, SutPrimitive(..))
import Sutori.AST       (SutID, SutModule, SutBlock, SutExpression(..), SutInstruction(..))
import Sutori.SymTable  (SutParamKind(SutVal, SutRef))

import Sutori.Lexer     (SutToken(..), lexwrap)

import Sutori.Monad     (SutMonad, insertScope, removeScope)
import Sutori.Error     (SutError(..), parserError)

import Sutori.Parser.Definitions
import Sutori.Parser.Expressions
import Sutori.Parser.Symbols
import Sutori.Parser.TypeCheck

}

%name      parseModule_     Module
%name      parseExpression_ Expression
%name      parseType_       TypeExpr
%tokentype { SutToken }
%monad     { SutMonad }
%lexer     { lexwrap } { SutTkEOF }
%error     { parserError }

%token
    BLOCK_OPEN          { BLOCK_OPEN }
    BLOCK_CLOSE         { BLOCK_CLOSE }

    PROGRAM_INI         { PROGRAM_INI }
    PROGRAM_FIN         { PROGRAM_FIN }
    FUNCTION_INI        { FUNCTION_INI }
    FUNCTION_FIN        { FUNCTION_FIN }

    S_andthatswhere     { S_andthatswhere }
    S_therewas          { S_therewas }
    S_brokea            { S_brokea }
    S_broughta          { S_broughta }
    S_comesfrom         { S_comesfrom }
    S_dreamsof          { S_dreamsof }
    S_keepsdreamingof   { S_keepsdreamingof }
    S_madeof            { S_madeof }
    S_madea             { S_madea }
    S_invented          { S_invented }
    S_therewasa         { S_therewasa }
    S_toldthatstory     { S_toldthatstory }
    S_itsa              { S_itsa }
    S_wewillskipthis    { S_wewillskipthis }
    S_andnothingelse    { S_andnothingelse }

    TYPE_INT            { TYPE_INT }
    TYPE_FLOAT          { TYPE_FLOAT }
    TYPE_CHAR           { TYPE_CHAR }
    TYPE_BOOL           { TYPE_BOOL }
    TYPE_VOID           { TYPE_VOID }
    TYPE_ARRAY          { TYPE_ARRAY }
    TYPE_STRUCT         { TYPE_STRUCT }
    TYPE_UNION          { TYPE_UNION }
    TYPE_STRING         { TYPE_STRING }
    TYPE_POINTER        { TYPE_POINTER }

    '('                 { OPEN_PAREN }
    '['                 { OPEN_BRACKETS }
    '{'                 { OPEN_BRACES }
    ')'                 { CLOSE_PAREN }
    ']'                 { CLOSE_BRACKETS }
    '}'                 { CLOSE_BRACES }
    '.'                 { PERIOD }
    ','                 { COMMA }
    ':'                 { COLON }
    ';'                 { SEMICOLON }
    '!'                 { EXCLAMATION }
    '?'                 { QUESTIONMARK }
    '->'                { ARROW_RIGHT }
    '+'                 { PLUS }
    '-'                 { MINUS }
    '=='                { EQUAL }
    '='                 { ASSIGNMENT }
    '*'                 { ASTERISK }
    '%'                 { PERCENT }
    '/'                 { SLASH }
    div                 { DIV }
    '/='                { NOT_EQUAL }
    '>='                { GREATER_EQUAL }
    '<='                { LESS_EQUAL }
    '>'                 { GREATER }
    '<'                 { LESS }
    '^'                 { POWER }
    and                 { AND }
    or                  { OR }
    WITH                { WITH }
    YOUR                { YOUR }
    OF                  { OF }
    EITHER              { EITHER }
    TO                  { TO }
    WHEN                { WHEN }
    OTHERWISE           { OTHERWISE }
    TIMES               { TIMES }

    LITERAL_BOOL        { SutTkBool $$ }
    LITERAL_CHAR        { SutTkChar $$ }
    LITERAL_FLOAT       { SutTkFloat $$ }
    LITERAL_INT         { SutTkInt $$ }
    LITERAL_STRING      { SutTkString $$ }
    ID                  { SutTkID $$ }


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


-- Modules
-- ================================================================================================
Module            :: { () }
Module            : PROGRAM_INI ID BlockGlobal PROGRAM_FIN      {% defModule $2 $3 }



-- Blocks
-- ================================================================================================
InsertScope       :: { () }
InsertScope       : BLOCK_OPEN                                  {% insertScope }

RemoveScope       :: { () }
RemoveScope       : BLOCK_CLOSE                                 {% removeScope }

-- Local statements: We don't allow function or type definitions here
BlockLocal        :: { [SutInstruction] }
BlockLocal        : InsertScope LocalStatements RemoveScope     { $2 }

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
Array             :: { SutExpression }
Array             : '[' ArrayList ']'                  {% constructArray $2 }

ArrayList         :: { [SutExpression] }
ArrayList         : ArrayList_                         { reverse $1 }
ArrayList_        :: { [SutExpression] }
ArrayList_        : Expression                         { [ $1 ] }
                  | ArrayList_ ';' Expression          { $3 : $1 }

Struct            :: { SutExpression }
Struct            : '{' StructList '}'                 {% constructStruct $2 }

StructList        :: { [(SutID, SutExpression)] }
StructList        : StructList_                        { reverse $1 }
StructList_       :: { [(SutID, SutExpression)] }
StructList_       : ID ':' Expression                  { [ ($1, $3) ] }
                  | StructList_  ';' ID ':' Expression { ($3, $5) : $1 }


-- Operations
---------------------------------------------------------------------------------------------------
UnaryOp           :: { SutExpression }
UnaryOp           : '+' Expression %prec POS      {% unaryPlus   $2 }
                  | '-' Expression %prec NEG      {% unaryMinus  $2 }
                  | '!' Expression                {% unaryNot    $2 }


BinaryOp          :: { SutExpression }
BinaryOp          : Expression '+' Expression     {% opAddition       $1 $3 }
                  | Expression '-' Expression     {% opSubstraction   $1 $3 }
                  | Expression '*' Expression     {% opMultiplication $1 $3 }
                  | Expression '%' Expression     {% opModulo         $1 $3 }
                  | Expression '/' Expression     {% opDivision       $1 $3 }
                  | Expression div Expression     {% opIntDivision    $1 $3 }
                  | Expression '^' Expression     {% opPower          $1 $3 }

                  | Expression and  Expression    {% opAnd            $1 $3 }
                  | Expression or   Expression    {% opOr             $1 $3 }
                  | Expression '==' Expression    {% opEqual          $1 $3 }
                  | Expression '/=' Expression    {% opNotEqual       $1 $3 }
                  | Expression '>=' Expression    {% opGreaterEqual   $1 $3 }
                  | Expression '<=' Expression    {% opLessEqual      $1 $3 }
                  | Expression '>'  Expression    {% opGreater        $1 $3 }
                  | Expression '<'  Expression    {% opLess           $1 $3 }


-- Complex operations
Dereference       :: { SutExpression }
Dereference       : '*' Expression %prec IND                 {% dereference $2 }

Assignment        :: { SutExpression }
Assignment        : Assignable '=' Expression                {% assignment $1 $3 }

GetArrayItem      :: { SutExpression }
GetArrayItem      : Assignable '[' IndexExpr ']'             {% arrayGet   $1 $3 }

GetMember         :: { SutExpression }
GetMember         : Assignable '->' ID                       {% memberGet  $1 $3 }

NewPointer        :: { SutExpression }
NewPointer        : PersonID S_madea TypeExpr                {% createPointer $1 $3 }

Call              :: { SutExpression }
Call              : FunctionID '(' WithParams ')' %prec PAR  {% functionCall $1 $3 }

WithParams        :: { [SutExpression] }
WithParams        : WITH CallParams                          { reverse $2 }
                  | {-no parameters-}                        { [] }

CallParams        :: { [SutExpression] }
CallParams        : Expression                               { [$1]  }
                  | CallParams ',' Expression                { $3 : $1 }



-- Declarations
-- ================================================================================================

-- Person Definition
PersonDef         :: { () }
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

FunctionWOutP     :: { () }
FunctionWOutP     : S_therewasa TypeExpr BlockLocal                              {% defFunction $2 $3 }

FunctionWithP     :: { () }
FunctionWithP     : S_therewasa TypeExpr '(' S_madeof ParamsDef ')' BlockLocal   {% defFunction' $2 $7 }

-- Function actions: Register ID and params
NewFunctionID     :: { SutID }
NewFunctionID     : ID                                          {% insertFunctionID $1 }

ParamsDef         :: { () }
ParamsDef         : ParamsDef_                                  {% mapM_ insertParam (reverse $1) }

ParamsDef_        :: { [(SutParamKind, SutTypeID, SutID)] }
ParamsDef_        : TypeExpr ID                                 { [(SutVal, $1, $2)] }
                  | YOUR TypeExpr ID                            { [(SutRef, $2, $3)] }
                  | ParamsDef_ ',' TypeExpr ID                  {  (SutVal, $3, $4) : $1 }
                  | ParamsDef_ ',' YOUR TypeExpr ID             {  (SutRef, $4, $5) : $1 }


-- Variable Definition
VariableDef       :: { () }
VariableDef       : PersonID S_broughta TypeExpr ':' VariableList   {% mapM_ (defVariable $1 $3) $5 }

VariableList      :: { [(SutID, Maybe SutExpression)] }
VariableList      : VariableList_                               { reverse $1 }

VariableList_     :: { [(SutID, Maybe SutExpression)] }
VariableList_     : VariableList_ ',' ID                        {  ($3, Nothing) : $1 }
                  | VariableList_ ',' ID '=' Expression         {  ($3, Just $5) : $1 }
                  | ID '=' Expression                           { [($1, Just $3)] }
                  | ID                                          { [($1, Nothing)] }


-- Type Definition (see Type Expressions, below)
TypeDef           :: { () }
TypeDef           : PersonID S_invented ID ';' S_itsa TypeExpr  {% defType $1 $3 $6 }



-- Type Expressions
-- ================================================================================================
TypeExpr          :: { SutTypeID }
TypeExpr          : TYPE_INT                                    {% findTypeID (SutPrimitiveType SutBag) }
                  | TYPE_FLOAT                                  {% findTypeID (SutPrimitiveType SutWallet) }
                  | TYPE_CHAR                                   {% findTypeID (SutPrimitiveType SutLetter) }
                  | TYPE_BOOL                                   {% findTypeID (SutPrimitiveType SutLight) }
                  | TYPE_STRING                                 {% findTypeID (SutPrimitiveType SutPhrase) }
                  | TYPE_POINTER '(' TO TypeExpr ')'            {% findTypeID (SutDirection $4) }
                  | TYPE_ARRAY '(' OF LITERAL_INT TypeExpr ')'  {% findTypeID (SutChain $4 $5) }
                  | TYPE_STRUCT '(' WITH TypeMapping ')'        {% findTypeID (SutMachine $4) }
                  | TYPE_UNION '(' EITHER TypeMapping ')'       {% findTypeID (SutThing $4) }
                  | TypeID                                      {% return $1 }

-- List of (id, type) for union/structs
TypeMapping       :: { [(SutID, SutTypeID)] }
TypeMapping       : TypeMapping_                                { reverse $1 }

TypeMapping_      :: { [(SutID, SutTypeID)] }
TypeMapping_      : TypeExpr ID                                 { [($2, $1)] }
                  | TypeMapping_ or TypeExpr ID                 {  ($4, $3) : $1 }



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
                  | Break              { $1 }
                  | Continue           { $1 }

-- Flow-control
IterationU        :: { SutInstruction }
IterationU        : PersonID S_keepsdreamingof CondExpr BlockGlobal                     { IterationU $1 $3 $4 }

IterationB        :: { SutInstruction }
IterationB        : BlockGlobal PersonID S_toldthatstory IndexExpr TIMES '.'            { IterationB $2 $4 $1}

Selection         :: { SutInstruction }
Selection         : PersonID S_dreamsof BlockGlobal WHEN CondExpr '.'                   { Selection $1 $5 $3 [] }
                  | PersonID S_dreamsof BlockGlobal WHEN CondExpr OTHERWISE BlockGlobal { Selection $1 $5 $3 $7 }

Break             :: { SutInstruction }
Break             : S_andnothingelse                                                    { Break }

Continue          :: { SutInstruction }
Continue          : S_wewillskipthis                                                    { Continue }

-- Memory
FreePointer       :: { SutInstruction }
FreePointer       : PersonID S_brokea Assignable '.'            { FreePointer $1 $3 }

-- IO
Print             :: { SutInstruction }
Print             : PersonID ':' PrintableExpr '.'              { PrintVal $1 $3 }

Read              :: { SutInstruction }
Read              : PersonID ':' Assignable '?'                 { ReadVal $1 $3 }

-- Function-only
Return            :: { SutInstruction }
Return            : S_andthatswhere Expression S_comesfrom      { ReturnVal $2 }


-- Checks to the SymTable
---------------------------------------------------------------------------------------------------
PersonID          :: { SutID }
PersonID          : ID                            {% findPerson $1 }

FunctionID        :: { SutID }
FunctionID        : ID                            {% findFunction $1 }

VariableID        :: { SutExpression }
VariableID        : ID                            {% findVariable $1 }

TypeID            :: { SutTypeID }
TypeID            : ID                            {% findType $1 }

IndexExpr         :: { SutExpression }
IndexExpr         : Expression                    { checkIndex $1 }

CondExpr          :: { SutExpression }
CondExpr          : Expression                    { checkBoolean $1 }

PrintableExpr     :: { SutExpression }
PrintableExpr     : Expression                    { checkPrintable $1 }


{

-- |Parses an entire Sutori file (module)
parseModule     = parseModule_

-- |Parses a Sutori expression
parseExpression = parseExpression_

-- |Parses a Sutori type expression
parseType       = parseType_

}
