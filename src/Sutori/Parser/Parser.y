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
import Sutori.AST       (SutID, SutModule, SutAST, SutExpression(..), SutInstruction(..))
import Sutori.SymTable  (SutParam(..), paramByValue, paramByRef)

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
    FUNCTION_DEFINE     { FUNCTION_DEFINE  }
    FUNCTION_DECLARE    { FUNCTION_DECLARE }

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
Module            : PROGRAM_INI ID BlockGlobal PROGRAM_FIN '.'  {% defModule $2 $3 }



-- Blocks
-- ================================================================================================
InsertScope       :: { () }
InsertScope       : BLOCK_OPEN                                  {% insertScope }

RemoveScope       :: { () }
RemoveScope       : BLOCK_CLOSE                                 {% removeScope }

LocalStatements_  :: { [SutInstruction] }
LocalStatements_  : LocalStatements_ Instruction                { $2 : $1 }
                  | LocalStatements_ Return '.'                 { $2 : $1 }
                  | LocalStatements_ VariableDef '.'            { $2 ++ $1 }
                  | LocalStatements_ PersonDef '.'              { $1 }
                  | {- noop statement -}                        { [] }

-- GlobalStatements: We allow any kind of instruction and definition
BlockGlobal       :: { [SutInstruction] }
BlockGlobal       : InsertScope GlobalStatements_ RemoveScope   { reverse $2 }

GlobalStatements_ :: { [SutInstruction] }
GlobalStatements_ : GlobalStatements_ Instruction               { $2 : $1 }
                  | GlobalStatements_ PersonDef '.'             { $1 }
                  | GlobalStatements_ VariableDef '.'           { $2 ++ $1 }
                  | GlobalStatements_ TypeDef '.'               { $1 }
                  | GlobalStatements_ FunctionDef '.'           { $1 }
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
Literal           : LITERAL_INT        {% literalInt    $1 }
                  | LITERAL_BOOL       {% literalBool   $1 }
                  | LITERAL_CHAR       {% literalChar   $1 }
                  | LITERAL_FLOAT      {% literalFloat  $1 }
                  | LITERAL_STRING     {% literalString $1 }

Assignable        :: { SutExpression }
Assignable        : VariableID         { $1 }
                  | GetArrayItem       { $1 }
                  | GetMember          { $1 }
                  | Dereference        { $1 }


-- Structures
---------------------------------------------------------------------------------------------------
Array             :: { SutExpression }
Array             : '[' ArrayList_ ']'                 {% constructArray (reverse $2) }

ArrayList_        :: { [SutExpression] }
ArrayList_        : Expression                         { [ $1 ] }
                  | ArrayList_ ';' Expression          { $3 : $1 }

Struct            :: { SutExpression }
Struct            : '{' StructList_ '}'                {% constructStruct (reverse $2) }

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
PersonDef         : S_therewas PersonNames_       {% mapM_ defPerson (reverse $2) }

PersonNames_      :: { [SutID] }
PersonNames_      : ID                            { [$1] }
                  | PersonNames_ ',' ID           { $3 : $1 }
                  | PersonNames_ and ID           { $3 : $1 }


-- Function Definition
FunctionDef       :: { () }
FunctionDef       : FunctionDef_o '.' FUNCTION_DECLARE  { }
                  | FunctionDef_o LocalStatements_ RemoveScope FUNCTION_DEFINE  {% defineFunction $1 (reverse $2) }

FunctionDef_o     :: { SutID }
FunctionDef_o     : FUNCTION_INI ID ',' S_therewasa TypeExpr FunctionParams InsertScope
                  {% insertFunction $2 $5 $6 }

FunctionParams    :: { [SutParam] }
FunctionParams    : '(' S_madeof ParamsDef_ ')' { reverse $3 }
                  | {- No parameters -}         { []}

ParamsDef_        :: { [SutParam] }
ParamsDef_        : TypeExpr ID                                 { [paramByValue $1 $2] }
                  | YOUR TypeExpr ID                            { [paramByRef $2 $3] }
                  | ParamsDef_ ',' TypeExpr ID                  {  paramByValue $3 $4 : $1 }
                  | ParamsDef_ ',' YOUR TypeExpr ID             {  paramByRef $4 $5 : $1 }


-- Variable Definition
VariableDef       :: { [SutInstruction] }
VariableDef       : PersonID S_broughta TypeExpr ':' VariableList_   {% defVariables $1 $3 (reverse $5) }

VariableList_     :: { [(SutID, Maybe SutExpression)] }
VariableList_     : VariableList_ ',' ID                        {  ($3, Nothing) : $1 }
                  | VariableList_ ',' ID '=' Expression         {  ($3, Just $5) : $1 }
                  | ID '=' Expression                           { [($1, Just $3)] }
                  | ID                                          { [($1, Nothing)] }


-- Type Definition (see Type Expressions, below)
TypeDef           :: { () }
TypeDef           : TypeDef_o ';' S_itsa TypeExpr               {% defineType $1 $4 }

TypeDef_o         :: { SutID }
TypeDef_o         : PersonID S_invented ID                      {% insertType $1 $3 }



-- Type Expressions
-- ================================================================================================
TypeExpr          :: { SutTypeID }
TypeExpr          : TYPE_INT                                     {% findTypeID (SutPrimitiveType SutBag) }
                  | TYPE_FLOAT                                   {% findTypeID (SutPrimitiveType SutWallet) }
                  | TYPE_CHAR                                    {% findTypeID (SutPrimitiveType SutLetter) }
                  | TYPE_VOID                                    {% findTypeID (SutPrimitiveType SutTypeVoid) }
                  | TYPE_BOOL                                    {% findTypeID (SutPrimitiveType SutLight) }
                  | TYPE_STRING                                  {% findTypeID (SutPrimitiveType SutPhrase) }
                  | TYPE_POINTER '(' TO TypeExpr ')'             {% findTypeID (SutDirection $4) }
                  | TYPE_ARRAY   '(' OF LITERAL_INT TypeExpr ')' {% findTypeID (SutChain $4 $5) }
                  | TYPE_STRUCT  '(' WITH StructMapping_ ')'     {% findTypeID (SutMachine (reverse $4)) }
                  | TYPE_UNION   '(' EITHER UnionMapping_ ')'    {% findTypeID (SutThing   (reverse $4)) }
                  | TypeID                                       { $1 }

UnionMapping_     :: { [(SutID, SutTypeID)] }
UnionMapping_     : TypeExpr ID                                 { [($2, $1)] }
                  | UnionMapping_ or TypeExpr ID                 {  ($4, $3) : $1 }

StructMapping_    :: { [(SutID, SutTypeID)] }
StructMapping_    : TypeExpr ID                                 { [($2, $1)] }
                  | StructMapping_ and TypeExpr ID                 {  ($4, $3) : $1 }



-- Instructions
-- =================================================================================================
Instruction       :: { SutInstruction }
Instruction       : Assignment '.'     { InstExpression $1 }
                  | Call '.'           { InstExpression $1 }
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
FunctionID        : ID                            {% findFunctionID $1 }

TypeID            :: { SutTypeID }
TypeID            : ID                            {% findType $1 }

VariableID        :: { SutExpression }
VariableID        : ID                            {% findVariable $1 }

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
