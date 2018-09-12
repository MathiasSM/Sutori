{
module Sutori.Parser where

import Data.Maybe

import Sutori.Lexer
import Sutori.AST
import Sutori.Monad
import Sutori.Types
import Sutori.Utils

}

%name      parse
%tokentype { SutToken }
%monad     { SutParserM }
%lexer     { lexwrap } { SutTkEOF }

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
%left     '['
%left     '->'

%%


-- Program
-----------------------------------------------------------------------------------------------------------------------
Source                      : PROGRAM_INI Init
                            {% beforeStart $1 }

Init                        : ID Block PROGRAM_FIN EOF
                            { SutModule (getToken $1) $2 }

-- Expressions
-- ====================================================================================================================
Expression                  : AssignableObject %prec ASG
                            {% return $1 }
                            | Literal
                            {% return (SutExprLiteral (getExpressionType $1) $1) }
                            | Constructor
                            {% getType $1 >>= (\t -> SutExprConstructor t $1) }
                            | UnaryOperation     { $1 }
                            | BinaryOperation    { $1 }
                            | FunctionCall       { $1 }
                            | FreePointer        { $1 }
                            | '(' Expression ')' { $2 }


Literal                     : LITERAL_INT        { SutLitInt    ((getValueInt.getToken) $1) }
                            | LITERAL_CHAR       { SutLitChar   ((getValueChar.getToken) $1) }
                            | LITERAL_FLOAT      { SutLitFloat  ((getValueFloat.getToken) $1) }
                            | LITERAL_BOOL       { SutLitBool   ((getValueBool.getToken) $1) }
                            | LITERAL_STRING     { SutLitString ((getString.getToken) $1) }


-- Constructors
-----------------------------------------------------------------------------------------------------------------------
Constructor                 : ConstructorArray                            { SutArray $1 }
                            | ConstructorStruct                           { SutStruct $1 }

ConstructorArray            : '[' ConstructorArrayList ']'                { $2 }
ConstructorArrayList        : Expression                                  { [ $1 ] }
                            | Expression ';' ConstructorArrayList         { $1:$3 }

ConstructorStruct           : '{' ConstructorStructList '}'               { $2 }
ConstructorStructList       : ID ':' Expression                           { [ ($1,$3) ] }
                            | ID ':' Expression ';' ConstructorStructList { ($1,$3): $5 }


-- Operators
-----------------------------------------------------------------------------------------------------------------------
Operation                   : UnaryOperation                { $1 }
                            | BinaryOperation               { $1 }

UnaryOperation              : NumericalUnaryOperation       { $1 }
                            | LogicalUnaryOperation         { $1 }

BinaryOperation             : NumericalBinaryOperation      { $1 }
                            | LogicalBinaryOperation        { $1 }
                            | Assignment                    { $1 }

NumericalUnaryOperation     : '+' Expression %prec POS      { SutUnaryOp (getNumExprType $2) SutOpPos $2 }
                            | '-' Expression %prec NEG      { SutUnaryOp (getNumExprType $2) SutOpNeg $2 }

LogicalUnaryOperation       : '!' Expression                { SutUnaryOp (getBoolExprType $2) SutOpNot $2 }

NumericalBinaryOperation    : Expression '+' Expression     { SutBinaryOp (binaryNum2NumType $1 $3)   SutOpAdd $1 $3 }
                            | Expression '-' Expression     { SutBinaryOp (binaryNum2NumType $1 $3)   SutOpSub $1 $3 }
                            | Expression '*' Expression     { SutBinaryOp (binaryNum2NumType $1 $3)   SutOpMul $1 $3 }
                            | Expression '%' Expression     { SutBinaryOp (binaryNum2NumType $1 $3)   SutOpMod $1 $3 }
                            | Expression '/' Expression     { SutBinaryOp (binaryNum2NumType $1 $3)   SutOpIntDiv $1 $3 }
                            | Expression div Expression     { SutBinaryOp (binaryNum2FloatType $1 $3) SutOpDiv $1 $3 }
                            | Expression '^' Expression     { SutBinaryOp (binaryNum2FloatType $1 $3) SutOpPow $1 $3 }

LogicalBinaryOperation      : Expression and  Expression    { SutBinaryOp (binaryBoolType $1 $3)     SutOpAnd      $1 $3 }
                            | Expression or   Expression    { SutBinaryOp (binaryBoolType $1 $3)     SutOpOr       $1 $3 }
                            | Expression '==' Expression    { SutBinaryOp (binaryEqualityType $1 $3) SutOpEqual    $1 $3 }
                            | Expression '/=' Expression    { SutBinaryOp (binaryEqualityType $1 $3) SutOpNotEqual $1 $3 }
                            | Expression '>=' Expression    { SutBinaryOp (binaryNum2BoolType $1 $3) SutOpGEqual   $1 $3 }
                            | Expression '<=' Expression    { SutBinaryOp (binaryNum2BoolType $1 $3) SutOpLEqual   $1 $3 }
                            | Expression '>'  Expression    { SutBinaryOp (binaryNum2BoolType $1 $3) SutOpGreater  $1 $3 }
                            | Expression '<'  Expression    { SutBinaryOp (binaryNum2BoolType $1 $3) SutOpLess     $1 $3 }

Dereference                 : '*' Expression %prec IND
                            { % getExpressionType $2 >>= \t -> return SutPointed t $2 }

GetArrayItem                : AssignableObject '[' Expression ']'
                            { % checkIndexType $3 >> checkArrayType $1 >>= (\t -> return (SutArrayItem t $1 $3)) }

GetProp                     : AssignableObject '->' ID      { SutStructMember t $1 $3 }

Assignment                  : AssignableObject '=' Expression
                            { % getEqualityType SutOpAssign $1 $3 >>= (\t -> return (SutBinaryOp (getExpressionType $1) SutOpAssign $1 $3)) }

AssignableObject            : ID              { % checkId' $1 VarSym }
                            | GetArrayItem    { $1 }
                            | GetProp         { $1 }
                            | Dereference     { $1 }

FunctionCall                : ID
                            { % checkId' $1 FunctionSym >>= (\s -> return (SutCall (fromJust (getType' s)) (getToken $1) [])) }
                            | ID '(' WITH FunctionActualParams ')'
                            { % checkId' $1 FunctionSym >>= (\s -> checkParams $4 s  >> return (SutCall (fromJust (getType' s)) (getToken $1) (getToken $4))) }

FunctionActualParams        : Expression                           { [$1] }
                            | Expression ',' FunctionActualParams  { $1:$3 }




-- Declaration
-- =====================================================================================================================
Declaration                 : PersonDeclaration                     { $1 }
                            | FunctionDeclaration                   { $1 }
                            | VariableDeclaration                   { $1 }
                            | TypeDeclaration                       { $1 }


PersonDeclaration           : S_therewas PersonNames
                            { insertPerson $2 }

PersonNames                 : ID                                    { [$1] }
                            | ID ',' PersonNames                    { $1:$3 }
                            | ID and PersonNames                    { $1:$3 }

FunctionDeclaration         : FUNCTION_INI IdentificadorFun ',' S_therewasa Type FunctionBlock FUNCTION_FIN
                            { % checkType $5 >> modifyFunction $2 $5 [] $6 }

                            | FUNCTION_INI IdentificadorFun ',' S_therewasa Type '(' S_madeof StackParams ')' FunctionBlock FUNCTION_FIN
                            { % checkType $5 >> modifyFunction $2 $5 $8 $10 }

IdentificadorFun            : ID                                    { % checkId' $1 FunctionSym >> insertFunction $1 }

StackParams                 : FunctionFormalParams                  { % insertFunctionParams $1 }

FunctionFormalParams        : Type ID                               { [(False, $1, $2)] }
                            | Type ID ',' FunctionFormalParams      { (False, $1,$2):$4 }
                            | YOUR Type ID                          { [(True, $2, $3)] }
                            | YOUR Type ID ',' FunctionFormalParams { (True, $2, $3):$5 }


VariableDeclaration         : ID S_broughta Type ':' VariableList
                            { % variableDeclaration $1 $3 $5  }

VariableList                : ID ',' VariableList                   { ($1,Nothing):$3 }
                            | ID '=' Expression ',' VariableList    { ($1,Just $3):$5 }
                            | ID '=' Expression                     { [($1,Just $3)] }
                            | ID                                    { [($1,Nothing)] }

TypeDeclaration             : ID S_invented ID ';' S_itsa Type
                            { % checkId' $1 PersonSym >> checkToBeNewNow "Person" [$1]  }



-- Types
-- ====================================================================================================================
Type                        : TYPE_INT                                    { SutTypeInt }
                            | TYPE_FLOAT                                  { SutTypeFloat }
                            | TYPE_CHAR                                   { SutTypeChar }
                            | TYPE_BOOL                                   { SutTypeBool }
                            | TYPE_STRING                                 { SutTypeString }
                            | TYPE_ARRAY '(' OF LITERAL_INT Type ')'      { SutTypeArray $4 $5 }
                            | TYPE_STRUCT '(' WITH StructTyping ')'       { SutTypeStruct $4 }
                            | TYPE_UNION '(' EITHER UnionTyping ')'       { SutTypeUnion $4 }
                            | TYPE_POINTER '(' TO Type ')'                { SutTypePointer $4 }
                            | ID                                          { % checkId' $1 TypeSym >>= getType }

StructTyping                : Type ID                                     { [($1,$2)] }
                            | Type ID and StructTyping                    { ($1,$2):$4 }

UnionTyping                 : Type ID                                     { [($1,$2)] }
                            | Type ID or UnionTyping                      { ($1,$2):$4 }


-- Blocks
-- ====================================================================================================================
AddScope                    : BLOCK_OPEN                                  { % addInstructionScope }
RemoveScope                 : BLOCK_CLOSE                                 { % removeLastScope }

Block                       : AddScope BlockContent RemoveScope           { $3 }

BlockContent                : Statement BlockContent                      { $1: $2 }
                            | {-empty-}                                   { [] }

FunctionBlock               : AddScope FunctionBlockContent RemoveScope   { $3 }

FunctionBlockContent        : Statement FunctionBlockContent              { $1:$2 }
                            | ReturnStatement FunctionBlockContent        { $1:$2 }
                            | {-empty-}                                   { [] }

Statement                   : Instruction                                 { $1 }
                            | Declaration                                 { $1 }

ReturnStatement             : S_andthatswhere Expression S_comesfrom      { SutReturn $2 }


-- Instructions
-----------------------------------------------------------------------------------------------------------------------
Instruction                 : Expression '.'         { SutInstExpression $1 }
                            | FreePointer '.'        { $1 }
                            | Print '.'              { $1 }
                            | Selection              { $1 }
                            | UnboundedIteration     { $1 }
                            | BoundedIteration       { $1 }

CreatePointer               : '(' PersonID S_madea Type ')'
                            { % checkId' $2 >> return (SutCreatePointer $3 $1) }
FreePointer                 : '(' PersonID S_brokea AssignableObject ')'
                            { % checkId' $2 PersonSym >> return (SutFreePointer $1 $3) }

Selection                   : PersonID S_dreamsof Block WHEN Expression
                            { % checkConditionalType $5 >> return (SutSelection $1 $3 $5 []) }
                            | PersonID S_dreamsof Block WHEN Expression ';' OTHERWISE Block
                            { % checkConditionalType $5 >> return (SutSelection $1 $3 $5 $8) }

PersonID                    : ID
                            { % checkId' $1 PersonSym >> return $1 }

UnboundedIteration          : PersonID S_keepsdreamingof Expression Block
                            { % checkConditionalType $3 >> return (SutIterationU $1 $3 $4) }

BoundedIteration            : Block PersonID S_toldthatstory Expression TIMES
                            { % checkIndexType $4 >> return (SutIterationB $2 $4 $1) }


Print                       : PersonID ':' Expression
                            { SutPrintVal $1 $3 }
Read                        : AssignableObject '?'
                            { SutReadVal $1 }



{


happyError :: SutToken -> SutParserM a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
  where lcn = case tks of
                []    -> "end of file"
                ((SutToken pos _):_)  -> showSut pos

-- vim:ft=haskell
}
