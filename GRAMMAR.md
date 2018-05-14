# Sutori language definition

## (Common) Lexemes (tokens)


### Comments

Some kind a narration, or dialog, that doesn't take place within the story.

```
ONELINE           : `-- .*$`
MULTILINE         : `--|` .* `|--`
```

### Block separators

We're too cool to go with `{` `}`.

```
BLOCK_OPEN        : ... (
BLOCK_CLOSE       : ) ...
```

### _あなたはたくさん話します_

Anata talk too much.

```
PROGRAM_INI       : `Once upon a time in`
PROGRAM_FIN       : `and they lived happily ever after`
FUNCTION_INI      : `Once upon some other time`
FUNCTION_FIN      : `or that is what they say`

S_Therewas        : `There was`
S_therewasa       : `there was a`
S_madeof          : `made of`
S_broughta        : `brought a`
S_dreamsof        : `dreams of`
S_keepsdreamingof : `keeps dreaming of`
S_toldthatstory   : `told that story`
S_madea           : `made a`
```

### Type names

Nani?!

```
TYPE_INT          : `bag`
TYPE_FLOAT        : `wallet`
TYPE_CHAR         : `letter`
TYPE_BOOL         : `light`

TYPE_ARRAY        : `chain`
TYPE_STRUCT       : `machine`
TYPE_UNION        : `thing`
TYPE_STRING       : `phrase`

TYPE_POINTER      : `direction`
```

### Literals

We know commas are wrong.

```
LITERAL_INT           : `/\d+/`
LITERAL_FLOAT         : `/\d+,\d+/`
LITERAL_CHAR          : `/'.'/`
LITERAL_STRING        : `/"([^"]|\\.)*"/`
LITERAL_BOOL          : `on`
                        `off`
```

### Identifiers

Regular expressions. Names can be hyphened (Like Jean-Jacques)

```
ID_MODULE         : `/[a-zA-Z][A-Za-z0-9_]*/`
ID_FUNCTION       : `/[a-zA-Z][A-Za-z1-9_]*/`
ID_PERSON         : `/[a-zA-Z-]+/`
ID_VARIABLE       : `/[a-zA-Z_]+/`
ID_PROP           : `/[a-zA-Z_]+/`
```



## Grammar definition

### Expressions

#### Expression

Either a variable, a literal, a constructed object of a special type, a function call, an operation, or association. The usual.


```
Expression            : ID_VARIABLE
                        Literal
                        Constructor
                        FunctionCall
                        Operation 
                        ( Expression )

Literal               : LITERAL_INT
                        LITERAL_FLOAT
                        LITERAL_CHAR
                        LITERAL_BOOL
```

#### Constructors

Some helping syntax to set values quickly.

```
Constructor              : ConstructorArray
                           ConstructorStruct
                           
ConstructorArray         : [ ConstructorArray_list ]
ConstructorArray_list    : Expression
                           Expression , ConstructorArray_list
                           
ConstructorStruct        : { ConstructorStruct_list }
ConstructorStruct_list   : ID_PROP : Expression
                           ID_PROP : Expression , ConstructorStruct_list
```

#### Operators

Symbols. There are way more special characters, but we need more time and imagination to give them meanings.


##### The operators and their precedence

Based on C's precedence rules.

| Precedence | Associativy | Operator           | Description                                                |
| ---------- | ----------- | ------------------ | ---------------------------------------------------------- |
| 1          | left        | `->`               | Member access                                              |
| 2          | right       | `!` `*` `+` `-`  | Unary operators: negation, indirection, positive, negative |
| 3          | left        | `^`                | Power                                                      |
| 4          | left        | `*` `/` `div` `%`  | Multiplication, both divisions and remainder               |
| 5          | left        | `+` `-`            | Addition and substraction                                  |
| 6          | neither     | `<` `<=` `>=` `>`  | Order compators                                            |
| 7          | left        | `==` `/=`          | Equality, non-equality                                     |
| 8          | left        | `and`              | Logical 'and'                                              |
| 9          | left        | `or`               | Logical 'or'                                               |
| 10         | right       | `=`                | Assigment                                                  |

##### Operation grammar rules

```
Operation                 : UnaryOperation
                            BinaryOperation

UnaryOperation            : Numerical_UnaryOperation 
                            Logical_UnaryOperation
                            Dereference

BinaryOperation           : NumericalBinaryOperation
                            LogicalBinaryOperation
                            GetProp
                            GetArrayItem
                            Assignment

Numerical_UnaryOperation  : + Expression
                            - Expression
Numerical_BinaryOperation : Expression + Expression
                            Expression - Expression
                            Expression * Expression
                            Expression / Expression
                            Expression `div` Expression
                            Expression % Expression
                            Expression ^ Expression

Logical_UnaryOperation    : ! Expression   
Logical_BinaryOperation   : Expression `and` Expression
                            Expression `or` Expression

                            Expression == Expression
                            Expression /= Expression

                            Expression >= Expression
                            Expression <= Expression
                            Expression < Expression
                            Expression > Expression

Dereference               : * Expression

GetProp                   : Expression -> ID_PROP

GetArrayItem              : Expression [ Expression ]

Assignment                : ID_VARIABLE = Expression
```

#### Function call

```        
FunctionCall          : ID_FUNCTION
                        ID_FUNCTION ( `with` FunctionActualParams )

FunctionActualParams  : Expression
                        Expression , FunctionActualParam
```

### Declarations (of independence?)

#### Declaration

```
Declaration           : PersonDeclaration
                        FunctionDeclaration
                        VariableDeclaration
```

#### Persons

Not a type you can compute with, but one the language uses for comedic reasons.

```
PersonDeclaration     : S_Therewas PersonNames

PersonNames           : ID_PERSON
                        ID_PERSON , PersonNames
                        ID_PERSON `and` PersonNames
```

#### Functions, variables

We want to get rid of some parenthesis. You gotta deal with them for the time being, though. 

Yes, you can declare several variables at once (with the same person).

```
FunctionDeclaration   : ( FUNCTION_INI ID_FUNCTION, S_THEREWASA Type Block FUNCTION_FIN )
                        ( FUNCTION_INI ID_FUNCTION, S_THEREWASA Type ( S_MADEOF FunctionFormalParams ) Block FUNCTION_FIN )

FunctionFormalParams  : Type ID_VARIABLE
                        Type ID_VARIABLE , FunctionFormalParams
                        `your` Type ID_VARIABLE
                        `your` Type ID_VARIABLE , FunctionFormalParams

VariableDeclaration   : ID_PERSON S_broughta Type : ListOfVariables
                        
ListOfVariables       : ID_VARIABLE , ListOfVariables
                        ID_VARIABLE = Expression , ListOfVariables
                        ID_VARIABLE = Expression
                        ID_VARIABLE
```

#### Types

Maybe we could get rid of those parenthesis. But we're evil.

```
Type                  : TYPE_INT
                        TYPE_FLOAT
                        TYPE_CHAR
                        TYPE_BOOL
                        TYPE_ARRAY ( `of` LITERAL_INT Type )
                        TYPE_STRUCT ( `with` StructTyping )
                        TYPE_UNION ( `either` UnionTyping )
                        TYPE_POINTER ( `to` Type )

StructTyping          : Type ID_PROP
                        Type ID_PROP `and` StructTyping

UnionTyping           : Type ID_PROP
                        Type ID_PROP `or` UnionTyping
```

### Blocks

We start at `Source`. There's no "main" function. `EOF` is literally "End Of File".

```
Source                : PROGRAM_INI ID_MODULE Block PROGRAM_FIN EOF

Block                 : BLOCK_OPEN BlockContent BLOCK_CLOSE

BlockContent          : Statement . BlockContent
                        \l

FunctionBlock         : BLOCK_OPEN FunctionBlockContent BLOCK_CLOSE

FunctionBlockContent  : Statement . FunctionBlockContent
                        ReturnStatement . FunctionBlockContent
                        \l
                        
Statement             : Instruction
                        Declaration
                        \l

ReturnStatement       : `And that's where` Expression `comes from`
```

### Instructions

Remember that assignment is an expression, and a variable declaration may assign an initial value.

```                  
Instruction           : Expression
                        Selection
                        UnboundedIteration
                        BoundedIteration
                        ManageMemory
 
ManageMemory          : CreatePointer
                        Free
CreatePointer         : ID_PERSON S_madea Type 
FreePointer           : ID_PERSON S_broke ID_VARIABLE
                        
Selection             : ID_PERSON S_dreamsof Block `when` Expression
                      : ID_PERSON S_dreamsof Block `when` Expression ; `otherwise` Block

UnboundedIteration    : ID_PERSON S_keepsdreamingof Expression Block

BoundedIteration      : Block ID_PERSON S_toldthatstory Expression `times`

Print                 : ID_PERSON : Expression
```

