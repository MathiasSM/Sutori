# Sutori language definition

## Lexemes (tokens)


### Comments

```
ONELINE         = `--`
MULTILINE       = `/*` and `*/`
```
### Separators

```
OPEN            = `...(`
CLOSE           = `)...`
```

### Blah blah

```
INI             = `Once upon a time in`
FIN             = `and they lived happily ever after`

F_INI           = `Once upon some other time in`
F_FIN           = `or that is what they say`

THEREWAS        = `There was`
BROUGHTA        = `brought a`
DREAMSOF        = `dreams of`
KEEPSDREAMINGOF = `keeps dreaming of`
TOLDTHESTORY    = `told the story`
MADEA           = `made a`
```

### Type names

```
INT_TYPE        = `bag`
FLOAT_TYPE      = `wallet`
CHAR_TYPE       = `book`
BOOL_TYPE       = `lightbulb`

ARRAY_TYPE      = `chain`
STRUCT_TYPE     = `machine`
UNION_TYPE      = `thing`

POINTER_TYPE    = `direction`
```

### Identifiers

```
FNAME           = `/[A-Za-z][A-Za-z0-9_]*/`
PNAME           = `/[A-Za-z-]+/`
VNAME           = `/[A-Za-z_]+/`
PROPNAME        = `/[A-Za-z_]+/`
```


## Grammar definition

```
Source                : INI ProgramName OPEN Block CLOSE FIN

Block                 : Statement . Block
                        \l

FunctionBlock         : Statement . FunctionBlock
                        ReturnStatement . FunctionBlock
                        \l

ReturnStatement       : `return` Expression

Statement             : FunctionDeclaration
                        PersonDeclaration
                        VariableDeclaration
                        Instruction
                        \l

FunctionDeclaration   : ( F_INI FNAME, `a` Type OPEN Block CLOSE F_FIN )
                        ( F_INI FNAME, `a` Type (`from` FunctionArgs) OPEN Block CLOSE F_FIN )

FunctionFormalParams  : Type VNAME
                        Type * VNAME, FunctionFormalParams

PersonDeclaration     : THEREWAS PersonNames.

PersonNames           : PNAME
                        PNAME, PersonNames
                        PNAME and PersonNames

VariableDeclaration   : PNAME BROUGHTA Type: VNAME
                        PNAME BROUGHTA Type: VNAME = Expression

Type                  : INT_TYPE
                        FLOAT_TYPE
                        CHAR_TYPE
                        BOOL_TYPE
                        ARRAY_TYPE (`of` Type)
                        STRUCT_TYPE (`with` StructTyping)
                        UNION_TYPE (`either` UnionTyping)
                        POINTER_TYPE (`to` Type)

StructTyping          : PropTyping
                        PropTyping `and` StructTyping

PropTyping            : Type PROPNAME

UnionTyping           : Type
                        Type `or` UnionTyping

Expression            : LITERAL
                        VNAME
                        ( Expression )
                        Malloc
                        Assignment
                        FunctionCall
                        BinaryOperation
                        UnaryOperation
                        DeStructProp

Malloc                : PNAME MADEA Type 

Assignment            : VNAME = Expression

DeStructProp          : Expression .PROPNAME

UnaryOperation        : - Expression
                        + Expression
                        ~ Expression
                        $ Expression
                        

BinaryOperation       : Expression + Expression
                        Expression - Expression
                        Expression * Expression
                        Expression / Expression
                        Expression % Expression
                        Expression ^ Expression

                        Expression `and` Expression
                        Expression `or` Expression

                        Expression == Expression
                        Expression /= Expression

                        Expression >= Expression
                        Expression <= Expression
                        Expression < Expression
                        Expression > Expression

FunctionCall          : FNAME()
                        FNAME(`with` FunctionActualParams)

FunctionActualParams  : VNAME
                        Expression
                        Expression, FunctionActualParam

Instruction           : Expression
                        Selection
                        UnboundedIteration
                        BoundedIteration

Selection             : PNAME DREAMSOF OPEN Block CLOSE `when` Expression
                      : PNAME DREAMSOF OPEN Block CLOSE `when` Expression ... `otherwise` OPEN Block CLOSE

UnboundedIteration    : PNAME KEEPSDREAMINGOF Expression ? OPEN Block CLOSE

BoundedIteration      : OPEN Block CLOSE PNAME TOLDTHESTORY Expression times!
```
