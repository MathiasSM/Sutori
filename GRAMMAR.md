# Sutori language definition

## Syntax (tokens)

### Separators

```
OPEN         = `...(`
CLOSE        = `)...`
```

### Blah blah

```
INI          = `Once upon a time in`
FIN          = `and they lived happily ever after`

F_INI        = `Once upon some other time in`
F_FIN        = `or that is what they say`

THEREWAS     = `There was `
BROUGHTA     = ` brought a `
```

### Type names

```
INT_TYPE     = `bag`
FLOAT_TYPE   = `wallet`
CHAR_TYPE    = `book`
BOOL_TYPE    = `lightbulb`

ARRAY_TYPE   = `chain`
STRUCT_TYPE  = `machine`
UNION_TYPE   = `thing`

POINTER_TYPE = `direction`
```

### Identifiers

```
FNAME        = `/[A-Za-z][A-Za-z0-9_]*/`
PNAME        = `/[A-Za-z-]+/`
VNAME        = `/[A-Za-z_]+/`
PROPNAME     = `/[A-Za-z_]+/`
```


## Grammar definition

```
Source              : INI ProgramName OPEN Block CLOSE FIN

Block               : Statement . Block
                      \l

Statement           : FunctionDeclaration
                      PersonDeclaration
                      VariableDeclaration
                      Expression
                      \l

FunctionDeclaration : ( F_INI FNAME OPEN Block CLOSE F_FIN )

PersonDeclaration   : THEREWAS PNames.

PNames              : PNAME
                      PNAME, PNames
                      PNAME and PNames

VariableDeclaration : PNAME BROUGHTA Type: VNAME ASSIGNED Expression

Type                : INT_TYPE
                      FLOAT_TYPE
                      CHAR_TYPE
                      BOOL_TYPE
                      ARRAY_TYPE (`of` Type)
                      STRUCT_TYPE (`with` StructTyping)
                      UNION_TYPE (`either` UnionTyping)
                      POINTER_TYPE (`to` Type)

StructTyping        : PropTyping
                      PropTyping `and` StructTyping

PropTyping          : Type PROPNAME

UnionTyping         : Type
                      Type `or` UnionTyping

Expression          : LITERAL
                      VNAME
                      (Expression)
                      Expression + Expression
                      Expression - Expression
                      Expression * Expression
                      Expression / Expression
                      Expression % Expression
                      Expression ^ Expression
```
