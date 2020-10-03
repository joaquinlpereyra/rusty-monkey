# Grammar

This is a grammar for an extremely simple subset of Rusty Monkey.
This subset only takes statments as valid top-level syntax.



```
S      -> let LIT = EXPR | for LIT in INTEGER { S } | if (EXPR) { S } | if (EXPR) { S } else { S }
EXPR   -> [0-9a-z] INFIX EXPR | true | false
INFIX  -> + | - | * | /
```

## Firsts

```
FIRST(S) =     {let, for, if}
FIRST(EXPR) =  {true, false, [0-9a-z]
FIRST(INFIX) = {*, -, /, +}
```

## Follows
```
FOLLOW(S) = { $ }
FOLLOW(EXPR) = { ) }
FOLLOW(INFIX) = { true, false, [0-9a-z] }
```

## Prediction
```
PRED(S) ->  { $ }
PRED(EXPR) = {true, false, [0-9a-z]}
PRED(INFIX) = { +, -, *, / }
```

Intersecting the sets gives the empty set, so this language can be parsed by an LL(1) parser.
