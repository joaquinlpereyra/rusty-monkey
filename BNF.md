# Grammar

This is a grammar for an extremely simple subset of Rusty Monkey.
This subset only takes statments as valid top-level syntax.

## Informal description
The grammar should be able to express a simple Monkey program.

```
let x = 4;
let y = 10;
for i in 10 {
   let y = y + 1;
};
if (y == 20) {
   let z = 1;
} else {
   let z = 0;
};
z;
```

# BNF

```
S       ::= BLOCK
BLOCK   ::= <STMT>; | <STMT>; <S>
LITERAL ::= [a-zA-Z]
INT     ::= [0-9] | [0-9]<INT>
STMT    ::= <LET> | <EXPR> | <FOR>
LET     ::= let <IDENT> = <EXPR>
EXPR    ::= <LITERAL> | <INT> | <IFELSE> | <BINARY>
IFELSE  ::= if (<EXPR>) { <BLOCK> } | if (<EXPR>) { <BLOCK> } else S
BINARY  ::= <EXPR> + <EXPR> | <EXPR> - <EXPR> | <EXPR> * <EXPR> | <EXPR> / <EXPR>
FOR     ::= for <LITERAL> in <INT> { <BLOCK> }
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
