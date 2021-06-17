# rusty-monkey
A Monkey-lang implementation on Rust!

The language is very simple, and it is is interpreted as you write. There's a REPL provided.

We support simple arithmetics and assignments:

- \+ operator
- \- operator
- \/ operator
- \* operator

Strings and arrays are not yet supported. 

Integers are always represented as int64.

Here is a simple example, where you can also see how to assign variables, how to use conditionals and for loops.
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



The complete grammar looks like this:

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

# Install

```
# First install rust. This only works for Unix systems
# See https://forge.rust-lang.org/infra/other-installation-methods.html
# for Windows
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Clone this repository
$ git clone https://github.com/joaquinlpereyra/rusty-monkey

# Get into the new directory
$ cd rusty-monkey

# You can run the tests with
$ cargo test

# You can run the REPL with
$ cargo run
🙈 🙊 🙉 let x = 4;
tokens: [Let, Ident("x"), Assign, Int("4"), Semicolon]
parsed: LET x = 4
interpreted: null
🙈 🙊 🙉 x
tokens: [Ident("x")]
parsed: (x)
interpreted: 4
🙈 🙊 🙉
```

