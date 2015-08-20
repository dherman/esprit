# An Iterative ES6 Grammar

The following is an EBNF refactoring of the ES6 grammar that is more conducive to a recursive descent parser (with precedence parsing) that uses as much iteration and as little recursion as possible.

## Expressions

```
PrimaryExpression ::=
  "this"
  IdentifierReference
  Literal
  ArrayLiteral
  ObjectLiteral
  FunctionExpression
  ClassExpression
  GeneratorExpression
  RegularExpressionLiteral
  "(" Expression ")"

MemberBaseExpression ::=
  PrimaryExpression
  "new" "." "target"

NewExpression ::=
  "new"+n (MemberBaseExpression | "super" Deref) Deref* Arguments<n Suffix*

CallExpression ::=
  (MemberBaseExpression | "super" Suffix) Suffix*

LHSExpression ::=
  NewExpression
  CallExpression

UnaryExpression ::=
  Prefix* LHSExpression PostfixOperator?

Prefix ::=
  "delete"
  "void"
  "typeof"
  "++"
  "--"
  "+"
  "-"
  "~"
  "!"

Infix ::=
  "*"
  "/"
  "%"
  "+"
  "-"
  "<<"
  ">>"
  ">>>"
  "<"
  ">"
  "<="
  ">="
  "instanceof"
  "in"
  "=="
  "!="
  "==="
  "!=="
  "&"
  "^"
  "|"
  "&&"
  "||"

Suffix ::=
  Deref
  Arguments

PostfixOperator ::=
  [no line terminator] "++"
  [no line terminator] "--"

Deref ::=
  "[" Expression "]"
  "." IdentifierName

AssignmentExpression ::=
  YieldPrefix* "yield"
  YieldPrefix* ConditionalExpression (("=" | AssignmentOperator) AssignmentExpression)?

ConditionalExpression ::=
  UnaryExpression (Infix UnaryExpression)* ("?" AssignmentExpression ":" AssignmentExpression)?

Expression ::=
  AssignmentExpression ("," AssignmentExpression)*
```

## Operator Precedence

The following are the precedence levels for the Infix operators. Higher precedence values bind more tightly than lower precedence values.

Operator     | Precednce
--------     | ---------
"*"          | 11
"/"          | 11
"%"          | 11
"+"          | 10
"-"          | 10
"<<"         | 9
">>"         | 9
">>>"        | 9
"<"          | 8
">"          | 8
"<="         | 8
">="         | 8
"instanceof" | 8
"in"         | 8
"=="         | 7
"!="         | 7
"==="        | 7
"!=="        | 7
"&"          | 6
"^"          | 5
"|"          | 4
"&&"         | 3
"||"         | 2


# Avoiding Extra Lookahead

Conceptually, the ECMAScript grammar would require two tokens of lookahead for a recursive descent parser -- in particular, statements beginning with an Identifier followed by a colon are LabelledStatements, whereas statements beginning with an Identifier *not* followed by a colon are ExpressionStatements.

However, a single token of lookahead is much simpler to get right (because of ECMAScript's context-sensitive lexing and just generally the complexity of managing a lookahead buffer), and likely more efficient.

Luckily, the only cases where an extra token of lookahead is necessary are cases where the first lookahead token is an Identifier. This leads to a specialized version of the Expression grammar where the first token is an Identifier that cannot be unlexed in order to peek at the subsequent token. Again luckily, this only requires a very small amount of duplication of logic in the grammar, with four specialized versions of non-terminals in the Expression grammar:

```
IDUnaryExpression ::=
  IdentifierReference Suffix* PostfixOperator?

IDConditionalExpression ::=
  IDUnaryExpression (Infix UnaryExpression)* ("?" AssignmentExpression ":" AssignmentExpression)?

IDAssignmentExpression ::=
  YieldPrefix* "yield"
  YieldPrefix+ ConditionalExpression (("=" | AssignmentOperator) AssignmentExpression)?
  IDConditionalExpression (("=" | AssignmentOperator) AssignmentExpression)?

IDExpression ::=
  IDAssignmentExpression ("," AssignmentExpression)*
```
