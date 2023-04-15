# Main Function Language
The portion of SOS-Ext for defining functions tells how the language
should run.  It includes processing files by reading them, parsing
them, and deriving relations over the abstract syntax produced by
parsing.

The language's behavior is defined by writing functions.  Functions
are declared as
```
Function <name> : <params> -> <return type>{
   <body>
}
```
Function names must be lowercase.  The parameters are declared as
```
<name1 : type1> <name2 : type2> ...
```
Each parameter name must be capitalized.  The types that may be given
to arguments and as return types are the types in the [semantic
portion of the language](../semantic/syntax.md), in addition to new
`bool` (Boolean) and `unit` types.

When an SOS-Ext module is compiled and run, the starting point is the
special `main` function in the module with the type `[string] -> int`.
The list of strings is the list of command line arguments given when
the compiled program is run.


## Function Bodies
Each function body is a single expression producing a value, which is
then the result of the function.  We have several expression forms,
which we will separate into various classes for discussion.

### Boolean Expressions
We have basic Boolean expressions:
* `true`
* `false`
* `<expr> || <expr>`:  Disjunction
* `<expr> && <expr>`:  Conjunction

We also have conditionals `If <cond> Then <then> Else <else>`, where
the `<cond>` expression must have type `bool` and the `<then>` and
`<else>` expressions must have the same type.

### Arithmetic Expressions
We have basic integer expressions:
* `<expr> + <expr>`
* `<expr> - <expr>`
* `<expr> * <expr>`
* `<expr> / <expr>`
* `<expr> % <expr>`
* `<num>`:  Integer constants

### Comparison Expressions
We have basic comparisons on integers:
* `<expr> < <expr>`
* `<expr> > <expr>`
* `<expr> <= <expr>`
* `<expr> >= <expr>`

Additionally, we have an equality comparisons:
* `<expr> = <expr>`

### Other Simple Expressions
We have some other simple expressions:
* String literals written using double quotes
* Function calls, written `<fun name>(<comma-separated args>)`
* Zero-based list indexing, written `<list>[<index>]`
* Append, written `<expr> ++ <expr>`, concatenating two lists or two
  strings
* Let bindings, written `Let <var> := <bound> In <body>`, bind the
  result of `<bound>` to the variable `<var>` for use in `<body>`.
  This can bind multiple variables if the bound expression is a tuple,
  such as `Let A, B, C := (1, 2, 3) In A + B + C`, which will give a
  result of 6.  All bound variables must be capitalized.
* Variables, which may refer to arguments to the function or variables
  bound by a let binding
* Sequencing, written `<expr> Before <expr>`, which evaluates the
  first expression followed by the second one.  The first expression
  must have type `unit`.

### Input/Output Expressions
We have several expressions for carrying out I/O actions:
* `Print <expr>` prints the string form of the value resulting from
  the expression to the terminal
* `Write <expr> to <file>` writes the string expression to a file with
  the name `<file>`
* `Read <file>` reads and results in the string contents of the file
  with the name `<file>`

All three of these expressions have type `unit`.

### Parsing
We can parse concrete nonterminals, defined in the [concrete part of
the language](../concrete/README.md).  This is written as
```
Parse <nonterminal> from <string>
```
where `<nonterminal>` is a concrete nonterminal name and `<string>` is
an expression of type `string`.  A parsing expression produces a
three-tuple of type `(bool, <abstract syntax>, string)`.  The
members of the tuple are as follows:
* `bool`:  Whether the parse was successful (`true`) or not.
* `<abstract syntax>`:  The abstract syntax produced from the parsed
  concrete syntax.  This is only defined if the parse was successful
  (the first element is `true`).
* `string`:  Errors describing why the string didn't parse.  This is
  only defined if the parse was *not* successful.

A common structure for parsing is
```
Let Success, A, Err := Parse nt from String
In
  If Success
  Then <use A>
  Else <use Err>
```

### Deriving Judgments
We can derive judgments defined in the [semantic part of the
language](../semantic/README.md).  This is written as
```
Derive {<judgment>} for <input vars> assigning [<output vars>]
```
For example, we might write
```
Derive {typeOf [] Tm Ty} for Tm assigning [Ty]
```
to type a term `Tm`, finding it has type `Ty`.  The parts of the
derivation expression are:
* `<judgment>`:  The judgment we want to derive, applied to the
  arguments we want for it
* `<input vars>`:  The variables already defined in the expression's
  context for which we want to derive the relation.  In the example,
  we have `Tm` already defined, possibly parsed from a file, and we
  want to find its type.  If we have multiple input variables, we
  write them as a comma-separated list.
* `<output vars>`:  The variables we want to produce from the
  derivation.  In the example, we want to produce the binding found
  for `Ty` by deriving the typing relation.  If we have multiple
  output variables, we write them as a comma-separated list.

This produces an `n + 1`-tuple, where `n` is the number of output
variables.  The first element of the tuple is a `bool` representing
whether the derivation was successful or not.  The remaining elements
in the tuple are the terms found for the output variables, which are
only defined if the derivation was successful (the first element of
the tuple is `true`).  In the example above, the derivation expression
has type `(bool, ty)` where `ty` is a user-defined syntactic
category.
