# Defining Abstract Syntax
The abstract syntax of a language module is composed of a set of
syntactic categories and constructors building those categories.
Syntactic categories can be thought of as algebraic data types, with
the constructors of the category being the constructors of the data
type.


## Declaring New Syntactic Categories
We declare syntactic categories and a set of constructors for them
together.  For example, we might declare a syntactic category for
expressions named `expr` with a few constructors as follows:
```
expr ::= or(expr, expr)
       | and(expr, expr)
       | true
       | false
```
This gives us four constructors of the `expr` category.  Each
constructor declaration has the form
```
<name>(<comma-separated argument types>)
```
Each syntactic category is a type in the language and can be used in
the argument types for constructors.  Additionally, SOS-Ext has
[built-in types](#built-in-types) that can be used as arguments.  If a
constructor takes no arguments, we may optionally leave the
parentheses out, as we did with `true` and `false` above.

### Declaring Translation Types
Because of the view of extensibility we take, having a base language
and independent extensions to this base that automatically compose,
each syntactic category must have a translation relation.  This is
declared as
```
Translation <category> : <space-separated argument types>
```
The arguments here can be used to define translations for extension
syntax, as we will see below.  For example, we might define
translation for an expression to depend on a typing context:
```
Translation expr : typingCtx
```
Quite often, we find we don't want expressions to depend on anything,
so we have no arguments:
```
Translation expr :
```


## Adding to Syntactic Categories
Because our language is extensible, we can add new constructors for
syntactic categories given by one module in the modules that build on
it.  For example, another module could add to our expressions from
above:
```
expr ::= ...
       | plus(expr, expr)
       | num(int)
       | greater(expr, expr)
```
We use the ellipsis (`...`) at the beginning to alert SOS-Ext to the
fact that this category already exists and we are only adding
constructors, as opposed to creating a new category.  The constructor
declarations are then as before.

### Writing Translation Rules
In order for the language to be well-formed, each new constructor
given in a module for a syntactic category imported from a built-on
module must be given a translation rule.  The translation relation has
the form
```
<arguments> |{<category>}- <translating> ~~> <translation>
```
where the arguments have the types given by the `Translation`
declaration explained above.


## Built-In Types
SOS-Ext has two basic built-in types, strings and integers, written as
`string` and `int` respectively.

We also have two built-in type constructors that can contain any
types, lists and tuples:
* The list type is written with the inner type in square brackets,
  such as `[int]`, which is a list of integers.
* Tuples are written as comma-separated lists in bananas, such as
  `(|int, string|)`, which is a pair of an integer and a string.  As
  another example, we could also have `(|int, expr, [int]|)`, a triple
  of an integer, an expression, and a list of integers.
