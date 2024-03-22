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
Constructors and categories must have names starting with lowercase
letters.  Each syntactic category is a type in the language and can be
used in the argument types for constructors.  Additionally, Sterling
has [built-in types](#built-in-types) that can be used as arguments.
If a constructor takes no arguments, we may optionally leave the
parentheses out, as we did with `true` and `false` above.

Each new syntactic category must also declare its projection type,
discussed [elsewhere](projection.md).


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
We use the ellipsis (`...`) at the beginning to alert Sterling to the
fact that this category already exists and we are only adding
constructors, as opposed to creating a new category.  The constructor
declarations are then as before.

Each new constructor must be given a rule defining its projection as
well, discussed as part of the [broader discussion of
projection](projection.md).


## Built-In Types
Sterling has two basic built-in types, strings and integers, written as
`string` and `int` respectively.

We also have two built-in type constructors that can contain any
types, lists and tuples:
* The list type is written with the inner type in square brackets,
  such as `[int]`, which is a list of integers.
* Tuples are written as comma-separated lists in parentheses, such as
  `(int, string)`, which is a pair of an integer and a string.  As
  another example, we could also have `(int, expr, [int])`, a triple
  of an integer, an expression, and a list of integers.
