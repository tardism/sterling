# Defining Language Semantics
The semantics of languages are described by declaring judgments and
writing rules for when these judgments hold.  We have two types of
judgments.  The first is fixed judgments, which cannot be extended by
modules building on the module introducing them.  The other is
extensible judgments, where other modules can add new rules.  We
examine the introduction and definition of each in turn after looking
at the terms we can use in writing judgments and the built-in
judgments SOS-Ext provides.


## Terms
One form of term is, as expected, the application of a constructor
defined as part of the [abstract syntax](syntax.md).  The syntax for
writing a constructor application is similar to that used for
declaring constructors, with term arguments in parentheses
(e.g. `or(A, B)`).  If a constructor takes no arguments, it can be
written without parentheses (e.g. `true` or `true()`).

Another term is a variable, written starting with a capital letter
(e.g. `A`, `Var` `B32` are variables, but `aVar` is not).  In
judgments, variables stand for terms of any structure.  When applying
rules, a variable can be unified with any term.

We also have terms building our built-in types:
* Numeric constants build the integer (`int`) type.
* Double-quoted strings build the string type.
* Tuples are written as comma-separated lists of terms inside parentheses,
  such as `(1, 2, "x")`.
* Lists of constant length can be written in square brackets, such as
  `[1, 2, 3, 4]`.  Lists can also be written using cons notation, such
  as `1::2::Rest`, specifying a list where the first two elements are
  integer constants `1` and `2` and the remainder of the list may be
  anything.

Finally, we have one last type of term, an ascription, written as
`(<term> : <type>)`, asserting the term has the given type
(e.g. `(3 : int)`).  Sometimes it is difficult to understand why a
rule is ill-typed, and asserting the types of variables or larger
terms in strategic places can improve the type error messages given.
Other than possibly improving the error messages from typing, an
ascription is equivalent to the term inside it.


## Built-In Judgments
In addition to applications of defined relation judgments, SOS-Ext
provides several built-in judgment forms that can be used in writing
semantic rules.

One form is comparisons, written as `<term1> <op> <term2>`.  The
available operations are
* `=`:  Compares two terms of any (but equal) type, holding if they
  are syntactically equal
* `!=`:  Compares two terms of any (but equal) type, holding if they
  are not syntactically equal
* `<`:  Compares two integers, holding if the left is less than the
  right
* `>`:  Compares two integers, holding if the left is greater than the
  right
* `<=`:  Compares two integers, holding if the left is less than or
  equal to the right
* `>=`:  Compares two integers, holding if the left is greater than or
  equal to the right

Another form is operations, written as
`<term1> <op> <term2> = <term3>`.  These assert the operation applied
to the two operands on the left produces the term on the right.  The
available operations are
* `+`:  Addition of two integers
* `-`:  Subtraction of two integers
* `*`:  Multiplication of two integers
* `/`:  Division of two integers
* `%`:  Modulus of two integers
* `++`:  Append either two strings or two lists holding the same type

Another form is the translation of a constructor, written as
```
<args> |{<category>}- <term> ~~> <translation>
```
This is discussed in the [document about translation](translation.md).

One final form of built-in judgment is the negation of a relation
judgment, written as `! <rel> <args>` (e.g. `! mem X L`).  This holds
if the negated relation does not hold.


## Declaring Fixed Judgments
Fixed judgments are called "fixed" because they cannot be extended.
When a module introduces a fixed judgment, it also gives all the rules
defining the relation.

A fixed judgment is declared by giving its name and argument types.
For example, we might declare a relation to concatenate all the
strings in a list into a single string as
```
Fixed Judgment stringCat : [string] string
```
We can then write rules to define this relation.  Rules in general are
written as a (possibly empty) list of premises, a separation line, a
rule name, and a conclusion.  The separation line for fixed judgments
is built by a minimum of five equals signs, so our rules will look
like this:
```
<premise 1>
...
<premise n>
============ [<rule name>]
<conclusion>
```
For the `stringCat` relation, we would write two rules:
```
=============== [SC-Nil]
stringCat [] ""

stringCat Rest SRest
S ++ SRest = Full
====================== [SC-Cons]
stringCat S::Rest Full
```
It is generally a good idea to choose a short symbol for each relation
and prefix each rule name with it, but it is not required.

Fixed judgments should be used sparingly.  The purpose of an
extensible language is to allow new modules to add to the language's
definition, and they cannot if the language is defined using too many
fixed judgments.


## Declaring Extensible Judgments
Extensible judgments have the rules defining them written across
modules, with new modules adding new rules for the new additions to
the language syntax added.

An extensible judgment is declared by giving its name and argument
types, as we saw with fixed judgments.  However, the judgment must
also choose a *primary component* that will be used for restricting
the rules that can be written in modules building on the current one.
The primary component can be thought of as what the judgment is
"about".  For example, typing is *about* the expression being typed,
so we would choose the expression as the primary component.  We might
declare our typing relation thus:
```
Judgment typeOf : [(string, ty)] expr* ty
```
The `*` marks the expression as the primary component.

Rules for extensible judgments are written with a separating line of
hyphens made of hyphens, but are otherwise the same as for fixed
judgments:
```
typeOf Ctx E1 boolTy
typeOf Ctx E2 boolTy
---------------------------- [T-Or]
typeOf Ctx or(E1, E2) boolTy
```


## Adding Rules in Other Modules
If an extensible judgment `J` is declared in module `H` and module `E`
builds on `H`, module `E` can add new rules for `J`.  However, it is
restricted to adding new rules only for new constructors of `J`'s
primary component.  It cannot redefine the meaning of `J` on existing
constructors.  Then a new rule where the primary component is built by
the `H:c` constructor is not allowed, but one where the primary
component is built by the `E:d` constructor is allowed.

Consider the `typeOf` relation from above.  A new extension module
cannot write another typing rule for the `or` constructor.  However,
if a module introduces a new expression form `plus(expr, expr)`.  Then
it can also introduce a new typing rule for this expression form:
```
typeOf Ctx E1 intTy
typeOf Ctx E2 intTy
----------------------------- [T-Plus]
typeOf Ctx plus(E1, E2) intTy
```


## Imported Primary Component Type
Modules can define extensible judgments where the primary component is
a category imported from a built-on module.  However, other modules
building on the module introducing a category then cannot define this
judgment on the constructors they introduce.

Suppose the `expr` category is introduced by module `modExpr` and the
`typeOf` relation is introduced by module `modType` that builds on
`modExpr`.  A module `modOther` that builds on `modExpr` but not
`modType` can add new expression forms but does not know the `typeOf`
judgment exists to define it for them.

We solve this problem by having the module introducing an extensible
judgment give a rule for defining it for unknown constructors,
generally by copying from the translation.  This is similar to the
idea of [forwarding in
Silver](https://melt.cs.umn.edu/silver/ref/stmt/forwarding/).

These translation rules are written using the same syntax as other
rules for extensible languages, but they are marked as translation
rules with a star after the name.  For example, our typing relation
might have this translation rule:
```
|{expr}- E ~~> E_Trans
typeOf Ctx E_Trans Ty
---------------------- [T-Trans]*
typeOf Ctx E Ty
```
In a composed language including `modExpr`, `modType`, and `modOther`,
any expression form introduced by `modOther` will have `typeOf`
defined for it by copying its type from its translation.
