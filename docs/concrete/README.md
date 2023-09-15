# Concrete Language
The concrete portion of Sterling defines concrete syntax for parsing
files written in a language.  Concrete syntax is defined using a
grammar, with each grammar production mapping to an abstract syntax
term.


## Terminal Declarations
The grammar is built on terminals defined by regular expressions.
Terminals are declared with a name as follows:
```
<name>   /<regular expression>/
```
For example, we might declare lowercase names for a programming
language as
```
name_t   /[a-z][a-zA-Z0-9_]*/
```
Names of terminals must be lowercase.

Regular expressions are generally as expected.  Special characters
must be escaped using backslashes.  For example, to have a backslash
character or a period as a terminal, we would need to escape them:
```
backslash_t /\\/
dot_t   /\./
```
Spaces are ignored in regular expressions, but can be included by
backslash escaping them.

Terminals that will be ignored by all productions in the grammar can
be declared as ignore terminals.  For example, we might want to ignore
whitespace:
```
ignore /[\n\r\t\ ]+/
```
Strings matching ignore terminals may occur anywhere in a string being
parsed by the grammar and will be skipped.


## Grammar Declarations
A grammar is built by declaring nonterminals and productions building
those nonterminals.

### Nonterminals
Each nonterminal is declared with the type of abstract syntax its
productions will build.  The types that may be declared as the
abstract syntax type are the types in the [semantic portion of the
language](../semantic/syntax.md).  For example, we might declare a
nonterminal `term_c` building abstract syntax `term` as
```
term_c<term> ::= ...
```

### Productions
Nonterminals and their abstract syntax types are declared together
with productions building them.  Productions are built from terminals
and nonterminals that may be given names to reference them.  We might
declare a production to parse lambda abstractions such as
`\name. body` for terms as
```
term_c<term> ::= backslash_t Name::name_t dot_t Body::term_c
```
Any names given to elements of the production must be capitalized.
Each production must also include a term defining its abstract syntax,
discussed [below](#abstract-syntax).

Several productions can be declared together by separating them with
vertical bars, as we can do with declaring constructors for [abstract
syntax](../semantic/syntax.md), as
```
nt<ty> ::= prod1
         | prod2
         | prod3
```

Because we are working with extensible languages, we also need to be
able to add new productions to existing nonterminals.  We can do this
as follows:
```
nt ::= ...
     | new_prod1
     | new_prod2
```
Note that we do not re-declare the abstract syntax type.  The type for
the abstract syntax is already known from the initial declaration of
the nonterminal, and thus does not need to be restated.

### Abstract Syntax
Each production must include an abstract syntax term.  These are
written after the production and an arrow `~~>`.  For example, the
lambda abstract from above might build its term using an abstract
syntax constructor `abs`:
```
term_c<term> ::= backslash_t Name::name_t dot_t Body::term_c ~~>
                    abs(Name, Body)
```
This uses the name and body from the production to build a new term.

The constructs available for the abstract syntax:
* Production elements:  Names declared for elements of the production
  can be used in building the abstract syntax.  Names referring to
  terminals produce the string text matched by that terminal.  Names
  referring to nonterminals produce the abstract syntax built by the
  productions building that nonterminal instance.
* Abstract syntax constructors:  Constructors known in the current
  module, both those declared in the module and those from modules it
  builds on, can be used.
* String literal:  String literals, written in double quotes, can be
  used in the abstract syntax.
* Integer literal
* Conversion to integers:  Strings can be converted to integers as
  `$to_int(<string>)`.  A common use of this would be converting the
  text matching a terminal representing an integer literal into an
  actual integer.
* Substring:  Sometimes a terminal will match more than we want to
  place into the abstract syntax, so we need to take only part of it.
  This can be done by taking a substring, written as
  `<string>[<start>:<end>]` where `start` and `end` are zero-based
  indices into the string.
* Lists:  Lists can be written in terms, as they can be written in the
  abstract syntax, as list literals (e.g. `[1, 2, 3]`) or using the
  cons list constructor (e.g. `Hd::Tl`).
* Tuples:  Tuples can be written in terms, as they can be written in
  the abstract syntax as a comma-separated list between parentheses
  (e.g. `(1, 2, 3)`).
