# Language Categories
Sterling language definitions are split into three categories:
semantic, concrete, and main.  We discuss each briefly below.


## Semantic
The semantic category defines the abstract syntax of a language, as
well as its semantic relations.  It does not use any definitions given
by the other categories.  This category is the heart of Sterling.  The
other two categories are simply there to support it.

Semantic files use the extension `.sos`.


## Concrete
The concrete category defines the concrete syntax of a language and
how it maps to the abstract syntax.  In essence, the concrete category
builds on the semantic category's definitions.

Concrete files use the extension `.conc`.


## Main
The main category defines how the language should run on files,
parsing them using the grammar given by the concrete category and
deriving relations defined by the semantic category.

Main files use the extension `.main`.
