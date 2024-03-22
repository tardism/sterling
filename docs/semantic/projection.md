# Projections
Because languages written using Sterling are extensible, meaning
modules other than the one introducing a syntactic category can add
constructors building the category, we need a way to extend the
definition of new judgments over other constructors.  We do this by a
notion of *projection*.

Each constructor introduced by a module building on the module
introducing the syntactic category to which it belongs provides a
projection eliminating the new constructor.  Other modules also
provide projection rules (discussed in the [discussion of judgments
and rules](judgments.md)) for their new judgments to define them on
unknown syntax.  These two pieces together give each judgment a
complete definition, even though a complete definition could not be
given in any one module and the modules did not coordinate.


## Declaring Projection Types
Each syntactic category must have a projection relation.  This is
declared as
```
Projection <category> : <space-separated argument types>
```
The arguments here can be used to define projections for extension
syntax, as we will see below.  For example, we might define
projection for an expression to depend on a typing context:
```
Projection expr : typingCtx
```
Quite often, we find we don't want projections to depend on anything,
so we have no arguments:
```
Projection stmt :
```


## Writing Projection Rules
In order for the language to be well-formed, each new constructor
given in a module for a syntactic category imported from a built-on
module must be given a projection rule.  The projection relation has
the form
```
<arguments> |{<category>}- <projecting> ~~> <projection>
```
where the arguments have the types given by the `Projection`
declaration explained above.

Projections for new constructors are given as rules for the
extensible projection relation.  We might project a `repeat-while`
into a regular `while` loop thus:
```
------------------------------------- [Proj-RepeatWhile]
|{stmt}- repeatWhile(Body, Cond) ~~>
         seq(Body, while(Cond, Body))
```
We have no premises in our rule because the projection doesn't depend
on anything.

However, sometimes we will have premises.  Suppose we have an
extension introduce an expression `unifiedAdd(E1, E2)` that is
equivalent to addition (`plus` constructor) for integers and
concatenation (`append` constructor) for lists.  Then we would have
two rules for its projection, each with a premise for the type of the
sub-expressions:
```
typeOf Ctx E1 intTy
------------------------------------------------ [Proj-UA1]
Ctx |{expr}- unifiedAdd(E1, E2) ~~> plus(E1, E2)

typeOf Ctx E2 listTy(Ty)
-------------------------------------------------- [Proj-UA2]
Ctx |{expr}- unifiedAdd(E1, E2) ~~> append(E1, E2)
```
