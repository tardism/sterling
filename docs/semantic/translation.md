# Translations
Because languages written using SOS-Ext are extensible, meaning
modules other than the one introducing a syntactic category can add
constructors building the category, we need a way to extend the
definition of new judgments over other constructors.  We do this by a
notion of *translation*.

Each constructor introduced by a module building on the module
introducing the syntactic category to which it belongs provides a
translation eliminating the new constructor.  Other modules also
provide translation rules (discussed in the [discussion of judgments
and rules](judgments.md)) for their new judgments to define them on
unknown syntax.  These two pieces together give each judgment a
complete definition, even though a complete definition could not be
given in any one module and the modules did not coordinate.


## Declaring Translation Types
Each syntactic category must have a translation relation.  This is
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
Quite often, we find we don't want translations to depend on anything,
so we have no arguments:
```
Translation stmt :
```


## Writing Translation Rules
In order for the language to be well-formed, each new constructor
given in a module for a syntactic category imported from a built-on
module must be given a translation rule.  The translation relation has
the form
```
<arguments> |{<category>}- <translating> ~~> <translation>
```
where the arguments have the types given by the `Translation`
declaration explained above.

Translations for new constructors are given as rules for the
extensible translation relation.  We might translate a `repeat-while`
into a regular `while` loop thus:
```
------------------------------------- [Trans-RepeatWhile]
|{stmt}- repeatWhile(Body, Cond) ~~>
         seq(Body, while(Cond, Body))
```
We have no premises in our rule because the translation doesn't depend
on anything.

However, sometimes we will have premises.  Suppose we have an
extension introduce an expression `unifiedAdd(E1, E2)` that is
equivalent to addition (`plus` constructor) for integers and
concatenation (`append` constructor) for lists.  Then we would have
two rules for its translation, each with a premise for the type of the
sub-expressions:
```
typeOf Ctx E1 intTy
------------------------------------------------ [Trans-UA1]
Ctx |{expr}- unifiedAdd(E1, E2) ~~> plus(E1, E2)

typeOf Ctx E2 listTy(Ty)
-------------------------------------------------- [Trans-UA2]
Ctx |{expr}- unifiedAdd(E1, E2) ~~> append(E1, E2)
```
