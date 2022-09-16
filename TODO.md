
* We cannot currently run any examples.  We need:
  + A way to write a parser for concrete syntax.  This can probably
    turn into a Silver specification for concrete syntax.
  + A way to specify how we want to do things---that is, write a
    `main` function, essentially.  Being able to specify all this
    isn't that useful if we can't then use it for programs.

* We might add more translations:
  + Coq
  + Abella
  + Lambda Prolog
  + LaTeX

* We can add some built-in types:
  + Polymorphic contexts where we can choose the key and value types,
    with lookup, containment, and non-containment relations
  + Polymorphic lists, along with some relations for them like append
  + Pairs, maybe, though they might not be useful once we have
    contexts

