
* We cannot currently run any examples.  We need:
  + A translation for concrete syntax.
  + A way to specify how we want to do things---that is, write a
    `main` function, essentially.  Being able to specify all this
    isn't that useful if we can't then use it for programs.
  + A well-defined interface for interactions between translations of
    concrete syntax (parsers) and translations of abstract syntax
    (proof engines) so we can mix and match different choices for the
    two and still get results.
    * Probably a particular string format for the parser to return
      with the parsed program, like the standard
      ```
      constructor(constr(), constructor())
      ```

* We might add more translations:
  + Coq

* We can add some built-in types:
  + Polymorphic contexts where we can choose the key and value types,
    with lookup, containment, and non-containment relations
  + Polymorphic lists, along with some relations for them like append
  + Pairs, maybe, though they might not be useful once we have
    contexts, though we might just make contexts be lists of pairs

