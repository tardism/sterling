# Standard Library
Sterling contains a standard library for helpful relations and
functions that might be of common use.  As these are part of the
standard library module, they are qualified with the module name
`sterling:stdLib` (e.g. `sterling:stdLib:tail` is a function in the
standard library).

The standard library can be found [here](../stdLib).  This directory
only contains the definitions that can be written in Sterling itself.
In addition to these, there are some functions in the `main` category
listed below that cannot be written in Sterling but are available as
part of the standard library.


## Semantic
The standard library includes several fixed relations for working with
lists, found in [lists.sos](../stdLib/lists.sos).  This file contains
both the relations and their rules.


## Concrete
There are currently no definitions in the standard library for the
concrete category.


## Main
The standard library includes three functions for working with lists
in `main` files, none of which can be written in Sterling itself:
* `sterling:stdLib:tail`:  This function has type `[A] -> [A]`, producing
  the list minus the first element if the list is non-empty.  If the
  list is empty, this will crash the program.
* `sterling:stdLib:head`:  This function has type `[A] -> A`, producing the
  first element of the list if the list is non-empty.  If the list is
  empty, this will crash the program.
* `sterling:stdLib:null`:  This function has type `[A] -> bool`, producing
  `true` if the list is empty and `false` otherwise.
