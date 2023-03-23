# SOS-Ext Modules
Extensible languages in SOS-Ext are built by modules.  Each module can
introduce new concrete and abstract syntax, new semantic relations,
new rules defining semantic relations, and new functions for running
the language.  All modules are treated the same, whether they
conceptually represent a host language or extension.

A module is a collection of files in a single directory.  When
building a module, all files with SOS-Ext file extensions in the
module's directory are read and checked as part of the module.  There
is no distinction between the contents of different files of the same
[file category](langCategories.md).  For example, a relation defined
in `file_A.sos` can have rules defined in `file_B.sos` without any
problem.

A module contained in the directory `mo/du/le` will have the name
`mo:du:le`.  This directory/module name relationship comes from
[Silver](https://melt.cs.umn.edu/silver/concepts/modules/), an
attribute grammar system for writing extensible languages.

Each SOS-Ext file in the directory is expected to start with a
declaration of its module:
```
Module mo:du:le
```
This is a check to ensure modules are being handled correctly by the
user.


## Building on Modules
A module can build on another module using a builds-on declaration:
```
Builds on imp:ort
```
Builds-on declarations can only be given in semantic files, although
they have the effect of importing all the declarations, regardless of
category, from the built-on module.

In addition to the modules given in builds-on declarations, each
module builds on the [standard library](stdLib.md).


## Qualified Names
Each declaration in a grammar is written using a short name, but also
has a full, qualified name.  For example, we might define a syntactic
category named `term` in a module `lang:host`.  While we can use the
name `term` to refer to this category, its full name is qualified by
the name of the module defining it, giving us `lang:host:term`.  We
can use this full name wherever we can use the short name.  In an
extension `lang:ext` that builds on `lang:host`, we can use either the
short name or the full name for `term`.  This naming scheme was also
inspired by
[Silver](https://melt.cs.umn.edu/silver/concepts/modules/#names).

The reason we qualify names is so a language with two extensions
introducing the same short name does not have a conflict when they are
used together.  Suppose both module `a` and module `b` introduce
`name`.  If we did not have qualified names, another module building
on both `a` and `b` would have two `name`s without a way to
differentiate them.  However, with qualification these become `a:name`
and `b:name`, so the third module can refer to each unambiguously.
