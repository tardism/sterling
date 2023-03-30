# Extensions
As SOS-Ext is written in [Silver](melt.cs.umn.edu/silver), an
extensible language system using attribute grammars, extensions can be
written to it in Silver.  Some extensions are included in the SOS-Ext
repository itself, and thus are part of any installation of it.  We
discuss these here.


## Translations
The main set of extensions we have are translations of the different
language categories to other languages.

SOS-Ext has a concept of runnable and non-runnable translations.  A
runnable translation is one that can be used to animate the language.
A non-runnable translation is one that does not do this.  In order to
produce an executable for the language, one needs to specify runnable
semantic and concrete translations, which will then result in a Java
JAR file being produced.

### Semantic Translations
SOS-Ext has one runnable translation of the semantic portion of the
language:
* Prolog:  This translates the language's rules into a Prolog
  specification.  It assumes `swipl` ([SWI-Prolog](swi-prolog.org)) is
  a program on the computer where it is running.  Note that running
  this translation is not guaranteed to produce a result, since it
  might lead to infinite search.  The executable produced by running
  this is also not portable, since it will search for the Prolog files
  in a particular location on the computer on which it was compiled.
  + Flag:  `--prolog`

We have several other semantic translations as well:
* Extensibella:  This produces a specification of the language
  [Extensibella](github.com/RandomActsOfGrammar/extensibella) can read
  to prove properties about the extensible language specified by the
  module.  Note that one must run the `build_extensibella` script in
  the [`stdLib`](../../stdLib/) directory in order to use Extensibella
  with any compiled specifications, since all SOS-Ext modules build on
  the standard library.
  + Flag:  `--extensibella`
* LaTeX:  The LaTeX extension will produce a dump of LaTeX code for
  the abstract syntax and rules defined in the current module in a
  file with the given name.  The relations and constructors in the
  rules are defined by macros, making it relatively easy to give
  better syntax to relations.
  + Flag:  `--latex <filename>`
* Lambda Prolog:  The Lambda Prolog translation produces [Lambda
  Prolog](www.lix.polytechnique.fr/~dale/lProlog/) signature and
  module files in the current directory for the given module, assuming
  built-on modules have been built in the current directory.
  Compiling a module `mo:du:le` produces files `mo-du-le.sig` and
  `mo-du-le.mod`.  This behavior is likely to be changed to producing
  the code in the `generated` directory as the Prolog translation
  does, with this becoming a runnable translation using the
  [Teyjus](github.com/teyjus/teyjus) implementation.
  + Flag:  `--lprolog`

### Concrete Translations
We currently have one runnable concrete translation:
* Silver:  This produces a Silver grammar for the concrete syntax, as
  well as parsers for each nonterminal parsed in a program.
  + Flag:  `--silver-concrete`

### Main Translations
* Silver:  This produces a set of Silver functions that delegate to
  whichever semantic and concrete runnable translations were chosen.
  At this time, this is triggered not by a flag given when running
  SOS-Ext, but by any runnable semantic and concrete translations
  being specified by flags.  This makes it currently tied into SOS-Ext
  more tightly than an extension normally would be, and this behavior
  might be modified in the future.


## Testing
