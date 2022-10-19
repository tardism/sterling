grammar sos:translation:semantic:latex;


imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;


synthesized attribute latex<a>::a;

--gather things up
synthesized attribute latexRules::[LaTeXRule];
synthesized attribute latexSyntax::[LaTeXAbsSyn];


--LaTeX string output
synthesized attribute ppLaTeX::String;
inherited attribute ppLaTeXIndent::Integer; --how far to indent
