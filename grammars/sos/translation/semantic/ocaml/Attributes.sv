grammar sos:translation:semantic:ocaml;

imports sos:core:common:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;

synthesized attribute ocaml<a>::a;
synthesized attribute ocamlExpr::OCamlExpr;
synthesized attribute ocamlType::OCamlType;
synthesized attribute ocamlPattern::OCamlPattern;
synthesized attribute ocamlString::String;

synthesized attribute ocamlExprs::[OCamlExpr];
synthesized attribute ocamlTypes::[OCamlType];
synthesized attribute ocamlConstructors::[OCamlConstructor];
synthesized attribute ocamlDecls::[OCamlDecl];

synthesized attribute ocamlBinOp::(OCamlExpr ::= OCamlExpr OCamlExpr OCamlExpr);
synthesized attribute ocamlTopBinOp::(OCamlExpr ::= OCamlExpr OCamlExpr);
synthesized attribute ocamlConjunction::(OCamlExpr ::= OCamlExpr);

