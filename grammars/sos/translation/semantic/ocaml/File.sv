grammar sos:translation:semantic:ocaml;

attribute ocamlDecls occurs on File, Decls;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.ocamlDecls = decls.ocamlDecls;
}

aspect production nilDecls
top::Decls ::=
{
  top.ocamlDecls = [];
}

aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.ocamlDecls = [];
}

aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.ocamlDecls = r.ocamlDecls;
}

aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.ocamlDecls = a.ocamlDecls;
}

aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.ocamlDecls = [];
}

aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.ocamlDecls = d1.ocamlDecls ++ d2.ocamlDecls;
}
