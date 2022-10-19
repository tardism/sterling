grammar sos:translation:semantic:latex;


attribute latexRules, latexSyntax occurs on File, Decls;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.latexRules = decls.latexRules;
  top.latexSyntax = decls.latexSyntax;
}



aspect production nilDecls
top::Decls ::=
{
  top.latexRules = [];
  top.latexSyntax = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.latexRules = [];
  top.latexSyntax = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.latexRules = [r.latex];
  top.latexSyntax = [];
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.latexRules = [];
  top.latexSyntax = [a.latex];
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.latexRules = [];
  top.latexSyntax = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.latexRules = d1.latexRules ++ d2.latexRules;
  top.latexSyntax = d1.latexSyntax ++ d2.latexSyntax;
}
