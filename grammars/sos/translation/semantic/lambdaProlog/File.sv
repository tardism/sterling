grammar sos:translation:semantic:lambdaProlog;


attribute
   lpDecls, lpRules, lpProjectionRules
occurs on File;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.lpDecls = decls.lpDecls;

  top.lpRules = decls.lpRules;
  top.lpProjectionRules = decls.lpProjectionRules;
}





attribute
   lpDecls, lpRules, lpProjectionRules
occurs on Decls;

aspect production nilDecls
top::Decls ::=
{
  top.lpDecls = [];

  top.lpRules = [];
  top.lpProjectionRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.lpDecls = [];

  top.lpRules = [];
  top.lpProjectionRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.lpDecls = [];

  top.lpRules = r.lpRules;
  top.lpProjectionRules = r.lpProjectionRules;
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.lpDecls = a.lp;

  top.lpRules = [];
  top.lpProjectionRules = [];
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.lpDecls = j.lpDecls;

  top.lpRules = [];
  top.lpProjectionRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.lpDecls = d1.lpDecls ++ d2.lpDecls;

  top.lpRules = d1.lpRules ++ d2.lpRules;
  top.lpProjectionRules =
      d1.lpProjectionRules ++ d2.lpProjectionRules;
}

