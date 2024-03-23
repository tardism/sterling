grammar sos:translation:semantic:prolog;


attribute
   prologDefaultRules,
   prologRules
occurs on File, Decls;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.prologDefaultRules = decls.prologDefaultRules;

  top.prologRules = decls.prologRules;
}



aspect production nilDecls
top::Decls ::=
{
  top.prologDefaultRules = [];
  top.prologRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.prologDefaultRules = [];
  top.prologRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.prologDefaultRules = r.prologDefaultRules;
  top.prologRules = r.prologRules;
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.prologDefaultRules = [];
  top.prologRules = [];
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.prologDefaultRules = [];
  top.prologRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.prologDefaultRules =
      d1.prologDefaultRules ++ d2.prologDefaultRules;
  top.prologRules = d1.prologRules ++ d2.prologRules;
}

