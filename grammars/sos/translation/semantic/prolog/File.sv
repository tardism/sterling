grammar sos:translation:semantic:prolog;


attribute
   prologProjectionRules,
   prologRules
occurs on File, Decls;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.prologProjectionRules = decls.prologProjectionRules;

  top.prologRules = decls.prologRules;
}



aspect production nilDecls
top::Decls ::=
{
  top.prologProjectionRules = [];
  top.prologRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.prologProjectionRules = [];
  top.prologRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.prologProjectionRules = r.prologProjectionRules;
  top.prologRules = r.prologRules;
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.prologProjectionRules = [];
  top.prologRules = [];
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.prologProjectionRules = [];
  top.prologRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.prologProjectionRules =
      d1.prologProjectionRules ++ d2.prologProjectionRules;
  top.prologRules = d1.prologRules ++ d2.prologRules;
}

