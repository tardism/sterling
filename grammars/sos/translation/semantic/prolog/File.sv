grammar sos:translation:semantic:prolog;


attribute
   prologTranslationRules,
   prologRules
occurs on File, Decls;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.prologTranslationRules = decls.prologTranslationRules;

  top.prologRules = decls.prologRules;
}



aspect production nilDecls
top::Decls ::=
{
  top.prologTranslationRules = [];
  top.prologRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.prologTranslationRules = [];
  top.prologRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.prologTranslationRules = r.prologTranslationRules;
  top.prologRules = r.prologRules;
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.prologTranslationRules = [];
  top.prologRules = [];
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.prologTranslationRules = [];
  top.prologRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.prologTranslationRules =
      d1.prologTranslationRules ++ d2.prologTranslationRules;
  top.prologRules = d1.prologRules ++ d2.prologRules;
}

