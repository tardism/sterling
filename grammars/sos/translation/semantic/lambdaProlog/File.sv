grammar sos:translation:semantic:lambdaProlog;


attribute
   lpDecls, lpRules, lpTranslationRules
occurs on File;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.lpDecls = decls.lpDecls;

  top.lpRules = decls.lpRules;
  top.lpTranslationRules = decls.lpTranslationRules;
}





attribute
   lpDecls, lpRules, lpTranslationRules
occurs on Decls;

aspect production nilDecls
top::Decls ::=
{
  top.lpDecls = [];

  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.lpDecls = [];

  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.lpDecls = [];

  top.lpRules = r.lpRules;
  top.lpTranslationRules = r.lpTranslationRules;
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.lpDecls = a.lp;

  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.lpDecls = j.lpDecls;

  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.lpDecls = d1.lpDecls ++ d2.lpDecls;

  top.lpRules = d1.lpRules ++ d2.lpRules;
  top.lpTranslationRules =
      d1.lpTranslationRules ++ d2.lpTranslationRules;
}

