grammar sos:translation:semantic:extensibella:abstractSyntax;


attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebTranslationRules,
   ebStandInRules
occurs on File;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.ebKinds = decls.ebKinds;
  top.ebConstrs = decls.ebConstrs;
  top.ebRules = decls.ebRules;
  top.ebJudgments = decls.ebJudgments;
  top.ebTranslationRules = decls.ebTranslationRules;
  top.ebStandInRules = decls.ebStandInRules;
}





attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebTranslationRules,
   ebStandInRules
occurs on Decls;

aspect production nilDecls
top::Decls ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];
  top.ebStandInRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];
  top.ebStandInRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = r.ebRules;
  top.ebJudgments = [];
  top.ebTranslationRules = r.ebTranslationRules;
  top.ebStandInRules = [];
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.ebKinds = a.ebKinds;
  top.ebConstrs = a.ebConstrs;
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];
  top.ebStandInRules = a.ebStandInRules;
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = j.ebJudgments;
  top.ebTranslationRules = [];
  top.ebStandInRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.ebKinds = d1.ebKinds ++ d2.ebKinds;
  top.ebConstrs = d1.ebConstrs ++ d2.ebConstrs;
  top.ebRules = d1.ebRules ++ d2.ebRules;
  top.ebJudgments = d1.ebJudgments ++ d2.ebJudgments;
  top.ebTranslationRules =
      d1.ebTranslationRules ++ d2.ebTranslationRules;
  top.ebStandInRules = d1.ebStandInRules ++ d2.ebStandInRules;
}
