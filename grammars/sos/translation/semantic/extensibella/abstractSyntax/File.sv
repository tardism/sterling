grammar sos:translation:semantic:extensibella:abstractSyntax;


attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebDefaultRules,
   ebStandInRules
occurs on File;

aspect production file
top::File ::= moduleName::QName decls::Decls
{
  top.ebKinds = decls.ebKinds;
  top.ebConstrs = decls.ebConstrs;
  top.ebRules = decls.ebRules;
  top.ebJudgments = decls.ebJudgments;
  top.ebDefaultRules = decls.ebDefaultRules;
  top.ebStandInRules = decls.ebStandInRules;
}





attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebDefaultRules,
   ebStandInRules
occurs on Decls;

aspect production nilDecls
top::Decls ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebDefaultRules = [];
  top.ebStandInRules = [];
}


aspect production buildsOnDecls
top::Decls ::= importName::QName
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebDefaultRules = [];
  top.ebStandInRules = [];
}


aspect production ruleDecls
top::Decls ::= r::Rule
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = r.ebRules;
  top.ebJudgments = [];
  top.ebDefaultRules = r.ebDefaultRules;
  top.ebStandInRules = [];
}


aspect production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.ebKinds = a.ebKinds;
  top.ebConstrs = a.ebConstrs;
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebDefaultRules = [];
  top.ebStandInRules = a.ebStandInRules;
}


aspect production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = j.ebJudgments;
  top.ebDefaultRules = [];
  top.ebStandInRules = [];
}


aspect production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.ebKinds = d1.ebKinds ++ d2.ebKinds;
  top.ebConstrs = d1.ebConstrs ++ d2.ebConstrs;
  top.ebRules = d1.ebRules ++ d2.ebRules;
  top.ebJudgments = d1.ebJudgments ++ d2.ebJudgments;
  top.ebDefaultRules =
      d1.ebDefaultRules ++ d2.ebDefaultRules;
  top.ebStandInRules = d1.ebStandInRules ++ d2.ebStandInRules;
}
