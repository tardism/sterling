grammar sos:core:semanticDefs:abstractSyntax;


nonterminal File with
   pp,
   tyDecls, constructorDecls, judgmentDecls, projectionDecls,
   ruleDecls, buildsOnDecls,
   tyEnv, constructorEnv, judgmentEnv, projectionEnv, ruleEnv,
   moduleName,
   projRuleConstructors, projRuleConstructors_down,
   errors,
   location;
propagate errors on File;

abstract production file
top::File ::= moduleName::QName decls::Decls
{
  top.pp = "Module " ++ moduleName.pp ++ "\n\n" ++ decls.pp;

  decls.moduleName = top.moduleName;

  top.errors <-
      if moduleName.pp == top.moduleName.pp
      then []
      else [errorMessage("Module declaration is incorrect:  " ++
                         moduleName.pp, location=top.location)];

  top.tyDecls = decls.tyDecls;
  top.constructorDecls = decls.constructorDecls;
  top.judgmentDecls = decls.judgmentDecls;
  top.projectionDecls = decls.projectionDecls;
  top.ruleDecls = decls.ruleDecls;
  top.buildsOnDecls = decls.buildsOnDecls;
  top.projRuleConstructors = decls.projRuleConstructors;

  decls.tyEnv = top.tyEnv;
  decls.constructorEnv = top.constructorEnv;
  decls.judgmentEnv = top.judgmentEnv;
  decls.projectionEnv = top.projectionEnv;
  decls.ruleEnv = top.ruleEnv;
  decls.projRuleConstructors_down = top.projRuleConstructors_down;
}





nonterminal Decls with
   pp,
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, projectionDecls,
   ruleDecls,
   tyEnv, constructorEnv, judgmentEnv, projectionEnv, ruleEnv,
   projRuleConstructors, projRuleConstructors_down,
   errors,
   location,
   buildsOnDecls;
propagate errors on Decls;

abstract production nilDecls
top::Decls ::=
{
  top.pp = "";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.projectionDecls = [];
  top.ruleDecls = [];
  top.buildsOnDecls = [];
  top.projRuleConstructors = [];
}


abstract production buildsOnDecls
top::Decls ::= importName::QName
{
  top.pp = "Builds on " ++ importName.pp ++ "\n";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.projectionDecls = [];
  top.ruleDecls = [];
  top.buildsOnDecls = [importName];
  top.projRuleConstructors = [];
}


abstract production ruleDecls
top::Decls ::= r::Rule
{
  top.pp = r.pp;

  r.moduleName = top.moduleName;

  top.tyDecls = r.tyDecls;
  top.constructorDecls = r.constructorDecls;
  top.judgmentDecls = r.judgmentDecls;
  top.projectionDecls = r.projectionDecls;
  top.ruleDecls = r.ruleDecls;
  top.buildsOnDecls = [];
  top.projRuleConstructors = r.projRuleConstructors;

  r.tyEnv = top.tyEnv;
  r.constructorEnv = top.constructorEnv;
  r.judgmentEnv = top.judgmentEnv;
  r.projectionEnv = top.projectionEnv;
  r.ruleEnv = top.ruleEnv;
}


abstract production absSyntaxDecls
top::Decls ::= a::AbsSyntaxDecl
{
  top.pp = a.pp;

  a.moduleName = top.moduleName;

  top.tyDecls = a.tyDecls;
  top.constructorDecls = a.constructorDecls;
  top.judgmentDecls = a.judgmentDecls;
  top.projectionDecls = a.projectionDecls;
  top.ruleDecls = [];
  top.buildsOnDecls = [];
  top.projRuleConstructors = [];

  a.tyEnv = top.tyEnv;
  a.constructorEnv = top.constructorEnv;
  a.judgmentEnv = top.judgmentEnv;
  a.projectionEnv = top.projectionEnv;
  a.projRuleConstructors_down = top.projRuleConstructors_down;
}


abstract production judgmentDecls
top::Decls ::= j::JudgmentDecl
{
  top.pp = j.pp;

  j.moduleName = top.moduleName;

  top.tyDecls = j.tyDecls;
  top.constructorDecls = j.constructorDecls;
  top.judgmentDecls = j.judgmentDecls;
  top.projectionDecls = j.projectionDecls;
  top.ruleDecls = [];
  top.buildsOnDecls = [];
  top.projRuleConstructors = [];

  j.tyEnv = top.tyEnv;
  j.constructorEnv = top.constructorEnv;
  j.judgmentEnv = top.judgmentEnv;
  j.projectionEnv = top.projectionEnv;
  j.ruleEnv = top.ruleEnv;
}


abstract production branchDecls
top::Decls ::= d1::Decls d2::Decls
{
  top.pp = d1.pp ++ "\n" ++ d2.pp;

  d1.moduleName = top.moduleName;
  d2.moduleName = top.moduleName;

  top.tyDecls = d1.tyDecls ++ d2.tyDecls;
  top.constructorDecls = d1.constructorDecls ++ d2.constructorDecls;
  top.judgmentDecls = d1.judgmentDecls ++ d2.judgmentDecls;
  top.projectionDecls = d1.projectionDecls ++ d2.projectionDecls;
  top.ruleDecls = d1.ruleDecls ++ d2.ruleDecls;
  top.buildsOnDecls = d1.buildsOnDecls ++ d2.buildsOnDecls;
  top.projRuleConstructors =
      d1.projRuleConstructors ++ d2.projRuleConstructors;

  d1.tyEnv = top.tyEnv;
  d1.constructorEnv = top.constructorEnv;
  d1.judgmentEnv = top.judgmentEnv;
  d1.projectionEnv = top.projectionEnv;
  d1.ruleEnv = top.ruleEnv;
  d1.projRuleConstructors_down = top.projRuleConstructors_down;
  d2.tyEnv = top.tyEnv;
  d2.constructorEnv = top.constructorEnv;
  d2.judgmentEnv = top.judgmentEnv;
  d2.projectionEnv = top.projectionEnv;
  d2.ruleEnv = top.ruleEnv;
  d2.projRuleConstructors_down = top.projRuleConstructors_down;
}

