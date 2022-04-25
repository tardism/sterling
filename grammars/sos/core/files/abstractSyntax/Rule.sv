grammar sos:core:files:abstractSyntax;


nonterminal Rule with
   pp,
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   errors,
   location;
propagate errors on Rule;

abstract production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.pp =
      premises.pp ++ "---------- [" ++ name ++ "]\n" ++
      conclusion.pp ++ "\n";

  premises.moduleName = top.moduleName;
  conclusion.moduleName = top.moduleName;

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];

  premises.tyEnv = top.tyEnv;
  premises.constructorEnv = top.constructorEnv;
  premises.judgmentEnv = top.judgmentEnv;
  premises.translationEnv = top.translationEnv;
  conclusion.tyEnv = top.tyEnv;
  conclusion.constructorEnv = top.constructorEnv;
  conclusion.judgmentEnv = top.judgmentEnv;
  conclusion.translationEnv = top.translationEnv;

  premises.downSubst = emptySubst();
  conclusion.downSubst = premises.upSubst;
  --rule is unit of determining var types, so turn finalSubst here
  premises.finalSubst = conclusion.upSubst;
  conclusion.finalSubst = conclusion.upSubst;

  --get any unification errors
  top.errors <- errorsFromSubst(conclusion.upSubst);

  --initially no variable types are known
  premises.downVarTypes = [];
  conclusion.downVarTypes = premises.upVarTypes;
}

abstract production transRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.pp =
      premises.pp ++ "---------- [" ++ name ++ "]*\n" ++
      conclusion.pp ++ "\n";

  premises.moduleName = top.moduleName;
  conclusion.moduleName = top.moduleName;

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];

  premises.tyEnv = top.tyEnv;
  premises.constructorEnv = top.constructorEnv;
  premises.judgmentEnv = top.judgmentEnv;
  premises.translationEnv = top.translationEnv;
  conclusion.tyEnv = top.tyEnv;
  conclusion.constructorEnv = top.constructorEnv;
  conclusion.judgmentEnv = top.judgmentEnv;
  conclusion.translationEnv = top.translationEnv;

  premises.downSubst = emptySubst();
  conclusion.downSubst = premises.upSubst;
  --rule is unit of determining var types, so turn finalSubst here
  premises.finalSubst = conclusion.upSubst;
  conclusion.finalSubst = conclusion.upSubst;

  --get any unification errors
  top.errors <- errorsFromSubst(conclusion.upSubst);

  --initially no variable types are known
  premises.downVarTypes = [];
  conclusion.downVarTypes = premises.upVarTypes;
}


abstract production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.pp =
      premises.pp ++ "========== [" ++ name ++ "]\n" ++
      conclusion.pp ++ "\n";

  premises.moduleName = top.moduleName;
  conclusion.moduleName = top.moduleName;

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];

  premises.tyEnv = top.tyEnv;
  premises.constructorEnv = top.constructorEnv;
  premises.judgmentEnv = top.judgmentEnv;
  premises.translationEnv = top.translationEnv;
  conclusion.tyEnv = top.tyEnv;
  conclusion.constructorEnv = top.constructorEnv;
  conclusion.judgmentEnv = top.judgmentEnv;
  conclusion.translationEnv = top.translationEnv;

  premises.downSubst = emptySubst();
  conclusion.downSubst = premises.upSubst;
  --rule is unit of determining var types, so turn finalSubst here
  premises.finalSubst = conclusion.upSubst;
  conclusion.finalSubst = conclusion.upSubst;

  --get any unification errors
  top.errors <- errorsFromSubst(conclusion.upSubst);

  --initially no variable types are known
  premises.downVarTypes = [];
  conclusion.downVarTypes = premises.upVarTypes;
}





nonterminal JudgmentList with
   pp,
   moduleName,
   toList<Judgment>,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   errors, downSubst, upSubst, finalSubst,
   downVarTypes, upVarTypes,
   location;
propagate errors on JudgmentList;

abstract production nilJudgmentList
top::JudgmentList ::=
{
  top.pp = "";

  top.toList = [];

  top.upSubst = top.downSubst;

  top.upVarTypes = top.downVarTypes;
}


abstract production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.pp = j.pp ++ "\n" ++ rest.pp;

  j.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  top.toList = j::rest.toList;

  j.tyEnv = top.tyEnv;
  j.constructorEnv = top.constructorEnv;
  j.judgmentEnv = top.judgmentEnv;
  j.translationEnv = top.translationEnv;
  rest.tyEnv = top.tyEnv;
  rest.constructorEnv = top.constructorEnv;
  rest.judgmentEnv = top.judgmentEnv;
  rest.translationEnv = top.translationEnv;

  j.downSubst = top.downSubst;
  rest.downSubst = j.upSubst;
  top.upSubst = rest.upSubst;
  j.finalSubst = top.finalSubst;
  rest.finalSubst = top.finalSubst;

  j.downVarTypes = top.downVarTypes;
  rest.downVarTypes = j.upVarTypes;
  top.upVarTypes = rest.upVarTypes;
}

