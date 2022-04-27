grammar sos:core:files:abstractSyntax;


nonterminal Rule with
   pp,
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   ruleDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv, ruleEnv,
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

  local fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];
  top.ruleDecls =
      if conclusion.isRelJudgment
      then [extRuleEnvItem(fullName, conclusion.headRel.name, false)]
      else if conclusion.isTransJudgment
      then [translationRuleEnvItem(fullName, conclusion.transType)]
      else [errorRuleEnvItem(fullName, false)];

  premises.tyEnv = top.tyEnv;
  premises.constructorEnv = top.constructorEnv;
  premises.judgmentEnv = top.judgmentEnv;
  premises.translationEnv = top.translationEnv;
  conclusion.tyEnv = top.tyEnv;
  conclusion.constructorEnv = top.constructorEnv;
  conclusion.judgmentEnv = top.judgmentEnv;
  conclusion.translationEnv = top.translationEnv;

  conclusion.isConclusion = true;
  conclusion.isExtensibleRule = true;
  conclusion.isTranslationRule = false;

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

  top.errors <-
      if !conclusion.isRelJudgment
      then if !conclusion.isTransJudgment
           then [errorMessage("Extensible rule " ++ name ++
                    " must have either a relation or translation " ++
                    "as its conclusion",
                    location=conclusion.location)]
           else [] --trans judgment is OK as conclusion
           else if !conclusion.headRel.isError &&
                   conclusion.headRel.isExtensible
                then []
                else [errorMessage("Extensible rule " ++ name ++
                         " must have an extensible relation as " ++
                         "its conclusion",
                         location=conclusion.location)];

  --Check there is only one declaration of this rule
  local possibleRules::[RuleEnvItem] =
        lookupEnv(fullName, top.ruleEnv);
  top.errors <-
      case possibleRules of
      | [] -> error("Impossible:  Rule " ++ fullName.pp ++ " must " ++
                    "exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for rule " ++
            fullName.pp, location=top.location)]
      end;
}

abstract production transRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.pp =
      premises.pp ++ "---------- [" ++ name ++ "]*\n" ++
      conclusion.pp ++ "\n";

  premises.moduleName = top.moduleName;
  conclusion.moduleName = top.moduleName;

  local fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];
  top.ruleDecls =
      if conclusion.isRelJudgment
      then [extRuleEnvItem(fullName, conclusion.headRel.name, true)]
      else [errorRuleEnvItem(fullName, true)];

  premises.tyEnv = top.tyEnv;
  premises.constructorEnv = top.constructorEnv;
  premises.judgmentEnv = top.judgmentEnv;
  premises.translationEnv = top.translationEnv;
  conclusion.tyEnv = top.tyEnv;
  conclusion.constructorEnv = top.constructorEnv;
  conclusion.judgmentEnv = top.judgmentEnv;
  conclusion.translationEnv = top.translationEnv;

  conclusion.isConclusion = true;
  conclusion.isExtensibleRule = true;
  conclusion.isTranslationRule = true;

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

  top.errors <-
      if !conclusion.isRelJudgment ||
         (!conclusion.headRel.isError &&
          !conclusion.headRel.isExtensible)
      then [errorMessage("Translation rule " ++ name ++
               " must have an extensible relation judgment as its " ++
               "conclusion", location=conclusion.location)]
      else [];

  --Check there is only one declaration of this rule
  local possibleRules::[RuleEnvItem] =
        lookupEnv(fullName, top.ruleEnv);
  top.errors <-
      case possibleRules of
      | [] -> error("Impossible:  Rule " ++ fullName.pp ++ " must " ++
                    "exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for rule " ++
            fullName.pp, location=top.location)]
      end;
}


abstract production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.pp =
      premises.pp ++ "========== [" ++ name ++ "]\n" ++
      conclusion.pp ++ "\n";

  premises.moduleName = top.moduleName;
  conclusion.moduleName = top.moduleName;

  local fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];
  top.ruleDecls =
      if conclusion.isRelJudgment
      then [fixedRuleEnvItem(fullName, conclusion.headRel.name)]
      else [errorRuleEnvItem(fullName, false)];

  premises.tyEnv = top.tyEnv;
  premises.constructorEnv = top.constructorEnv;
  premises.judgmentEnv = top.judgmentEnv;
  premises.translationEnv = top.translationEnv;
  conclusion.tyEnv = top.tyEnv;
  conclusion.constructorEnv = top.constructorEnv;
  conclusion.judgmentEnv = top.judgmentEnv;
  conclusion.translationEnv = top.translationEnv;

  conclusion.isConclusion = true;
  conclusion.isExtensibleRule = false;
  conclusion.isTranslationRule = false;

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

  top.errors <-
      if !conclusion.isRelJudgment ||
         (!conclusion.headRel.isError &&
          conclusion.headRel.isExtensible)
      then [errorMessage("Fixed rule " ++ name ++ " must have a " ++
               "fixed relation judgment as its conclusion",
               location=conclusion.location)]
      else [];

  --Check there is only one declaration of this rule
  local possibleRules::[RuleEnvItem] =
        lookupEnv(fullName, top.ruleEnv);
  top.errors <-
      case possibleRules of
      | [] -> error("Impossible:  Rule " ++ fullName.pp ++ " must " ++
                    "exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for rule " ++
            fullName.pp, location=top.location)]
      end;
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

  j.isConclusion = false;
  j.isExtensibleRule = false;
  j.isTranslationRule = false;

  j.downSubst = top.downSubst;
  rest.downSubst = j.upSubst;
  top.upSubst = rest.upSubst;
  j.finalSubst = top.finalSubst;
  rest.finalSubst = top.finalSubst;

  j.downVarTypes = top.downVarTypes;
  rest.downVarTypes = j.upVarTypes;
  top.upVarTypes = rest.upVarTypes;
}

