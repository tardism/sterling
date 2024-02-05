grammar sos:translation:semantic:extensibella:abstractSyntax;


abstract production standInRuleDecls
top::Decls ::= r::Rule
{
  top.pp = "Extensibella_Stand_In{\n" ++ r.pp ++ "}\n";

  r.moduleName = top.moduleName;

  r.judgmentEnv = top.judgmentEnv;
  r.translationEnv = top.translationEnv;
  r.ruleEnv = top.ruleEnv;
  r.tyEnv = top.tyEnv;
  r.constructorEnv = top.constructorEnv;

  top.ruleDecls =
      case r of
      | extRule(_, _, conc) ->
        if conc.isRelJudgment
        then case r.ruleDecls of
             | [extRuleEnvItem(fullName, _, _)] ->
               [standInRuleEnvItem(fullName, conc.headRel.name)]
             | _ -> [] --impossible without error
             end
        else []
      | _ -> []
      end;

  propagate errors;
  top.errors <-
      case r of
      | extRule(_, _, conc) ->
        if conc.isRelJudgment
        then (if !sameModule(top.moduleName, conc.headRel.name)
              then [errorMessage("Can only define Extensibella " ++
                       "stand-in rule for new relations; found one" ++
                       "for " ++ conc.headRel.name.pp,
                       location=top.location)]
              else []) ++
             (if !conc.allArgsVars
              then [errorMessage("Extensibella stand-in rule " ++
                       "conclusion must have all arguments be " ++
                       "variables", location=top.location)]
              else if length(nub(sort(conc.argVars))) !=
                      length(conc.argVars)
              then [errorMessage("Extensibella stand-in rule " ++
                       "conclusion must have all arguments be " ++
                       "unique variables", location=top.location)]
              else [])
        else if conc.isTransJudgment
        then [errorMessage("Extensibella stand-in rule must " ++
                 "define a new relation, not a translation",
                 location=top.location)]
        else [] --error caught by rule itself
      | fixedRule(_, _, _) ->
        [errorMessage("Extensibella stand-in rule must be " ++
            "extensible, not fixed", location=top.location)]
      | transRule(_, _, _) ->
        [errorMessage("Extensibella stand-in rule cannot be a " ++
            "default rule; remove the *", location=top.location)]
      end;

  top.ebKinds = [];
  top.ebConstrs = [];

  top.ebRules = [];
  top.ebTranslationRules = [];
  top.ebStandInRules =
      case r of
      | extRule(premises, _, conc) ->
        [(conc.headRel, conc.eb, premises.eb, conc.pcVar)]
      | _ -> [] --shouldn't access
      end;

  forwards to nilDecls(location=top.location);
}



abstract production standInRuleEnvItem
top::RuleEnvItem ::= name::QName definedRel::QName
{
  top.name = name;

  top.isExtensible = true;

  top.isError = false;

  top.definedRel = definedRel;

  top.isTransRule = false;

  --is it a translation/default rule?  sort of
  forwards to extRuleEnvItem(name, definedRel, true);
}
