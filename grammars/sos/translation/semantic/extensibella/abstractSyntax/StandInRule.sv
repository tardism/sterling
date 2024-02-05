grammar sos:translation:semantic:extensibella:abstractSyntax;


abstract production standInRuleDecls
top::Decls ::= r::Rule
{
  top.pp = "Extensibella_Stand_In{\n" ++ r.pp ++ "}\n";

  r.judgmentEnv = top.judgmentEnv;
  r.translationEnv = top.translationEnv;
  r.ruleEnv = top.ruleEnv;
  r.tyEnv = top.tyEnv;
  r.constructorEnv = top.constructorEnv;

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
              else [])
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
