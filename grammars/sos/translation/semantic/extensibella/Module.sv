grammar sos:translation:semantic:extensibella;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;

attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebTranslationRules
occurs on ModuleList;

aspect production nilModuleList
top::ModuleList ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRulesByModule = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.ebKinds = m.ebKinds ++ rest.ebKinds;
  top.ebConstrs = m.ebConstrs ++ rest.ebConstrs;
  top.ebJudgments = m.ebJudgments ++ rest.ebJudgments;
  top.ebTranslationRules =
      m.ebTranslationRules ++ rest.ebTranslationRules;
  top.ebRulesByModule =
      case m.ebRulesByModule of
      | [(mod, rules)] ->
        [(mod, rules ++ instantiatedTransRules)]
      | _ -> error("Not possible")
      end ++ rest.ebRulesByModule;
  local instantiatedTransRules::[Def] =
     flatMap(
        \ p::(JudgmentEnvItem, [ConstructorEnvItem]) ->
          case lookupBy(\ j1::JudgmentEnvItem j2::JudgmentEnvItem ->
                          j1.name == j2.name,
                        p.1, top.ebTranslationRules) of
          | just((conc, prems, pc)) ->
            let used_vars::[String] =
                flatMap((.vars), prems) ++ conc.vars
            in
            let pcless_vars::[String] =
                remove(pc, used_vars)
            in
              map(\ c::ConstructorEnvItem ->
                    let childNames::[String] =
                        foldr(\ x::Type rest::[String] ->
                                case x of
                                | nameType(n) ->
                                  freshName(capitalize(n.base),
                                            rest ++ pcless_vars)
                                | intType() ->
                                  freshName("I", rest ++ pcless_vars)
                                | stringType() ->
                                  freshName("S", rest ++ pcless_vars)
                                | _ ->
                                  freshName("X", rest ++ pcless_vars)
                                end::rest,
                              [], c.types.toList)
                    in
                    let tm::ExtensibellaTerm =
                        applicationExtensibellaTerm(
                           decorate c.name with {
                              constructorEnv = m.constructorEnv;
                           }.ebConstructorName,
                           map(varExtensibellaTerm, childNames))
                    in
                    let newConc::Metaterm =
                        decorate conc with {
                           replaceVar = pc;
                           replaceVal = tm;
                        }.replaced
                    in
                    let newPrems::[Metaterm] =
                        map(\ m::Metaterm ->
                              decorate m with {
                                 replaceVar = pc;
                                 replaceVal = tm;
                              }.replaced, prems)
                    in
                    let newPremVars::[String] =
                        removeAll(newConc.vars,
                           nub(flatMap((.vars), newPrems)))
                    in
                      if null(newPrems)
                      then factDef(newConc)
                      else if null(newPremVars)
                      then ruleDef(newConc,
                              foldr1(andMetaterm, newPrems))
                      else ruleDef(newConc,
                              existsMetaterm(newPremVars,
                                foldr1(andMetaterm, newPrems)))
                    end end end end end,
                  p.2)
            end end
          | nothing() -> [] --host relations have no translation rule
          end,
        newRuleCombinations);
}





attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebTranslationRules
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  top.ebKinds = files.ebKinds;
  top.ebConstrs = files.ebConstrs;
  top.ebRulesByModule = [(name, files.ebRules)];
  top.ebJudgments = files.ebJudgments;
  top.ebTranslationRules = files.ebTranslationRules;
}





attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebTranslationRules
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.ebKinds = f.ebKinds ++ rest.ebKinds;
  top.ebConstrs = f.ebConstrs ++ rest.ebConstrs;
  top.ebRules = f.ebRules ++ rest.ebRules;
  top.ebJudgments = f.ebJudgments ++ rest.ebJudgments;
  top.ebTranslationRules =
      f.ebTranslationRules ++ rest.ebTranslationRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.ebKinds = rest.ebKinds;
  top.ebConstrs = rest.ebConstrs;
  top.ebRules = rest.ebRules;
  top.ebJudgments = rest.ebJudgments;
  top.ebTranslationRules = rest.ebTranslationRules;
}
