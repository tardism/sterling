grammar sos:translation:semantic:extensibella:abstractSyntax;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;

attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebDefaultRules, ebStandInRules, ebErrors,
   defFileContents, interfaceFileContents, fullFileContents
occurs on ModuleList;

synthesized attribute defFileContents::String;
synthesized attribute interfaceFileContents::String;
--definition for the non-extensible version of the language
synthesized attribute fullFileContents::String;

aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.ebKinds = files.ebKinds;
  top.ebConstrs = files.ebConstrs;
  top.ebRulesByModule = [(stdLibName, files.ebRules ++ isRules)];
  top.ebJudgments = files.ebJudgments ++ isRels;
  top.ebDefaultRules = files.ebDefaultRules;
  top.ebStandInRules = files.ebStandInRules;

  --automatically generate is relations
  local isRels::[(String, [ExtensibellaType])] =
      map(\ t::TypeEnvItem ->
            (t.name.ebIsName,
             [extensibellaNameTy(t.name.ebTypeName)]),
          files.tyDecls);
  local isRules::[Def] =
      map(\ c::ConstructorEnvItem ->
            let children::[(String, Metaterm)] =
                foldr(\ t::Type rest::[(String, Metaterm)] ->
                        let newName::String =
                            freshNameFromType(t, map(fst, rest))
                        in
                          (newName,
                           relationMetaterm(t.ebIs,
                              [nameExtensibellaTerm(newName)]))::rest
                        end,
                      [], c.types.toList)
            in
              if null(children)
              then factDef(relationMetaterm(c.type.ebIs,
                    [nameExtensibellaTerm(c.name.ebConstructorName)]))
              else ruleDef(
                      relationMetaterm(c.type.ebIs,
                         [applicationExtensibellaTerm(
                             c.name.ebConstructorName,
                             map(nameExtensibellaTerm,
                                 map(fst, children)))]),
                      foldr1(andMetaterm, map(snd, children)))
            end,
          files.constructorDecls);

  top.defFileContents =
      buildExtensibellaFile(top.ebKinds, top.ebConstrs,
         top.ebJudgments, top.ebRulesByModule, [], []);
  top.interfaceFileContents =
      buildExtensibellaInterfaceFile(stdLibName, [(stdLibName, [])]);
  top.fullFileContents =
      buildExtensibellaFile(top.ebKinds, top.ebConstrs,
         top.ebJudgments, top.ebRulesByModule, [], []);

  top.ebErrors = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.ebKinds = m.ebKinds ++ rest.ebKinds;
  top.ebConstrs = m.ebConstrs ++ rest.ebConstrs;
  top.ebJudgments = m.ebJudgments ++ rest.ebJudgments;
  top.ebDefaultRules =
      m.ebDefaultRules ++ rest.ebDefaultRules;
  top.ebStandInRules =
      m.ebStandInRules ++ rest.ebStandInRules;
  top.ebRulesByModule =
      m.ebRulesByModule ++ rest.ebRulesByModule;
  --fill in default rules for constructors not known with them before
  local instantiatedDefaultRules::[Def] =
     instantiateExtensibellaDefaultRules(newRuleCombinations,
        top.ebDefaultRules);

  --types known in this module but not defined here
  local importedTys::[TypeEnvItem] =
      removeAllBy(\ ty1::TypeEnvItem ty2::TypeEnvItem ->
                    ty1.name == ty2.name,
                  m.tyDecls, --remove new
                  head(top.moduleTyDecls).2); --from all known
  --split relations into imported and new
  local isJdgs::[JudgmentEnvItem] =
      map(\ e::TypeEnvItem ->
            extJudgmentEnvItem(e.name.ebIsQName,
               consTypeList(
                  nameType(e.name, location=bogusLoc()),
                  nilTypeList(location=bogusLoc()),
                  location=bogusLoc()), 0),
          nubBy(\ a::TypeEnvItem b::TypeEnvItem -> a.name == b.name,
                flatMap(snd, rest.moduleTyDecls)));
  local allJdgs::[JudgmentEnvItem] =
      head(top.moduleJudgmentDecls).2 ++ isJdgs;
  local jdgSplit::([JudgmentEnvItem], [JudgmentEnvItem]) =
      partition(\ j::JudgmentEnvItem ->
                  j.name.baselessName == m.modName,
                allJdgs);
  local newJdgs::[JudgmentEnvItem] = jdgSplit.1;
  local importedJdgs::[JudgmentEnvItem] = jdgSplit.2;

  --declarations for unknown constructors
  local unknownConstrs::[ConstrDecl] =
      map(\ t::TypeEnvItem ->
            constrDecl(t.name.ebUnknownNameI, [],
               extensibellaNameTy(t.name.ebTypeName)),
            importedTys) ++
      flatMap(\ j::JudgmentEnvItem ->
                if j.isExtensible &&
                   !sameModule(toQName(m.modName, bogusLoc()), j.name)
                then [constrDecl(j.ebUnknownNameK, [],
                         extensibellaNameTy(j.pcType.name.ebTypeName))]
                else [],
              jdgs ++ isJdgs);
  --env items for unknown constructors
  local constrEnvsI::[ConstructorEnvItem] =
      map(\ t::TypeEnvItem ->
            constructorEnvItem(
               baseName(t.name.ebUnknownNameI, location=bogusLoc()),
               nameType(t.name, location=bogusLoc()),
               nilTypeList(location=bogusLoc())),
          importedTys);
  local constrEnvsK::[ConstructorEnvItem] =
      flatMap(\ j::JudgmentEnvItem ->
                if j.isExtensible &&
                   !sameModule(toQName(m.modName, bogusLoc()), j.name)
                then [constructorEnvItem(
                         baseName(j.ebUnknownNameK,
                                  location=bogusLoc()),
                         j.pcType, nilTypeList(location=bogusLoc()))]
                else [],
              jdgs ++ isJdgs);
  --rules for default rules holding on unknown constructors
  local joinedNewJdgsI::[(JudgmentEnvItem, [ConstructorEnvItem])] =
      map(\ j::JudgmentEnvItem ->
            (j, filter(\ c::ConstructorEnvItem ->
                         --same type
                         j.pcType == c.type &&
                         --and judgment is introduced separately from pc
                         j.name.baselessName != j.pcType.name.baselessName,
                       constrEnvsI)),
          allJdgs);
  local rulesNewUnknownI::[Def] =
      instantiateExtensibellaDefaultRules(joinedNewJdgsI,
         top.ebDefaultRules);
  --rules for stand-in rules holding on unknown constructors
  local joinedOldJdgsK::[(JudgmentEnvItem, [ConstructorEnvItem])] =
      flatMap(\ j::JudgmentEnvItem ->
                if j.isExtensible
                then [(j, filter(\ c::ConstructorEnvItem ->
                                   c.name.base == j.ebUnknownNameK,
                                 constrEnvsK))]
                else [],
              importedJdgs);
  local joinedNewJdgsK::[(JudgmentEnvItem, [ConstructorEnvItem])] =
      flatMap(
          \ j::JudgmentEnvItem ->
            if j.isExtensible
            then [(j, filter(\ c::ConstructorEnvItem ->
                               j.pcType == c.type &&
                               c.name.base != j.ebUnknownNameK,
                           --instantiated default rules does not include
                           --   K for same rel
                             constrEnvsK))]
            else [],
          m.judgmentDecls);
  local rulesNewUnknownK::[Def] =
      --stand-in rules from imported judgments
      instantiateExtensibellaDefaultRules(joinedOldJdgsK,
         top.ebStandInRules) ++
      --default rules for new judgments
      instantiateExtensibellaDefaultRules(joinedNewJdgsK,
         m.ebDefaultRules);

  --extra defs for full file to pass along stand-in rule
  local standInDefs::[Definition] =
      map(\ p::(JudgmentEnvItem, Metaterm, [Metaterm], String) ->
            let name::String =
                "$stand-in_rule__" ++
                case p.2 of
                | relationMetaterm(rel, _) -> rel
                | _ -> error("Head must be relationMetaterm")
                end
            in
            let prems::[Metaterm] = init(p.3) --drop false prem
            in
            let defJdg::(String, [ExtensibellaType]) =
                (name, decorate p.1.types with {
                          tyEnv = m.tyEnv;
                       }.eb)
            in
            let headM::Metaterm =
                case p.2 of
                | relationMetaterm(rel, args) ->
                  relationMetaterm(name, args)
                | _ -> error("Head must be relationMetaterm")
                end
            in
            let headVars::[String] = headM.vars
            in
            let bodyVars::[String] =
              removeAll(headVars, nub(flatMap((.vars), prems)))
            in
              definition([defJdg],
                 oneDefs(
                    if null(prems)
                    then factDef(headM)
                    else ruleDef(headM,
                            if null(bodyVars)
                            then foldr1(andMetaterm, prems)
                            else existsMetaterm(bodyVars,
                                    foldr1(andMetaterm, prems)))))
            end end end end end end,
          top.ebStandInRules);

  --contents of different Extensibella files
  top.defFileContents =
      buildExtensibellaFile(top.ebKinds,
         top.ebConstrs ++ unknownConstrs,
         top.ebJudgments, top.ebRulesByModule,
         instantiatedDefaultRules,
         rulesNewUnknownI ++ rulesNewUnknownK);
  top.interfaceFileContents =
      buildExtensibellaInterfaceFile(m.modName,
         (stdLibName, [])::top.buildsOns);
  top.fullFileContents =
      buildExtensibellaFile(top.ebKinds,
         --no unknown constructors in the non-extensible definition
         top.ebConstrs,
         top.ebJudgments, top.ebRulesByModule,
         instantiatedDefaultRules,
         --no unknown rules in the non-extensible definition
         []) ++
      --add stand-in-rule-passing defs specially
      "\n\n%Stand-In Rules\n" ++
      implode("", map((.pp), standInDefs));

  top.ebErrors =
      case intersect(map((.name), m.constructorDecls),
                     map((.name), m.judgmentDecls)) of
      | [] -> []
      | l ->
        ["Module " ++ m.modName ++ " declares both a constructor " ++
         "and a judgment of the following names:  [" ++
         implode(", ", map((.pp), l)) ++ "];  Extensibella has a " ++
         "single namespace for both"]
      end ++ rest.ebErrors;
}





attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebDefaultRules, ebStandInRules
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  top.ebKinds = files.ebKinds;
  top.ebConstrs = files.ebConstrs;
  top.ebRulesByModule = [(name, files.ebRules ++ isRules)];
  top.ebJudgments = files.ebJudgments ++ isRels;
  top.ebDefaultRules = files.ebDefaultRules;

  --To keep instantiated stand-in rules from being used with the DefR
  --rule from the logic G, we add a premise of (0 = 0 -> false).  This
  --is, of course, not derivable unless false is derivable, so it is
  --safe to add, but hides the false well enough Abella doesn't
  --recognize it and clear the subgoal.  Extensibella gets rid of this
  --so it can't be used.
  top.ebStandInRules =
      map(\ p::(JudgmentEnvItem, Metaterm, [Metaterm], String) ->
            (p.1, p.2, p.3 ++ [^falsePrem], p.4),
          files.ebStandInRules ++ blankStandInRules);
  local zero::ExtensibellaTerm = extensibellaIntegerTerm(0);
  local falsePrem::Metaterm =
      impliesMetaterm(eqMetaterm(^zero, ^zero), falseMetaterm());

  --automatically generate is relations
  local isRels::[(String, [ExtensibellaType])] =
      map(\ t::TypeEnvItem ->
            (t.name.ebIsName,
             [extensibellaNameTy(t.name.ebTypeName)]),
          top.tyDecls);
  local isRules::[Def] =
      map(\ c::ConstructorEnvItem ->
            let children::[(String, Metaterm)] =
                foldr(\ t::Type rest::[(String, Metaterm)] ->
                        let newName::String =
                            freshNameFromType(t, map(fst, rest))
                        in
                          (newName,
                           relationMetaterm(t.ebIs,
                              [nameExtensibellaTerm(newName)]))::rest
                        end,
                      [], c.types.toList)
            in
              if null(children)
              then factDef(relationMetaterm(c.type.ebIs,
                    [nameExtensibellaTerm(c.name.ebConstructorName)]))
              else ruleDef(
                      relationMetaterm(c.type.ebIs,
                         [applicationExtensibellaTerm(
                             c.name.ebConstructorName,
                             map(nameExtensibellaTerm,
                                 map(fst, children)))]),
                      foldr1(andMetaterm, map(snd, children)))
            end,
          top.constructorDecls);

  {-
    When we declare Ext_Ind in Extensibella, it must be in the same
    module as the one declaring the relation.  However, since we don't
    instantiate the stand-in rule until we include an extension, we
    don't know if there will be a rule for unknownKTerm when Ext_Ind
    is declared.  To solve this, we include a blank stand-in rule for
    every relation without an explicit one given.  That way we can
    guarantee there will be a case, and a pretty generic one at that.
  -}
  local presentJdgs::[QName] = map((.name), map(fst, files.ebStandInRules));
  local missingJdgs::[JudgmentEnvItem] =
      filter(\ j::JudgmentEnvItem ->
               j.isExtensible && !contains(j.name, presentJdgs),
             top.judgmentDecls);
  local blankStandInRules::[(JudgmentEnvItem, Metaterm, [Metaterm],
                             String)] =
      map(\ j::JudgmentEnvItem ->
            let args::[String] =
                map(\ x::Integer -> "A" ++ toString(x),
                    range(1, j.types.len + 1))
            in
            let conc::Metaterm =
                relationMetaterm(j.eb, map(varExtensibellaTerm, args))
            in
              (j, conc, [], head(drop(j.pcIndex, args)))
            end end,
          missingJdgs);
}





attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebDefaultRules,
   ebStandInRules
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebDefaultRules = [];
  top.ebStandInRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.ebKinds = f.ebKinds ++ rest.ebKinds;
  top.ebConstrs = f.ebConstrs ++ rest.ebConstrs;
  top.ebRules = f.ebRules ++ rest.ebRules;
  top.ebJudgments = f.ebJudgments ++ rest.ebJudgments;
  top.ebDefaultRules =
      f.ebDefaultRules ++ rest.ebDefaultRules;
  top.ebStandInRules = f.ebStandInRules ++ rest.ebStandInRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.ebKinds = rest.ebKinds;
  top.ebConstrs = rest.ebConstrs;
  top.ebRules = rest.ebRules;
  top.ebJudgments = rest.ebJudgments;
  top.ebDefaultRules = rest.ebDefaultRules;
  top.ebStandInRules = rest.ebStandInRules;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.ebKinds = rest.ebKinds;
  top.ebConstrs = rest.ebConstrs;
  top.ebRules = rest.ebRules;
  top.ebJudgments = rest.ebJudgments;
  top.ebDefaultRules = rest.ebDefaultRules;
  top.ebStandInRules = rest.ebStandInRules;
}
