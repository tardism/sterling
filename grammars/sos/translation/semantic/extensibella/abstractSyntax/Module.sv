grammar sos:translation:semantic:extensibella:abstractSyntax;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;

attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebTranslationRules, ebStandInRules, ebErrors,
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
  top.ebTranslationRules = files.ebTranslationRules;
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
  top.ebTranslationRules =
      m.ebTranslationRules ++ rest.ebTranslationRules;
  top.ebStandInRules =
      m.ebStandInRules ++ rest.ebStandInRules;
  top.ebRulesByModule =
      m.ebRulesByModule ++ rest.ebRulesByModule;
  --fill in trans rules for constructors not known with them before
  local instantiatedTransRules::[Def] =
     instantiateExtensibellaTransRules(newRuleCombinations,
        top.ebTranslationRules);

  --types known in this module but not defined here
  local importedTys::[TypeEnvItem] =
      removeAllBy(\ ty1::TypeEnvItem ty2::TypeEnvItem ->
                    ty1.name == ty2.name,
                  m.tyDecls, --remove new
                  head(top.moduleTyDecls).2); --from all known
  --split relations into imported and new
  local jdgSplit::([JudgmentEnvItem], [JudgmentEnvItem]) =
      partition(\ j::JudgmentEnvItem ->
                  j.name.baselessName == m.modName,
                head(top.moduleJudgmentDecls).2);
  local newJdgs::[JudgmentEnvItem] = jdgSplit.1;
  local importedJdgs::[JudgmentEnvItem] = jdgSplit.2;

  --declarations for unknown constructors
  local unknownConstrs::[ConstrDecl] =
      flatMap(\ t::TypeEnvItem ->
                [constrDecl(t.name.ebUnknownNameI, [],
                    extensibellaNameTy(t.name.ebTypeName)),
                 constrDecl(t.name.ebUnknownNameK, [],
                    extensibellaNameTy(t.name.ebTypeName))],
              importedTys);
  --rules for translation rules holding on unknown constructors
  local constrEnvsI::[ConstructorEnvItem] =
      map(\ t::TypeEnvItem ->
            constructorEnvItem(
               baseName(t.name.ebUnknownNameI, location=bogusLoc()),
               nameType(t.name, location=bogusLoc()),
               nilTypeList(location=bogusLoc())),
          importedTys);
  local joinedNewJdgsI::[(JudgmentEnvItem, [ConstructorEnvItem])] =
      map(\ j::JudgmentEnvItem ->
            (j, filter(\ c::ConstructorEnvItem -> j.pcType == c.type,
                       constrEnvsI)),
          newJdgs);
  local rulesNewUnknownI::[Def] =
      instantiateExtensibellaTransRules(joinedNewJdgsI,
         top.ebTranslationRules);
  --rules for stand-in rules holding on unknown constructors
  local constrEnvsK::[ConstructorEnvItem] =
      map(\ t::TypeEnvItem ->
            constructorEnvItem(
               baseName(t.name.ebUnknownNameK, location=bogusLoc()),
               nameType(t.name, location=bogusLoc()),
               nilTypeList(location=bogusLoc())),
          importedTys);
  local joinedOldJdgsK::[(JudgmentEnvItem, [ConstructorEnvItem])] =
      map(\ j::JudgmentEnvItem ->
            (j, filter(\ c::ConstructorEnvItem -> j.pcType == c.type,
                       constrEnvsK)),
          importedJdgs);
  local rulesNewUnknownK::[Def] =
      instantiateExtensibellaTransRules(joinedOldJdgsK,
         top.ebStandInRules);

  --contents of different Extensibella files
  top.defFileContents =
      buildExtensibellaFile(top.ebKinds,
         top.ebConstrs ++ unknownConstrs,
         top.ebJudgments, top.ebRulesByModule,
         instantiatedTransRules,
         rulesNewUnknownI ++ rulesNewUnknownK);
  top.interfaceFileContents =
      buildExtensibellaInterfaceFile(m.modName,
         (stdLibName, [])::top.buildsOns);
  top.fullFileContents =
      buildExtensibellaFile(top.ebKinds,
         --no unknown constructors in the non-extensible definition
         top.ebConstrs,
         top.ebJudgments, top.ebRulesByModule,
         instantiatedTransRules,
         --no unknown rules in the non-extensible definition
         []);

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
   ebTranslationRules, ebStandInRules
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  top.ebKinds = files.ebKinds;
  top.ebConstrs = files.ebConstrs;
  top.ebRulesByModule = [(name, files.ebRules ++ isRules)];
  top.ebJudgments = files.ebJudgments ++ isRels;
  top.ebTranslationRules = files.ebTranslationRules;
  top.ebStandInRules = files.ebStandInRules;

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
}





attribute
   ebKinds, ebConstrs, ebRules, ebJudgments, ebTranslationRules,
   ebStandInRules
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRules = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];
  top.ebStandInRules = [];
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
  top.ebStandInRules = f.ebStandInRules ++ rest.ebStandInRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.ebKinds = rest.ebKinds;
  top.ebConstrs = rest.ebConstrs;
  top.ebRules = rest.ebRules;
  top.ebJudgments = rest.ebJudgments;
  top.ebTranslationRules = rest.ebTranslationRules;
  top.ebStandInRules = rest.ebStandInRules;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.ebKinds = rest.ebKinds;
  top.ebConstrs = rest.ebConstrs;
  top.ebRules = rest.ebRules;
  top.ebJudgments = rest.ebJudgments;
  top.ebTranslationRules = rest.ebTranslationRules;
  top.ebStandInRules = rest.ebStandInRules;
}
