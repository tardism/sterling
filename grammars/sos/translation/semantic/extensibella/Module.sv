grammar sos:translation:semantic:extensibella;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;

attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebTranslationRules, ebErrors,
   defFileContents, interfaceFileContents
occurs on ModuleList;

synthesized attribute defFileContents::String;
synthesized attribute interfaceFileContents::String;

aspect production nilModuleList
top::ModuleList ::=
{
  top.ebKinds = [];
  top.ebConstrs = [];
  top.ebRulesByModule = [];
  top.ebJudgments = [];
  top.ebTranslationRules = [];

  top.defFileContents =
      error("Should not access on empty module list");
  top.interfaceFileContents =
      error("Should not access on empty module list");

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
  top.ebRulesByModule =
      case m.ebRulesByModule of
      | [(mod, rules)] ->
        [(mod, rules ++ instantiatedTransRules)]
      | _ -> error("Not possible")
      end ++ rest.ebRulesByModule;
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

  --declaration for unknown constructors
  local unknownConstrs::[ConstrDecl] =
      map(\ t::TypeEnvItem ->
            constrDecl(t.name.ebUnknownName, [],
               extensibellaNameTy(t.name.ebTypeName)),
          importedTys);
  --rules for imported relations so they hold on unknown constructors
  local rulesImportedUnknown::[Def] =
      buildImportedUnknownRules(importedJdgs, importedTys,
                                m.judgmentEnv);
  --rules for translation rules holding on unknown constructors
  local constrEnvs::[ConstructorEnvItem] =
      map(\ t::TypeEnvItem ->
            constructorEnvItem(
               baseName(t.name.ebUnknownName, location=bogusLoc()),
               nameType(t.name, location=bogusLoc()),
               nilTypeList(location=bogusLoc())),
          importedTys);
  local joinedNewJdgs::[(JudgmentEnvItem, [ConstructorEnvItem])] =
      map(\ j::JudgmentEnvItem ->
            (j, filter(\ c::ConstructorEnvItem -> j.pcType == c.type,
                       constrEnvs)),
          newJdgs);
  local rulesNewUnknown::[Def] =
      instantiateExtensibellaTransRules( joinedNewJdgs,
         top.ebTranslationRules);

  top.defFileContents =
      buildExtensibellaFile(top.ebKinds,
         top.ebConstrs ++ unknownConstrs,
         top.ebJudgments, top.ebRulesByModule,
         rulesImportedUnknown ++ rulesNewUnknown);
  top.interfaceFileContents =
      buildExtensibellaInterfaceFile(m.modName, top.buildsOns);

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
   ebTranslationRules
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  top.ebKinds = files.ebKinds;
  top.ebConstrs = files.ebConstrs;
  top.ebRulesByModule = [(name, files.ebRules ++ isRules)];
  top.ebJudgments = files.ebJudgments ++ isRels;
  top.ebTranslationRules = files.ebTranslationRules;

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


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.ebKinds = rest.ebKinds;
  top.ebConstrs = rest.ebConstrs;
  top.ebRules = rest.ebRules;
  top.ebJudgments = rest.ebJudgments;
  top.ebTranslationRules = rest.ebTranslationRules;
}
