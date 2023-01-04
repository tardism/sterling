grammar sos:translation:semantic:extensibella;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;

attribute
   ebKinds, ebConstrs, ebRulesByModule, ebJudgments,
   ebTranslationRules,
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
