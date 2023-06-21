grammar sos:core:modules;

imports sos:core:common:concreteSyntax;
imports sos:core:common:abstractSyntax;

imports sos:core:semanticDefs:concreteSyntax only File_c;
imports sos:core:semanticDefs:abstractSyntax;

imports sos:core:concreteDefs:concreteSyntax only ConcreteFile_c;
imports sos:core:concreteDefs:abstractSyntax;

imports sos:core:main:concreteSyntax only MainFile_c;
imports sos:core:main:abstractSyntax;

import silver:util:graph as graph;


--Sets of declarations known to modules
--e.g. if moduleTyDecls=[(mod:ule, tys)], module mod:ule knows types
--     in tys and no other types (other than built-ins)
synthesized attribute moduleTyDecls::[(String, [TypeEnvItem])];
synthesized attribute moduleConstructorDecls::[(String, [ConstructorEnvItem])];
synthesized attribute moduleJudgmentDecls::[(String, [JudgmentEnvItem])];
synthesized attribute moduleTranslationDecls::[(String, [TranslationEnvItem])];
synthesized attribute moduleRuleDecls::[(String, [RuleEnvItem])];
synthesized attribute moduleConcreteDecls::[(String, [ConcreteEnvItem])];
synthesized attribute moduleFunctionDecls::[(String, [FunctionEnvItem])];

--Sets of modules known to modules
synthesized attribute buildsOns::[(String, [String])];

synthesized attribute nameList::[String];
synthesized attribute modName::String;

synthesized attribute errorString::String;


--to make it easier to change the name, should we need to do so
global stdLibName::String = "stdLib";


{-
  We assume all modules a module builds on/imports occur *later* in
  the list.  For example, suppose we have
       stlc:host
       stlc:let, stlc:pair build on stlc:host
       stlc:letPair builds on stlc:let, stlc:pair
  Our list could be either
       [stlc:letPair, stlc:let, stlc:pair, stlc:host]
  or
       [stlc:letPair, stlc:pair, stlc:let, stlc:host]
  Either one is valid because the modules a module build on come later
  in the list.  Since stlc:pair and stlc:let don't have any
  relationship to one another, we don't care about the relative
  ordering of those modules.  We cannot have cycles in a well-defined
  set of modules, so this can always be the case.

  In addition to the declared set of builds-on declarations, there is
  the standard library.  Everything builds on this implicitly, and it
  therefore comes at the end of the list.
-}
nonterminal ModuleList with
   nameList,
   moduleTyDecls, moduleConstructorDecls, moduleJudgmentDecls,
   moduleTranslationDecls, moduleRuleDecls, moduleConcreteDecls,
   moduleFunctionDecls,
   buildsOns,
   errorString;

abstract production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.nameList = [stdLibName];

  files.moduleName = toQName(stdLibName, loc);
  files.tyEnv = buildEnv(files.tyDecls);
  files.constructorEnv = buildEnv(files.constructorDecls);
  files.judgmentEnv = buildEnv(files.judgmentDecls);
  files.translationEnv = buildEnv(files.translationDecls);
  files.ruleEnv = buildEnv(files.ruleDecls);
  files.concreteEnv = buildEnv(files.concreteDecls);
  files.funEnv = buildEnv(files.funDecls);
  files.transRuleConstructors_down = files.transRuleConstructors;

  top.moduleTyDecls = [(stdLibName, files.tyDecls)];
  top.moduleConstructorDecls =
      [(stdLibName, files.constructorDecls)];
  top.moduleJudgmentDecls =
      [(stdLibName, files.judgmentDecls)];
  top.moduleTranslationDecls =
      [(stdLibName, files.translationDecls)];
  top.moduleRuleDecls = [(stdLibName, files.ruleDecls)];
  top.moduleConcreteDecls = [(stdLibName, files.concreteDecls)];
  top.moduleFunctionDecls =
      [(stdLibName, hardFunDefs++ files.funDecls)];
  local loc::Location = txtLoc(stdLibName);
  --some functions we can't write in the code:
  production hardFunDefs::[FunctionEnvItem] =
      [functionEnvItem(toQName(stdLibName ++ ":tail", loc),
          toTypeList(
             [listType(varType("A", location=loc), location=loc)],
             loc),
          listType(varType("A", location=loc), location=loc)),
       functionEnvItem(toQName(stdLibName ++ ":head", loc),
          toTypeList(
             [listType(varType("A", location=loc), location=loc)],
             loc),
          varType("A", location=loc)),
       functionEnvItem(toQName(stdLibName ++ ":null", loc),
          toTypeList(
             [listType(varType("A", location=loc), location=loc)],
             loc),
          boolType(location=loc))];

  top.buildsOns = [];

  --standard library should be error-free, so die if it isn't
  top.errorString =
      if files.errorString == ""
      then ""
      else error("Errors in standard library:\n" ++
                 files.errorString);
}


abstract production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.nameList = m.modName::rest.nameList;

  --Everything includes the standard library
  local fullBuildsOnDecls::[QName] = m.buildsOnDecls;
      --toQName(stdLibName, bogusLoc())::m.buildsOnDecls;
  --Reduce imported items in case something is imported by two
  --imported modules:  m imports A, B; A imports C; B imports C.
  --In that case, m would see everything from C twice.
  --We want to keep any multiple copies of m's defs, though, so we can
  --detect multiple declarations of the same name.
  production tys::[TypeEnvItem] =
        nubBy(\ t1::TypeEnvItem t2::TypeEnvItem -> t1.name == t2.name,
           lookupAllModules(fullBuildsOnDecls, rest.moduleTyDecls)) ++
           m.tyDecls;
  production cons::[ConstructorEnvItem] =
        nubBy(\ c1::ConstructorEnvItem c2::ConstructorEnvItem ->
                c1.name == c2.name,
           lookupAllModules(fullBuildsOnDecls,
              rest.moduleConstructorDecls)) ++ m.constructorDecls;
  production jdgs::[JudgmentEnvItem] =
        nubBy(\ j1::JudgmentEnvItem j2::JudgmentEnvItem ->
                j1.name == j2.name,
           lookupAllModules(fullBuildsOnDecls,
              rest.moduleJudgmentDecls)) ++ m.judgmentDecls;
  production trns::[TranslationEnvItem] =
        nubBy(\ t1::TranslationEnvItem t2::TranslationEnvItem ->
                t1.name == t2.name,
           lookupAllModules(fullBuildsOnDecls,
              rest.moduleTranslationDecls)) ++ m.translationDecls;
  production rules::[RuleEnvItem] =
        nubBy(\ r1::RuleEnvItem r2::RuleEnvItem -> r1.name == r2.name,
           lookupAllModules(fullBuildsOnDecls,
              rest.moduleRuleDecls)) ++ m.ruleDecls;
  production concretes::[ConcreteEnvItem] =
        nubBy(\ c1::ConcreteEnvItem c2::ConcreteEnvItem ->
                c1.name == c2.name,
           lookupAllModules(fullBuildsOnDecls,
              rest.moduleConcreteDecls)) ++ m.concreteDecls;
  production functions::[FunctionEnvItem] =
        nubBy(\ f1::FunctionEnvItem f2::FunctionEnvItem ->
                f1.name == f2.name,
           lookupAllModules(fullBuildsOnDecls,
              rest.moduleFunctionDecls)) ++ m.funDecls;
  top.moduleTyDecls = (m.modName, tys)::rest.moduleTyDecls;
  top.moduleConstructorDecls =
      (m.modName, cons)::rest.moduleConstructorDecls;
  top.moduleJudgmentDecls =
      (m.modName, jdgs)::rest.moduleJudgmentDecls;
  top.moduleTranslationDecls =
      (m.modName, trns)::rest.moduleTranslationDecls;
  top.moduleRuleDecls =
      (m.modName, rules)::rest.moduleRuleDecls;
  top.moduleConcreteDecls =
      (m.modName, concretes)::rest.moduleConcreteDecls;
  top.moduleFunctionDecls =
      (m.modName, functions)::rest.moduleFunctionDecls;

  top.buildsOns =
      (m.modName, map((.pp), m.buildsOnDecls))::rest.buildsOns;

  --Pairs of judgments and the constructors that don't know about them
  --Useful for translations to fill in translation rules
  production newRuleCombinations::[(JudgmentEnvItem, [ConstructorEnvItem])] =
     getPairsOfUnknown(top.moduleJudgmentDecls, top.moduleConstructorDecls,
                       top.buildsOns, m.modName);

  m.tyEnv = buildEnv(tys);
  m.constructorEnv = buildEnv(cons);
  m.judgmentEnv = buildEnv(jdgs);
  m.translationEnv = buildEnv(trns);
  m.ruleEnv = buildEnv(rules);
  m.concreteEnv = buildEnv(concretes);
  m.funEnv = buildEnv(functions);
  m.transRuleConstructors_down = m.transRuleConstructors;

  top.errorString =
      if rest.errorString == ""
      then m.errorString
      else if m.errorString == ""
           then rest.errorString
           else rest.errorString ++ "\n\n" ++ m.errorString;
}

function lookupAllModules
[a] ::= modules::[QName] decls::[(String, [a])]
{
  return
     foldr(\ s::String rest::[a] ->
     --it has to exist, if this is called from a well-built ModuleList
             lookup(s, decls).fromJust ++ rest,
           [], map((.pp), modules));
}

--Get the pairs of judgments and constructors not previously known in
--the same module
function getPairsOfUnknown
[(JudgmentEnvItem, [ConstructorEnvItem])] ::=
     jdgs::[(String, [JudgmentEnvItem])]
     cons::[(String, [ConstructorEnvItem])]
     buildsOns::[(String, [String])]
     currentMod::String
{
  --get all pairs of modules
  local allPairs::[(String, String)] = produceAllPairs(buildsOns);
  --build a graph to get the transitive closure
  local g::graph:Graph<String> =
     foldr(\ p::(String, [String]) rest::graph:Graph<String> ->
             graph:add(flatMap(\ x::String -> [(x, p.1)],
                               p.2), rest),
           graph:empty(), buildsOns);
  local closurePairs::[(String, String)] =
     graph:toList(graph:transitiveClosure(g));
  --unknown pairs are  allPairs \ closurePairs
  local unknownPairs::[(String, String)] =
     removeAllBy(\ p1::(String, String) p2::(String, String) ->
                   (p1.1 == p2.1 && p1.2 == p2.2) || --regular
                   (p1.2 == p2.1 && p1.1 == p2.2),   --flipped
                 closurePairs, allPairs);

  --group by first module
  local sorted::[(String, String)] = sort(unknownPairs);
  local grouped::[[(String, String)]] = group(sorted);
  --replace first module with judgments from that module
  local pairedOut::[(String, [String])] =
     map(\ l::[(String, String)] -> (head(l).1, map(snd, l)), grouped);
  local jdgGroups::[([JudgmentEnvItem], [String])] =
     map(\ p::(String, [String]) ->
           (filter(\ j::JudgmentEnvItem ->
                     j.name.baselessName == p.1,
                   lookup(p.1, jdgs).fromJust), p.2),
         pairedOut);
  --replace second module with constructors from that module
  local jdgConPairs::[([JudgmentEnvItem], [ConstructorEnvItem])] =
     map(\ p::([JudgmentEnvItem], [String]) ->
           (p.1,
            flatMap(\ x::String ->
                      filter(\ c::ConstructorEnvItem ->
                               c.name.baselessName == x,
                             lookup(x, cons).fromJust),
                    p.2)),
         jdgGroups);

  --get the pairs of judgments and constructors to which they apply
  local splitOut::[(JudgmentEnvItem, [ConstructorEnvItem])] =
     flatMap(\ p::([JudgmentEnvItem], [ConstructorEnvItem]) ->
               map(pair(_, p.2), p.1),
             jdgConPairs);
  local extOnly::[(JudgmentEnvItem, [ConstructorEnvItem])] =
     filter(\ p::(JudgmentEnvItem, [ConstructorEnvItem]) ->
              p.1.isExtensible,
            splitOut);
  local filteredCons::[(JudgmentEnvItem, [ConstructorEnvItem])] =
     map(\ p::(JudgmentEnvItem, [ConstructorEnvItem]) ->
           (p.1,
            filter(\ c::ConstructorEnvItem ->
                     p.1.pcType == c.type, p.2)),
         extOnly);

  return filteredCons;
}

function produceAllPairs
[(String, String)] ::= l::[(String, [String])]
{
  return
     case l of
     | [] -> []
     | [_] -> []
     | (h, _)::tl ->
       flatMap(\ p::(String, [String]) -> [(h, p.1), (p.1, h)],
               tl) ++ produceAllPairs(tl)
     end;
}





nonterminal Module with
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   ruleDecls, buildsOnDecls, concreteDecls, funDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv, ruleEnv,
   concreteEnv, funEnv,
   transRuleConstructors, transRuleConstructors_down,
   modName,
   errorString;

abstract production module
top::Module ::= name::String files::Files
{
  top.modName = name;

  files.moduleName = toQName(name, bogusLoc());

  top.tyDecls = files.tyDecls;
  top.constructorDecls = files.constructorDecls;
  top.judgmentDecls = files.judgmentDecls;
  top.translationDecls = files.translationDecls;
  top.ruleDecls = files.ruleDecls;
  top.buildsOnDecls = toQName(stdLibName, bogusLoc())::files.buildsOnDecls;
  top.concreteDecls = files.concreteDecls;
  top.funDecls = files.funDecls;
  top.transRuleConstructors = files.transRuleConstructors;

  files.tyEnv = top.tyEnv;
  files.constructorEnv = top.constructorEnv;
  files.judgmentEnv = top.judgmentEnv;
  files.translationEnv = top.translationEnv;
  files.ruleEnv = top.ruleEnv;
  files.concreteEnv = top.concreteEnv;
  files.funEnv = top.funEnv;
  files.transRuleConstructors_down = top.transRuleConstructors_down;

  top.errorString =
      if files.errorString == ""
      then ""
      else "Errors for " ++ name ++ "\n" ++ files.errorString;
}


instance Eq Module {
  eq = \ x::Module y::Module -> x.modName == y.modName;
}

instance Ord Module {
  compare = \ x::Module y::Module -> compare(x.modName, y.modName);
}





nonterminal Files with
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   ruleDecls, buildsOnDecls, concreteDecls, funDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv, ruleEnv,
   concreteEnv, funEnv,
   transRuleConstructors, transRuleConstructors_down,
   errorString;
propagate tyEnv, constructorEnv, judgmentEnv, translationEnv, ruleEnv,
          concreteEnv, funEnv on Files;

abstract production nilFiles
top::Files ::=
{
  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];
  top.ruleDecls = [];
  top.buildsOnDecls = [];
  top.concreteDecls = [];
  top.funDecls = [];
  top.transRuleConstructors = [];

  top.errorString = "";
}


abstract production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  f.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  top.tyDecls = f.tyDecls ++ rest.tyDecls;
  top.constructorDecls = f.constructorDecls ++ rest.constructorDecls;
  top.judgmentDecls = f.judgmentDecls ++ rest.judgmentDecls;
  top.translationDecls = f.translationDecls ++ rest.translationDecls;
  top.ruleDecls = f.ruleDecls ++ rest.ruleDecls;
  top.buildsOnDecls = f.buildsOnDecls ++ rest.buildsOnDecls;
  top.concreteDecls = rest.concreteDecls;
  top.funDecls = rest.funDecls;
  top.transRuleConstructors =
      f.transRuleConstructors ++ rest.transRuleConstructors;

  f.transRuleConstructors_down = top.transRuleConstructors_down;
  rest.transRuleConstructors_down = top.transRuleConstructors_down;

  top.errorString =
      if null(f.errors)
      then rest.errorString
      else "  [" ++ filename ++ "]\n" ++
           implode("\n", map((.pp), f.errors)) ++
           if rest.errorString == ""
           then ""
           else "\n" ++ rest.errorString;
}


abstract production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  f.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  top.tyDecls = rest.tyDecls;
  top.constructorDecls = rest.constructorDecls;
  top.judgmentDecls = rest.judgmentDecls;
  top.translationDecls = rest.translationDecls;
  top.ruleDecls = rest.ruleDecls;
  top.buildsOnDecls = rest.buildsOnDecls;
  top.concreteDecls = f.concreteDecls ++ rest.concreteDecls;
  top.funDecls = rest.funDecls;
  top.transRuleConstructors = rest.transRuleConstructors;

  rest.transRuleConstructors_down = top.transRuleConstructors_down;

  top.errorString =
      if null(f.errors)
      then rest.errorString
      else "  [" ++ filename ++ "]\n" ++
           implode("\n", map((.pp), f.errors)) ++
           if rest.errorString == ""
           then ""
           else "\n" ++ rest.errorString;
}


abstract production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  f.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  top.tyDecls = rest.tyDecls;
  top.constructorDecls = rest.constructorDecls;
  top.judgmentDecls = rest.judgmentDecls;
  top.translationDecls = rest.translationDecls;
  top.ruleDecls = rest.ruleDecls;
  top.buildsOnDecls = rest.buildsOnDecls;
  top.concreteDecls = rest.concreteDecls;
  top.funDecls = f.funDecls ++ rest.funDecls;
  top.transRuleConstructors = rest.transRuleConstructors;

  rest.transRuleConstructors_down = top.transRuleConstructors_down;

  top.errorString =
      if null(f.errors)
      then rest.errorString
      else "  [" ++ filename ++ "]\n" ++
           implode("\n", map((.pp), f.errors)) ++
           if rest.errorString == ""
           then ""
           else "\n" ++ rest.errorString;
}
