grammar sos:core:modules;


imports sos:core:files:concreteSyntax;
imports sos:core:files:abstractSyntax;


--Sets of declarations known to modules
--e.g. if moduleTyDecls=[(mod:ule, tys)], module mod:ule knows types
--     in tys and no other types (other than built-ins)
synthesized attribute moduleTyDecls::[(String, [TypeEnvItem])];
synthesized attribute moduleConstructorDecls::[(String, [ConstructorEnvItem])];
synthesized attribute moduleJudgmentDecls::[(String, [JudgmentEnvItem])];
synthesized attribute moduleTranslationDecls::[(String, [TranslationEnvItem])];

synthesized attribute nameList::[String];
synthesized attribute name::String;

synthesized attribute errorString::String;


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
-}
nonterminal ModuleList with
   nameList,
   moduleTyDecls, moduleConstructorDecls, moduleJudgmentDecls,
   moduleTranslationDecls,
   errorString;

abstract production nilModuleList
top::ModuleList ::=
{
  top.nameList = [];

  top.moduleTyDecls = [];
  top.moduleConstructorDecls = [];
  top.moduleJudgmentDecls = [];
  top.moduleTranslationDecls = [];

  top.errorString = "";
}


abstract production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.nameList = m.name::rest.nameList;

  local tys::[TypeEnvItem] =
        lookupAllModules(m.buildsOnDecls, rest.moduleTyDecls);
  local cons::[ConstructorEnvItem] =
        lookupAllModules(m.buildsOnDecls, rest.moduleConstructorDecls);
  local jdgs::[JudgmentEnvItem] =
        lookupAllModules(m.buildsOnDecls, rest.moduleJudgmentDecls);
  local trns::[TranslationEnvItem] =
        lookupAllModules(m.buildsOnDecls, rest.moduleTranslationDecls);
  top.moduleTyDecls = (m.name, tys)::rest.moduleTyDecls;
  top.moduleConstructorDecls =
      (m.name, cons)::rest.moduleConstructorDecls;
  top.moduleJudgmentDecls = (m.name, jdgs)::rest.moduleJudgmentDecls;
  top.moduleTranslationDecls =
      (m.name, trns)::rest.moduleTranslationDecls;

  m.tyEnv = buildTyEnv(tys);
  m.constructorEnv = buildConstructorEnv(cons);
  m.judgmentEnv = buildJudgmentEnv(jdgs);
  m.translationEnv = buildTranslationEnv(trns);

  top.errorString =
      if rest.errorString == ""
      then m.errorString
      else if m.errorString == ""
           then rest.errorString
           else m.errorString ++ "\n\n" ++ rest.errorString;
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





nonterminal Module with
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   buildsOnDecls,
   name,
   errorString;

abstract production module
top::Module ::= name::String files::Files
{
  top.name = name;

  files.moduleName = toQName(name, bogusLoc());

  top.tyDecls = files.tyDecls;
  top.constructorDecls = files.constructorDecls;
  top.judgmentDecls = files.judgmentDecls;
  top.translationDecls = files.translationDecls;
  top.buildsOnDecls = files.buildsOnDecls;

  files.tyEnv = top.tyEnv;
  files.constructorEnv = top.constructorEnv;
  files.judgmentEnv = top.judgmentEnv;
  files.translationEnv = top.translationEnv;

  top.errorString =
      if files.errorString == ""
      then ""
      else "Errors for " ++ name ++ "\n" ++ files.errorString;
}


instance Eq Module {
  eq = \ x::Module y::Module -> x.name == y.name;
}

instance Ord Module {
  compare = \ x::Module y::Module -> compare(x.name, y.name);
}





nonterminal Files with
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   buildsOnDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   errorString;

abstract production nilFiles
top::Files ::=
{
  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls = [];
  top.buildsOnDecls = [];

  top.errorString = "";
}


abstract production consFiles
top::Files ::= filename::String f::File rest::Files
{
  f.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  top.tyDecls = f.tyDecls ++ rest.tyDecls;
  top.constructorDecls = f.constructorDecls ++ rest.constructorDecls;
  top.judgmentDecls = f.judgmentDecls ++ rest.judgmentDecls;
  top.translationDecls = f.translationDecls ++ rest.translationDecls;
  top.buildsOnDecls = f.buildsOnDecls ++ rest.buildsOnDecls;

  f.tyEnv = top.tyEnv;
  f.constructorEnv = top.constructorEnv;
  f.judgmentEnv = top.judgmentEnv;
  f.translationEnv = top.translationEnv;
  rest.tyEnv = top.tyEnv;
  rest.constructorEnv = top.constructorEnv;
  rest.judgmentEnv = top.judgmentEnv;
  rest.translationEnv = top.translationEnv;

  top.errorString =
      if null(f.errors)
      then rest.errorString
      else "  [" ++ filename ++ "]\n" ++
           implode("\n", map((.pp), f.errors)) ++
           if rest.errorString == ""
           then ""
           else "\n" ++ rest.errorString;
}

