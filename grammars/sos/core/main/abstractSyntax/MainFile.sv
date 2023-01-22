grammar sos:core:main:abstractSyntax;

imports sos:core:common:abstractSyntax;
imports sos:core:concreteDefs:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;


nonterminal MainFile with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   errors,
   location;
propagate errors on MainFile;

abstract production mainFile
top::MainFile ::= moduleName::QName contents::MainDecls
{
  top.pp = module.pp ++ "\n" ++ contents.pp;

  contents.moduleName = top.moduleName;

  top.errors <-
      if moduleName.pp == top.moduleName.pp
      then []
      else [errorMessage("Module declaration is incorrect:  " ++
                         moduleName.pp, location=top.location)];

  contents.judgmentEnv = top.judgmentEnv;
  contents.translationEnv = top.translationEnv;
  contents.concreteEnv = top.concreteEnv;
  contents.tyEnv = top.tyEnv;
  contents.constructorEnv = top.constructorEnv;

  --initially no vars known
  contents.downVarTypes = [];
}





nonterminal MainDecls with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   downVarTypes, upVarTypes,
   moduleName,
   errors,
   location;
propagate errors on MainDecls;

abstract production emptyMainDecls
top::MainDecls ::=
{
  top.pp = "";

  top.upVarTypes = top.downVarTypes;
}


abstract production branchMainDecls
top::MainDecls ::= d1::MainDecls d2::MainDecls
{
  top.pp = d1.pp ++ d2.pp;

  d1.judgmentEnv = top.judgmentEnv;
  d1.translationEnv = top.translationEnv;
  d1.concreteEnv = top.concreteEnv;
  d1.tyEnv = top.tyEnv;
  d1.constructorEnv = top.constructorEnv;
  d2.judgmentEnv = top.judgmentEnv;
  d2.translationEnv = top.translationEnv;
  d2.concreteEnv = top.concreteEnv;
  d2.tyEnv = top.tyEnv;
  d2.constructorEnv = top.constructorEnv;

  d1.downVarTypes = top.downVarTypes;
  d2.downVarTypes = d1.upVarTypes;
  top.upVarTypes = d2.upVarTypes;
}


abstract production parseMainDecl
top::MainDecls ::= p::ParseDecl
{
  top.pp = p.pp;

  p.judgmentEnv = top.judgmentEnv;
  p.translationEnv = top.translationEnv;
  p.concreteEnv = top.concreteEnv;
  p.tyEnv = top.tyEnv;
  p.constructorEnv = top.constructorEnv;
}


abstract production deriveMainDecl
top::MainDecls ::= d::DeriveRelation
{
  top.pp = d.pp;

  d.judgmentEnv = top.judgmentEnv;
  d.translationEnv = top.translationEnv;
  d.concreteEnv = top.concreteEnv;
  d.tyEnv = top.tyEnv;
  d.constructorEnv = top.constructorEnv;
}





nonterminal Parse with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   type,
   errors,
   location;
propagate errors on Parse;

--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
abstract production parse
top::ParseDecl ::= nt::QName varName::String parseString::MainExpr
{
  top.pp = "Parse " ++ nt.pp ++ " as " ++ varName ++ " from " ++
           parseString ++ "\n";

  nt.concreteEnv = top.concreteEnv;
  top.errors <-
      if !nt.concreteFound
      then [errorMessage("Unknown concrete nonterminal " ++ nt.pp,
            location=nt.location)]
      else if !nt.isConcreteNt
      then [errorMessage(nt.pp ++ " is not a concrete nonterminal " ++
               "but must be one to be parsed", location=nt.location)]
      else [];
  top.type = nt.concreteType;
}





nonterminal DeriveRelation with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   errors,
   location;
propagate errors on DeriveRelation;

abstract production deriveRelation
top::DeriveRelation ::= resultVar::String j::Judgment
{
  top.pp = resultVar ++ " := " ++ j.pp;
}
