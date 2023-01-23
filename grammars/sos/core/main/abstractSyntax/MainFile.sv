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
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv on MainFile;

abstract production mainFile
top::MainFile ::= moduleName::QName contents::MainDecls
{
  top.pp = moduleName.pp ++ "\n" ++ contents.pp;

  contents.moduleName = top.moduleName;

  top.errors <-
      if moduleName.pp == top.moduleName.pp
      then []
      else [errorMessage("Module declaration is incorrect:  " ++
                         moduleName.pp, location=top.location)];
}





nonterminal MainDecls with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   errors,
   location;
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv on MainDecls;

abstract production emptyMainDecls
top::MainDecls ::=
{
  top.pp = "";
}


abstract production branchMainDecls
top::MainDecls ::= d1::MainDecls d2::MainDecls
{
  top.pp = d1.pp ++ "\n" ++ d2.pp;
}


abstract production funMainDecl
top::MainDecls ::= f::FunDecl
{
  top.pp = f.pp;
}





nonterminal FunDecl with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   errors,
   location;
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv on FunDecl;

abstract production funDecl
top::FunDecl ::= name::String params::Params retTy::Type body::Stmt
{
  top.pp = "Function " ++ name ++ " : " ++ params.pp ++ " -> " ++
           retTy.pp ++ " {\n" ++ body.pp ++ "}";
}





nonterminal Params with
   pp,
   tyEnv,
   errors,
   location;
propagate errors, tyEnv on Params;

abstract production branchParams
top::Params ::= p1::Params p2::Params
{
  top.pp = p1.pp ++ " " ++ p2.pp;
}


abstract production emptyParams
top::Params ::=
{
  top.pp = "";
}


abstract production onParams
top::Params ::= name::String ty::Type
{
  top.pp = "<" ++ name ++ " : " ++ ty.pp ++ ">";
}
