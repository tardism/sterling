grammar sos:core:main:abstractSyntax;

imports sos:core:common:abstractSyntax;
imports sos:core:concreteDefs:abstractSyntax;
imports sos:core:semanticDefs:abstractSyntax;


nonterminal MainFile with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   funEnv, funDecls,
   moduleName,
   errors,
   location;
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv, funEnv on MainFile;

abstract production mainFile
top::MainFile ::= moduleName::QName contents::MainDecls
{
  top.pp = moduleName.pp ++ "\n" ++ contents.pp;

  contents.moduleName = top.moduleName;

  top.funDecls = contents.funDecls;

  top.errors <-
      if moduleName.pp == top.moduleName.pp
      then []
      else [errorMessage("Module declaration is incorrect:  " ++
                         moduleName.pp, location=top.location)];
}





nonterminal MainDecls with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   funEnv, funDecls,
   moduleName,
   errors,
   location;
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv, funEnv, moduleName on MainDecls;

abstract production emptyMainDecls
top::MainDecls ::=
{
  top.pp = "";

  top.funDecls = [];
}


abstract production branchMainDecls
top::MainDecls ::= d1::MainDecls d2::MainDecls
{
  top.pp = d1.pp ++ "\n" ++ d2.pp;

  top.funDecls = d1.funDecls ++ d2.funDecls;
}


abstract production funMainDecl
top::MainDecls ::= f::FunDecl
{
  top.pp = f.pp;

  top.funDecls = f.funDecls;
}





nonterminal FunDecl with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   funEnv, funDecls,
   moduleName,
   errors,
   location;
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv, funEnv, moduleName on FunDecl;

abstract production funDecl
top::FunDecl ::= name::String params::Params retTy::Type body::Stmt
{
  top.pp = "Function " ++ name ++ " : " ++ params.pp ++ " -> " ++
           retTy.pp ++ " {\n" ++ body.pp ++ "}";

  body.downVarTypes = params.upVarTypes;

  production fullName::QName = addQNameBase(top.moduleName, name);
  top.funDecls = [functionEnvItem(fullName, params.types, retTy)];

  --Check there is only one declaration of this function
  local possibleFunctions::[FunctionEnvItem] =
      lookupEnv(fullName, top.funEnv);
  top.errors <-
      case possibleFunctions of
      | [] -> error("Impossible:  Function " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | _ ->
        [errorMessage("Found multiple declarations for function " ++
                      fullName.pp, location=top.location)]
      end;
}





nonterminal Params with
   pp,
   tyEnv,
   upVarTypes, types,
   errors,
   location;
propagate errors, tyEnv on Params;

abstract production branchParams
top::Params ::= p1::Params p2::Params
{
  top.pp = p1.pp ++ " " ++ p2.pp;

  top.upVarTypes = p1.upVarTypes ++ p2.upVarTypes;
  top.types = foldr(consTypeList(_, _, location=top.location),
                    p2.types, p1.types.toList);

  top.errors <-
      map(\ s::String ->
            errorMessage("Duplicate parameters named " ++ s,
                         location=top.location),
          intersect(map(fst, p1.upVarTypes),
                    map(fst, p2.upVarTypes)));
}


abstract production emptyParams
top::Params ::=
{
  top.pp = "";

  top.upVarTypes = [];
  top.types = nilTypeList(location=top.location);
}


abstract production oneParams
top::Params ::= name::String ty::Type
{
  top.pp = "<" ++ name ++ " : " ++ ty.pp ++ ">";

  top.upVarTypes = [(name, ty)];
  top.types = consTypeList(ty, nilTypeList(location=top.location),
                           location=top.location);
}
