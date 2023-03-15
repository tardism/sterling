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
top::FunDecl ::= name::String params::Params retTy::Type body::Expr
{
  top.pp = "Function " ++ name ++ " : " ++ params.pp ++ " -> " ++
           retTy.pp ++ " {\n" ++ body.pp ++ "}";

  body.downVarTypes = params.upVarTypes;
  body.downSubst = emptySubst();
  body.finalSubst = body.upSubst;

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
  --Check the return type
  top.errors <-
      if performSubstitutionType(body.type, body.upSubst) == retTy
      then []
      else [errorMessage("Expected function body to have type " ++
               retTy.pp ++ " but found " ++ body.type.pp,
               location=top.location)];
  --Check for a correct main type
  top.errors <-
      if name == "main"
      then case params.toList of
           | [(_, listType(stringType()))] -> []
           | _ -> [errorMessage("Main function must take one " ++
                                "argument of type [string]",
                                location=top.location)]
           end ++
           if retTy == intType(location=top.location)
           then []
           else [errorMessage("Main function must return int",
                              location=top.location)]
      else [];
  --typing errors
  top.errors <- errorsFromSubst(body.upSubst);
}





nonterminal Params with
   pp,
   toList<(String, Type)>, len,
   tyEnv,
   upVarTypes, types,
   errors,
   location;
propagate errors, tyEnv on Params;

abstract production branchParams
top::Params ::= p1::Params p2::Params
{
  top.pp = p1.pp ++ " " ++ p2.pp;

  top.toList = p1.toList ++ p2.toList;
  top.len = p1.len + p2.len;

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

  top.toList = [];
  top.len = 0;

  top.upVarTypes = [];
  top.types = nilTypeList(location=top.location);
}


abstract production oneParams
top::Params ::= name::String ty::Type
{
  top.pp = "<" ++ name ++ " : " ++ ty.pp ++ ">";

  top.toList = [(name, ty)];
  top.len = 1;

  top.upVarTypes = [(name, ty)];
  top.types = consTypeList(ty, nilTypeList(location=top.location),
                           location=top.location);
}
