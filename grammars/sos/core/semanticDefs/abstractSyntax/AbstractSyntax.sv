grammar sos:core:semanticDefs:abstractSyntax;


nonterminal AbsSyntaxDecl with
   pp,
   moduleName,
   constructorDecls, tyDecls, judgmentDecls, translationDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv,
   errors,
   location;
propagate errors on AbsSyntaxDecl;

--When we first declare a type and constructors
abstract production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.pp = type ++ " ::= " ++ constructors.pp ++ "\n";

  constructors.moduleName = top.moduleName;

  production fullName::QName = addQNameBase(top.moduleName, type);

  constructors.builtType = nameType(fullName, location=top.location);

  top.constructorDecls = constructors.constructorDecls;
  top.tyDecls = typeEnvItem(fullName)::constructors.tyDecls;
  top.judgmentDecls = constructors.judgmentDecls;
  top.translationDecls = [];

  constructors.tyEnv = top.tyEnv;
  constructors.constructorEnv = top.constructorEnv;

  --Check there is exactly one translation relation declared
  local possibleTrans::[TranslationEnvItem] =
        lookupEnv(fullName, top.translationEnv);
  top.errors <-
      case possibleTrans of
      | [] ->
        [errorMessage("Must define translation relation type for " ++
            fullName.pp, location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Found multiple translation relation type " ++
            "declarations for " ++ fullName.pp, location=top.location)]
      end;

  --Check there is only one declaration of this type
  local possibleTys::[TypeEnvItem] = lookupEnv(fullName, top.tyEnv);
  top.errors <-
      case possibleTys of
      | [] -> error("Impossible:  Type " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for type " ++
            fullName.pp, location=top.location)]
      end;
}


--When we add to a type and constructors
abstract production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  top.pp = type.pp ++ " ::= ...\n" ++ constructors.pp ++ "\n";

  constructors.moduleName = top.moduleName;

  type.tyEnv = top.tyEnv;
  constructors.builtType =
     if type.tyFound
     then type.fullTy
     else errorType(location=top.location);
  top.errors <- type.tyErrors;

  top.constructorDecls = constructors.constructorDecls;
  top.tyDecls = constructors.tyDecls;
  top.judgmentDecls = constructors.judgmentDecls;
  top.translationDecls = [];

  constructors.tyEnv = top.tyEnv;
  constructors.constructorEnv = top.constructorEnv;
}





nonterminal AbsConstructorDecls with
   pp,
   moduleName,
   constructorDecls, tyDecls, judgmentDecls,
   tyEnv, constructorEnv,
   errors,
   location;
propagate errors on AbsConstructorDecls;

inherited attribute builtType::Type occurs on AbsConstructorDecls;

abstract production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.pp = "";

  top.constructorDecls = [];
  top.tyDecls = [];
  top.judgmentDecls = [];
}


abstract production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  top.pp = if d1.pp == ""
           then d2.pp
           else if d2.pp == ""
           then d1.pp
           else d1.pp ++ "\n   | " ++ d2.pp;

  d1.moduleName = top.moduleName;
  d2.moduleName = top.moduleName;

  d1.builtType = top.builtType;
  d2.builtType = top.builtType;

  top.constructorDecls = d1.constructorDecls ++ d2.constructorDecls;
  top.tyDecls = d1.tyDecls ++ d2.tyDecls;
  top.judgmentDecls = d1.judgmentDecls ++ d2.judgmentDecls;

  d1.tyEnv = top.tyEnv;
  d1.constructorEnv = top.constructorEnv;
  d2.tyEnv = top.tyEnv;
  d2.constructorEnv = top.constructorEnv;
}


abstract production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  top.pp = name ++ "(" ++ tyargs.pp_comma ++ ")";

  production fullName::QName = addQNameBase(top.moduleName, name);

  top.constructorDecls =
      [constructorEnvItem(fullName, top.builtType, tyargs.types)];
  top.tyDecls = [];
  top.judgmentDecls = [];

  tyargs.tyEnv = top.tyEnv;

  --Check this constructor is only declared once
  local possibleCons::[ConstructorEnvItem] =
        lookupEnv(fullName, top.constructorEnv);
  top.errors <-
      case possibleCons of
      | [] -> error("Impossible:  Constructor " ++ fullName.pp ++
                    "must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for " ++
            "constructor " ++ fullName.pp, location=top.location)]
      end;

  --Cannot contain variable types in declared constructor args
  top.errors <-
      if !tyargs.containsVars then []
      else [errorMessage("Declared type arguments to constructor " ++
               fullName.pp ++ " cannot contain variable types",
               location=top.location)];
}

