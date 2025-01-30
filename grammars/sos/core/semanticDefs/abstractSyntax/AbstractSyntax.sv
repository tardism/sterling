grammar sos:core:semanticDefs:abstractSyntax;


nonterminal AbsSyntaxDecl with
   pp,
   moduleName,
   constructorDecls, tyDecls, judgmentDecls, projectionDecls,
   tyEnv, constructorEnv, judgmentEnv, projectionEnv,
   projRuleConstructors_down,
   errors,
   location;
propagate errors on AbsSyntaxDecl;

inherited attribute expectedProjRules::Boolean;

--When we first declare a type and constructors
abstract production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.pp = type ++ " ::= " ++ constructors.pp ++ "\n";

  constructors.moduleName = top.moduleName;

  production fullName::QName = addQNameBase(top.moduleName, type);

  constructors.builtType = nameType(^fullName, location=top.location);

  top.constructorDecls = constructors.constructorDecls;
  top.tyDecls = typeEnvItem(^fullName)::constructors.tyDecls;
  top.judgmentDecls = constructors.judgmentDecls;
  top.projectionDecls = [];

  constructors.tyEnv = top.tyEnv;
  constructors.constructorEnv = top.constructorEnv;
  constructors.expectedProjRules = false;
  constructors.projRuleConstructors_down =
      top.projRuleConstructors_down;

  --Check there is exactly one projection relation declared
  local possibleProj::[ProjectionEnvItem] =
        lookupEnv(^fullName, top.projectionEnv);
  top.errors <-
      case possibleProj of
      | [] ->
        [errorMessage("Must define projection relation type for " ++
            fullName.pp, location=top.location)]
      | [_] -> []
      | l ->
        [errorMessage("Found multiple projection relation type " ++
            "declarations for " ++ fullName.pp, location=top.location)]
      end;

  --Check there is only one declaration of this type
  local possibleTys::[TypeEnvItem] = lookupEnv(^fullName, top.tyEnv);
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
  constructors.expectedProjRules =
      type.tyFound &&
      case type.fullTy of
      | nameType(n) -> !sameModule(top.moduleName, ^n)
      | _ -> false
      end;
  constructors.projRuleConstructors_down =
      top.projRuleConstructors_down;

  type.tyEnv = top.tyEnv;
  constructors.builtType =
     if type.tyFound
     then type.fullTy
     else errorType(location=top.location);
  top.errors <- type.tyErrors;
  top.errors <-
      if type.tyFound
      then case type.fullTy of
           | nameType(_) -> []
           | _ ->
             [errorMessage("Can only define new constructors for " ++
                 "user-defined types, not " ++ type.fullTy.pp,
                 location=top.location)]
           end
      else [];

  top.constructorDecls = constructors.constructorDecls;
  top.tyDecls = constructors.tyDecls;
  top.judgmentDecls = constructors.judgmentDecls;
  top.projectionDecls = [];

  constructors.tyEnv = top.tyEnv;
  constructors.constructorEnv = top.constructorEnv;
}





nonterminal AbsConstructorDecls with
   pp,
   moduleName,
   constructorDecls, tyDecls, judgmentDecls,
   tyEnv, constructorEnv,
   expectedProjRules, projRuleConstructors_down,
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

  d1.expectedProjRules = top.expectedProjRules;
  d2.expectedProjRules = top.expectedProjRules;
  d1.projRuleConstructors_down = top.projRuleConstructors_down;
  d2.projRuleConstructors_down = top.projRuleConstructors_down;

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
      [constructorEnvItem(^fullName, top.builtType, tyargs.types)];
  top.tyDecls = [];
  top.judgmentDecls = [];

  tyargs.tyEnv = top.tyEnv;

  --Check this constructor is only declared once
  local possibleCons::[ConstructorEnvItem] =
        lookupEnv(^fullName, top.constructorEnv);
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

  --Must have a projection rule if one is expected
  top.errors <-
      if top.expectedProjRules
      then if contains(^fullName, top.projRuleConstructors_down)
           then []
           else [errorMessage("Missing rule for projecting " ++
                    fullName.pp, location=top.location)]
      else [];
}

