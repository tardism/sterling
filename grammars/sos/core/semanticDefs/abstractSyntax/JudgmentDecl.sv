grammar sos:core:semanticDefs:abstractSyntax;


nonterminal JudgmentDecl with
   pp,
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, projectionDecls,
   tyEnv, constructorEnv, judgmentEnv, projectionEnv, ruleEnv,
   errors,
   location;
propagate errors on JudgmentDecl;

--New rules can be added, but only for new PC constructors
--pcIndex is zero-based into list of arguments
abstract production extJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList
{
  top.pp = "Judgment " ++ name ++ " : " ++ ty.pp_space ++ "\n";

  production fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls =
      if ty.foundPC
      then [extJudgmentEnvItem(^fullName, ty.types, ty.pcIndex)]
      else [errorJudgmentEnvItem(^fullName, ty.types)];
  top.projectionDecls = [];

  ty.tyEnv = top.tyEnv;

  --Check there is only one declaration of this judgment
  local possibleJudgments::[JudgmentEnvItem] =
        lookupEnv(^fullName, top.judgmentEnv);
  top.errors <-
      case possibleJudgments of
      | [] -> error("Impossible:  Extensible judgment " ++
                    fullName.pp ++ " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for judgment " ++
            fullName.pp, location=top.location)]
      end;

  local pcFound::Boolean = ty.foundPC;
  production pcType::Type =
      if ty.foundPC
      then head(drop(ty.pcIndex, ty.types.toList))
      else errorType(location=top.location);

  --PC must be within the types
  top.errors <-
      if pcFound
      then []
      else [errorMessage("Invalid primary component for extensible " ++
               "judgment " ++ fullName.pp, location=top.location)];

  --PC must be extensible type
  top.errors <-
      if !pcFound then []
      else case pcType of
           | nameType(_) -> []
           | errorType() -> []
           | _ -> [errorMessage("Primary component for extensible " ++
                      "judgment " ++ fullName.pp ++ " must be an " ++
                      "extensible type; found " ++ pcType.pp,
                      location=top.location)]
           end;

  --Check there is a projection rule if this is not the module
  --introducing the PC type
  top.errors <-
      if !pcFound
      then []
      else
         case pcType of
         | nameType(name) when !sameModule(top.moduleName, ^name) ->
           --must have projection rule when PC is from another module
           case findAllEnv(
                   \ r::RuleEnvItem ->
                     !r.isError && r.isDefaultRule &&
                     ^fullName == r.definedRel, top.ruleEnv) of
           | [] -> [errorMessage("Must define projection rule " ++
                       "for " ++ fullName.pp, location=top.location)]
           | [_] -> []
           | l -> [errorMessage("Can only define one projection " ++
                      "rule for " ++ fullName.pp,
                      location=top.location)]
           end
         | nameType(_) ->
           --may have projection rule, but not required
           case findAllEnv(
                   \ r::RuleEnvItem ->
                     !r.isError && r.isDefaultRule &&
                     ^fullName == r.definedRel, top.ruleEnv) of
           | [] -> []
           | [_] -> []
           | l -> [errorMessage("Can only define one projection " ++
                      "rule for " ++ fullName.pp,
                      location=top.location)]
           end
         | _ -> []
         end;
}


--No new rules can be added outside this module
--Intended to be similar to Silver function
--Does not need a PC
abstract production fixedJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList
{
  top.pp = "Fixed Judgment " ++ name ++ " : " ++ ty.pp_space ++ "\n";

  production fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls =
      [fixedJudgmentEnvItem(addQNameBase(top.moduleName, name),
                            ty.types)];
  top.projectionDecls = [];

  ty.tyEnv = top.tyEnv;

  top.errors <-
      if ty.foundPC
      then [errorMessage("Fixed judgment " ++ fullName.pp ++
                         " has a primary component but cannot",
                         location=top.location)]
      else [];

  --Check there is only one declaration of this judgment
  local possibleJudgments::[JudgmentEnvItem] =
        lookupEnv(^fullName, top.judgmentEnv);
  top.errors <-
      case possibleJudgments of
      | [] -> error("Impossible:  Fixed judgment " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for judgment " ++
            fullName.pp, location=top.location)]
      end;
}


--Generic judgment declaration defined in an erroneous way
abstract production errorJudgmentDecl
top::JudgmentDecl ::= errs::[Message] name::String ty::TypeList
{
  top.pp = "#Error judgment declaration:  " ++ name ++ "#\n";

  production fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls =
      [errorJudgmentEnvItem(addQNameBase(top.moduleName, name),
                            ty.types)];
  top.projectionDecls = [];

  ty.tyEnv = top.tyEnv;

  top.errors <- errs;

  --Check there is only one declaration of this judgment
  local possibleJudgments::[JudgmentEnvItem] =
        lookupEnv(^fullName, top.judgmentEnv);
  top.errors <-
      case possibleJudgments of
      | [] -> error("Impossible:  Error judgment " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for judgment " ++
            fullName.pp, location=top.location)]
      end;
}




--Type for the projection relation for a particular type
--e.g. Projection term : ctx  means  ctx |- term ~~> term
abstract production projectionTypeDecl
top::JudgmentDecl ::= tyname::String args::TypeList
{
  top.pp = "Projection " ++ tyname ++ " : " ++ args.pp_space ++ "\n";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.projectionDecls =
      [projectionEnvItem(^fullTyName, args.types)];

  production fullTyName::QName = addQNameBase(top.moduleName, tyname);

  args.tyEnv = top.tyEnv;

  --Check if type is declared as part of this module
  local possibleTys::[TypeEnvItem] =
        lookupEnv(addQNameBase(top.moduleName, tyname), top.tyEnv);
  top.errors <-
      case possibleTys of
      | [] ->
        [errorMessage("Projection declared for unknown type " ++
            tyname, location=top.location)]
      --we won't do an error here for multiple decls of tyname
      --that will be handled in the type declarations
      | _ -> []
      end;

  --Cannot contain variable types in declared judgment args
  top.errors <-
      if !args.containsVars then []
      else [errorMessage("Declared type arguments to projection " ++
               " for " ++ tyname ++ " cannot contain variable types",
               location=top.location)];
}


--Error for 
abstract production errorProjectionDecl
top::JudgmentDecl ::= errs::[Message] tyname::String args::TypeList
{
  top.pp = "#Error projection declaration:  " ++ tyname ++ "#\n";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.projectionDecls =
      [projectionEnvItem(addQNameBase(top.moduleName, tyname),
                         args.types)];

  args.tyEnv = top.tyEnv;

  top.errors <- errs;
}

