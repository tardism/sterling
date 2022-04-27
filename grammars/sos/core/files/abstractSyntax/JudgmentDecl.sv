grammar sos:core:files:abstractSyntax;


nonterminal JudgmentDecl with
   pp,
   moduleName,
   tyDecls, constructorDecls, judgmentDecls, translationDecls,
   tyEnv, constructorEnv, judgmentEnv, translationEnv, ruleEnv,
   errors,
   location;
propagate errors on JudgmentDecl;

--New rules can be added, but only for new PC constructors
--pcIndex is zero-based into list of arguments
abstract production extJudgmentDecl
top::JudgmentDecl ::= name::String ty::TypeList pcIndex::Integer
{
  top.pp = "Judgment " ++ name ++ " : " ++ ty.pp_space ++ "\n";

  local fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [extJudgmentEnvItem(fullName, ty, pcIndex)];
  top.translationDecls = [];

  ty.tyEnv = top.tyEnv;
  ty.expectedPCIndex = just(pcIndex);

  --Check there is only one declaration of this judgment
  local possibleJudgments::[JudgmentEnvItem] =
        lookupEnv(fullName, top.judgmentEnv);
  top.errors <-
      case possibleJudgments of
      | [] -> error("Impossible:  Extensible judgment " ++
                    fullName.pp ++ " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for judgment " ++
            fullName.pp, location=top.location)]
      end;

  --Cannot contain variable types in declared judgment args
  top.errors <-
      if !ty.containsVars then []
      else [errorMessage("Declared type arguments to judgment " ++
               fullName.pp ++ " cannot contain variable types",
               location=top.location)];

  local pcFound::Boolean = ty.len > pcIndex;
  production pcType::Type = head(drop(pcIndex - 1, ty.types.toList));

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

  --Check there is a translation rule if this is not the module
  --introducing the PC type
  top.errors <-
      if !pcFound
      then []
      else
         case pcType of
         | nameType(name) when !sameModule(top.moduleName, name) ->
           --must have translation rule when PC is from another module
           case findAllEnv(
                   \ r::RuleEnvItem ->
                     !r.isError && r.isTransRule &&
                     fullName == r.definedRel, top.ruleEnv) of
           | [] -> [errorMessage("Must define translation rule " ++
                       "for " ++ fullName.pp, location=top.location)]
           | [_] -> []
           | l -> [errorMessage("Can only define one translation " ++
                      "rule for " ++ fullName.pp,
                      location=top.location)]
           end
         | nameType(_) ->
           --may have translation rule, but not required
           case findAllEnv(
                   \ r::RuleEnvItem ->
                     !r.isError && r.isTransRule &&
                     fullName == r.definedRel, top.ruleEnv) of
           | [] -> []
           | [_] -> []
           | l -> [errorMessage("Can only define one translation " ++
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

  local fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls =
      [fixedJudgmentEnvItem(addQNameBase(top.moduleName, name), ty)];
  top.translationDecls = [];

  ty.tyEnv = top.tyEnv;
  ty.expectedPCIndex = nothing();

  --Check there is only one declaration of this judgment
  local possibleJudgments::[JudgmentEnvItem] =
        lookupEnv(fullName, top.judgmentEnv);
  top.errors <-
      case possibleJudgments of
      | [] -> error("Impossible:  Fixed judgment " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for judgment " ++
            fullName.pp, location=top.location)]
      end;

  --Cannot contain variable types in declared judgment args
  top.errors <-
      if !ty.containsVars then []
      else [errorMessage("Declared type arguments to judgment " ++
               fullName.pp ++ " cannot contain variable types",
               location=top.location)];
}


--Generic judgment declaration defined in an erroneous way
abstract production errorJudgmentDecl
top::JudgmentDecl ::= errs::[Message] name::String ty::TypeList
{
  top.pp = "#Error judgment declaration:  " ++ name ++ "#\n";

  local fullName::QName = addQNameBase(top.moduleName, name);

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls =
      [errorJudgmentEnvItem(addQNameBase(top.moduleName, name), ty)];
  top.translationDecls = [];

  ty.tyEnv = top.tyEnv;
  ty.expectedPCIndex = nothing();

  top.errors <- errs;

  --Check there is only one declaration of this judgment
  local possibleJudgments::[JudgmentEnvItem] =
        lookupEnv(fullName, top.judgmentEnv);
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




--Type for the translation relation for a particular type
--e.g. Translation term : ctx  means  ctx |- term ~~> term
abstract production translationTypeDecl
top::JudgmentDecl ::= tyname::String args::TypeList
{
  top.pp = "Translation " ++ tyname ++ " : " ++ args.pp_space ++ "\n";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls =
      [translationEnvItem(addQNameBase(top.moduleName, tyname), args)];

  args.tyEnv = top.tyEnv;
  args.expectedPCIndex = nothing();

  --Check if type is declared as part of this module
  local possibleTys::[TypeEnvItem] =
        lookupEnv(addQNameBase(top.moduleName, tyname), top.tyEnv);
  top.errors <-
      case possibleTys of
      | [] ->
        [errorMessage("Translation declared for unknown type " ++
            tyname, location=top.location)]
      --we won't do an error here for multiple decls of tyname
      --that will be handled in the type declarations
      | _ -> []
      end;

  --Cannot contain variable types in declared judgment args
  top.errors <-
      if !args.containsVars then []
      else [errorMessage("Declared type arguments to translation " ++
               " for " ++ tyname ++ " cannot contain variable types",
               location=top.location)];
}


--Error for 
abstract production errorTranslationDecl
top::JudgmentDecl ::= errs::[Message] tyname::String args::TypeList
{
  top.pp = "#Error translation declaration:  " ++ tyname ++ "#\n";

  top.tyDecls = [];
  top.constructorDecls = [];
  top.judgmentDecls = [];
  top.translationDecls =
      [translationEnvItem(addQNameBase(top.moduleName, tyname), args)];

  args.tyEnv = top.tyEnv;
  args.expectedPCIndex = nothing();

  top.errors <- errs;
}

