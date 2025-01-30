grammar sos:core:semanticDefs:abstractSyntax;


nonterminal Term with
   pp,
   moduleName,
   tyEnv, constructorEnv,
   type, upSubst, downSubst, finalSubst,
   downVarTypes, upVarTypes,
   headIsConstructor, headConstructorCurrentModule, isVariable,
   headConstructor,
   errors,
   location;
propagate errors on Term;

--whether the term is headed by a constructor
synthesized attribute headIsConstructor::Boolean;
--whether the term's head is from the current module or is imported
synthesized attribute headConstructorCurrentModule::Boolean;
--constructor of the term at the head
synthesized attribute headConstructor::QName;
--whether the term is a variable
synthesized attribute isVariable::Boolean;

abstract production const
top::Term ::= name::QName
{
  top.pp = name.pp;

  name.constructorEnv = top.constructorEnv;

  top.errors <- name.constrErrors;
  top.errors <-
      if name.constrFound
      then if name.constrTypeArgs.len == 0
           then []
           else [errorMessage("Too few arguments to constructor " ++
                    name.pp, location=top.location)]
      else [];
  top.type = if name.constrFound
             then freshenType(name.constrType)
             else errorType(location=top.location);

  top.upSubst = top.downSubst;

  top.upVarTypes = top.downVarTypes;

  top.headIsConstructor = true;
  top.headConstructorCurrentModule =
      if name.constrFound
      then addQNameBase(top.moduleName, name.base) ==
           name.fullConstrName
      else true; --if not found, just assume it matches
  top.isVariable = false;

  top.headConstructor =
      if name.constrFound
      then name.fullConstrName
      else ^name;
}


abstract production var
top::Term ::= name::String
{
  top.pp = name;

  top.upVarTypes =
      if lookup(name, top.downVarTypes).isJust
      then top.downVarTypes
      else (name, top.type)::top.downVarTypes;
  top.type =
      case lookup(name, top.downVarTypes) of
      | just(ty) -> ty
      | nothing() ->
        varType("__var_" ++ name ++ "_" ++ toString(genInt()),
                location=top.location)
      end;

  top.upSubst = top.downSubst;

  top.headIsConstructor = false;
  top.headConstructorCurrentModule = false; --placeholder
  top.isVariable = true;

  top.headConstructor = baseName("err", location=bogusLoc());
}


abstract production num
top::Term ::= int::Integer
{
  top.pp = toString(int);

  top.type = intType(location=top.location);

  top.upSubst = top.downSubst;

  top.upVarTypes = top.downVarTypes;

  top.headIsConstructor = false;
  top.headConstructorCurrentModule = false; --placeholder
  top.isVariable = false;

  top.headConstructor = baseName("err", location=bogusLoc());
}


abstract production stringConst
top::Term ::= s::String
{
  top.pp = "\"" ++ s ++ "\"";

  top.type = stringType(location=top.location);

  top.upSubst = top.downSubst;

  top.upVarTypes = top.downVarTypes;

  top.headIsConstructor = false;
  top.headConstructorCurrentModule = false; --placeholder
  top.isVariable = false;

  top.headConstructor = baseName("err", location=bogusLoc());
}


abstract production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.pp = constructor.pp ++ "(" ++ args.pp_comma ++ ")";

  args.moduleName = top.moduleName;

  constructor.constructorEnv = top.constructorEnv;
  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;

  top.errors <- constructor.constrErrors;
  top.type = if constructor.constrFound
             then freshenType(constructor.constrType)
             else errorType(location=top.location);

  args.isConclusion = false;
  args.isExtensibleRule = false;
  args.isProjectionRule = false;
  args.expectedPC = nothing();

  args.lastConstructor = ^constructor;
  args.expectedTypes =
       if constructor.constrFound
       then just(constructor.constrTypeArgs)
       else nothing();
  args.downSubst = top.downSubst;
  top.upSubst = args.upSubst;
  args.finalSubst = top.finalSubst;

  args.downVarTypes = top.downVarTypes;
  top.upVarTypes = args.upVarTypes;

  top.headIsConstructor = true;
  top.headConstructorCurrentModule =
      if constructor.constrFound
      then addQNameBase(top.moduleName, constructor.base) ==
           constructor.fullConstrName
      else true; --if not found, just assume it matches
  top.isVariable = false;

  top.headConstructor =
      if constructor.constrFound
      then constructor.fullConstrName
      else baseName("err", location=bogusLoc());
}


abstract production tupleTerm
top::Term ::= contents::TermList
{
  top.pp = "(" ++ contents.pp_comma ++ ")";

  contents.moduleName = top.moduleName;

  contents.constructorEnv = top.constructorEnv;
  contents.tyEnv = top.tyEnv;

  --"expect" a list of any types of the right length
  contents.expectedTypes =
      just(foldr(consTypeList(_, _, location=top.location),
                 nilTypeList(location=top.location),
                 map(\ x::Integer ->
                       varType("Tuple" ++ toString(x) ++ "_" ++
                               toString(genInt()),
                               location=top.location),
                     range(1, contents.len + 1))));
  contents.expectedPC = nothing();
  contents.isProjectionRule = false;
  contents.isExtensibleRule = false;
  contents.isConclusion = false;
  contents.lastConstructor =
      error("Should not access lastConstructor (tupleTerm)");

  contents.downSubst = top.downSubst;
  top.upSubst = contents.upSubst;
  contents.finalSubst = top.finalSubst;

  top.type = tupleType(contents.types, location=top.location);

  contents.downVarTypes = top.downVarTypes;
  top.upVarTypes = contents.upVarTypes;

  top.headIsConstructor = false;
  top.headConstructorCurrentModule = false; --placeholder
  top.isVariable = false;

  top.headConstructor = baseName("err", location=bogusLoc());
}


abstract production nilTerm
top::Term ::=
{
  top.pp = "[]";

  top.upSubst = top.downSubst;

  --fresh type variable in there
  top.type = listType(varType("X" ++ toString(genInt()),
                              location=top.location),
                      location=top.location);

  top.upVarTypes = top.downVarTypes;

  top.headIsConstructor = false;
  top.headConstructorCurrentModule = false; --placeholder
  top.isVariable = false;

  top.headConstructor = baseName("err", location=bogusLoc());
}


abstract production consTerm
top::Term ::= hd::Term tl::Term
{
  top.pp = "(" ++ hd.pp ++ ")::(" ++ tl.pp ++ ")";

  hd.moduleName = top.moduleName;
  tl.moduleName = top.moduleName;

  hd.constructorEnv = top.constructorEnv;
  tl.constructorEnv = top.constructorEnv;
  hd.tyEnv = top.tyEnv;
  tl.tyEnv = top.tyEnv;

  local unify::TypeUnify =
      typeUnify(listType(hd.type, location=top.location), tl.type,
                location=top.location);
  hd.downSubst = top.downSubst;
  tl.downSubst = hd.upSubst;
  unify.downSubst = tl.upSubst;
  top.upSubst = unify.upSubst;
  hd.finalSubst = top.finalSubst;
  tl.finalSubst = top.finalSubst;

  top.type = listType(hd.type, location=top.location);

  hd.downVarTypes = top.downVarTypes;
  tl.downVarTypes = hd.upVarTypes;
  top.upVarTypes = tl.upVarTypes;

  top.headIsConstructor = false;
  top.headConstructorCurrentModule = false; --placeholder
  top.isVariable = false;

  top.headConstructor = baseName("err", location=bogusLoc());
}


abstract production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.pp = "(" ++ tm.pp ++ " : " ++ ty.pp ++ ")";

  tm.moduleName = top.moduleName;

  tm.constructorEnv = top.constructorEnv;
  tm.tyEnv = top.tyEnv;
  ty.tyEnv = top.tyEnv;

  local unify::TypeUnify =
        typeUnify(tm.type, @ty, location=top.location);
  tm.downSubst = top.downSubst;
  unify.downSubst = tm.upSubst;
  top.upSubst = unify.upSubst;
  tm.finalSubst = top.finalSubst;

  top.type = ^ty;

  tm.downVarTypes = top.downVarTypes;
  top.upVarTypes = tm.upVarTypes;

  top.headIsConstructor = tm.headIsConstructor;
  top.headConstructorCurrentModule = tm.headConstructorCurrentModule;
  top.isVariable = tm.isVariable;

  top.headConstructor = tm.headConstructor;
}





nonterminal TermList with
   pp_comma, pp_space,
   moduleName,
   tyEnv, constructorEnv,
   types, upSubst, downSubst, finalSubst,
   expectedTypes, lastConstructor,
   downVarTypes, upVarTypes,
   toList<Term>, len,
   expectedPC, isConclusion, isExtensibleRule, isProjectionRule,
   errors,
   location;
propagate errors on TermList;

--When zero, that is the PC
inherited attribute expectedPC::Maybe<Integer>;

abstract production nilTermList
top::TermList ::=
{
  top.pp_comma = "";
  top.pp_space = "";

  top.toList = [];
  top.len = 0;

  top.upSubst = top.downSubst;

  top.errors <-
      case top.expectedTypes of
      | nothing() -> []
      | just(nilTypeList()) -> []
      | just(consTypeList(x, l)) ->
        [errorMessage("Too few arguments to " ++
            top.lastConstructor.pp, location=top.location)]
      end;

  top.types = nilTypeList(location=top.location);

  top.upVarTypes = top.downVarTypes;
}


abstract production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.pp_comma = if rest.pp_comma == ""
                 then t.pp else t.pp ++ ", " ++ rest.pp_comma;
  top.pp_space = if rest.pp_space == ""
                 then t.pp else t.pp ++ " " ++ rest.pp_space;

  t.moduleName = top.moduleName;
  rest.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;
  rest.tyEnv = top.tyEnv;
  rest.constructorEnv = top.constructorEnv;

  top.toList = ^t::rest.toList;
  top.len = 1 + rest.len;

  t.downSubst = top.downSubst;
  local unifyFirst::TypeUnify =
      case top.expectedTypes of
      | just(consTypeList(ty, l)) ->
        typeUnify(t.type, ^ty, location=top.location)
             --unify useless as a placeholder
      | _ -> typeUnify(errorType(location=top.location), t.type,
                       location=top.location)
      end;
  rest.downSubst = t.upSubst;
  unifyFirst.downSubst = rest.upSubst;
  top.upSubst = unifyFirst.upSubst;
  t.finalSubst = top.finalSubst;
  rest.finalSubst = top.finalSubst;

  rest.lastConstructor = top.lastConstructor;
  rest.expectedTypes =
       case top.expectedTypes of
       | just(consTypeList(_, l)) -> just(^l)
       | _ -> nothing()
       end;
  top.errors <-
      case top.expectedTypes of
      | just(nilTypeList()) ->
        [errorMessage("Too many arguments to " ++
            top.lastConstructor.pp, location=top.location)]
      | _ -> []
      end;

  top.types = consTypeList(t.type, rest.types, location=top.location);

  t.downVarTypes = top.downVarTypes;
  rest.downVarTypes = t.upVarTypes;
  top.upVarTypes = rest.upVarTypes;

  rest.expectedPC = bind(top.expectedPC, \ x::Integer -> just(x - 1));
  rest.isConclusion = top.isConclusion;
  rest.isExtensibleRule = top.isExtensibleRule;
  rest.isProjectionRule = top.isProjectionRule;
  top.errors <-
      if !top.isConclusion
      then []
      else if !top.isExtensibleRule
      then []
      else case top.expectedPC of
           | just(0) ->
             if top.isProjectionRule
             then if t.isVariable then []
                  else [errorMessage("Primary component of " ++
                           "relation " ++ top.lastConstructor.pp ++
                           " in projection rule must be variable;" ++
                           " found " ++ t.pp, location=top.location)]
             else if sameModule(top.moduleName, top.lastConstructor)
                  then [] --initial definition can define anything
                  else if !t.headIsConstructor
                  then [errorMessage("Primary component of " ++
                           "imported relation " ++
                           top.lastConstructor.pp ++
                           " in rule conclusion must be a constructor",
                           location=top.location)]
                  else if !t.headConstructorCurrentModule
                  then [errorMessage("Primary component of " ++
                           "imported relation " ++
                           top.lastConstructor.pp ++
                           " in rule conclusion must be a " ++
                           "constructor introduced in this module",
                           location=top.location)]
                  else [] --PC built by a new constructor
           | _ -> []
           end;
}

