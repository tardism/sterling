grammar sos:core:concreteDefs:abstractSyntax;


nonterminal Term with
   pp,
   moduleName,
   type, upSubst, downSubst, finalSubst,
   tyEnv, constructorEnv, concreteEnv,
   productionElements,
   errors,
   location;
propagate errors, concreteEnv on Term;

abstract production nameTerm
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
             then name.constrType
             else errorType(location=top.location);
  top.upSubst = top.downSubst;
}


abstract production applicationTerm
top::Term ::= name::QName args::TermList
{
  top.pp = name.pp ++ "(" ++ args.pp ++ ")";

  args.moduleName = top.moduleName;

  args.tyEnv = top.tyEnv;
  args.constructorEnv = top.constructorEnv;
  name.constructorEnv = top.constructorEnv;

  args.productionElements = top.productionElements;

  top.errors <- name.constrErrors;
  top.type = if name.constrFound
             then name.constrType
             else errorType(location=top.location);
  args.downSubst = top.downSubst;
  top.upSubst = args.upSubst;

  args.argumentIndex = 1;
  args.lastConstructor = name.fullConstrName;
  args.expectedTypes =
       if name.constrFound
       then just(freshenTypeList(name.constrTypeArgs).toList)
       else nothing();

  top.errors <-
      case args.remainingTypes of
      | nothing() -> []
      | just([]) -> []
      | just(l) ->
        [errorMessage("Too many arguments to " ++
            name.fullConstrName.pp ++ ":  Expected " ++
            toString(name.constrTypeArgs.len) ++ " but found " ++
            toString(name.constrTypeArgs.len - length(l)),
            location=top.location)]
      end;
}


abstract production stringTerm
top::Term ::= s::String
{
  top.pp = "\"" ++ s ++ "\"";

  top.type = stringType(location=top.location);

  top.upSubst = top.downSubst;
}


abstract production intTerm
top::Term ::= i::Integer
{
  top.pp = toString(i);

  top.type = intType(location=top.location);

  top.upSubst = top.downSubst;
}


abstract production toIntTerm
top::Term ::= t::Term
{
  top.pp = "$to_int(" ++ t.pp ++ ")";

  local unify::TypeUnify =
      typeUnify(t.type, stringType(location=top.location),
                location=top.location);
  top.type = intType(location=top.location);
  t.downSubst = top.downSubst;
  unify.downSubst = t.upSubst;
  top.upSubst = unify.upSubst;
  t.finalSubst = top.finalSubst;

  t.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;

  t.productionElements = top.productionElements;

  top.errors <-
      if stringType(location=top.location) == t.type
      then []
      else [errorMessage("$to_int argument must be of type string; " ++
               "found " ++ t.type.pp, location=top.location)];
}


abstract production prodIndex
top::Term ::= var::String
{
  top.pp = var;

  production prodElem::[(QName, Type, Location)] =
        lookupAll(var, top.productionElements);
  top.errors <-
      case prodElem of
      | [] ->
        [errorMessage("Production variable " ++ var ++ " does " ++
                      "not exist", location=top.location)]
      --anything with multiple is an error on the production, not here
      | l -> []
      end;
  top.type =
      case prodElem of
      | (_, ty, _)::_ -> ty
      | _ -> errorType(location=top.location)
      end;
  top.upSubst = top.downSubst;
}


abstract production substringTerm
top::Term ::= t::Term i1::Maybe<Integer> i2::Maybe<Integer>
{
  top.pp =
      case i1, i2 of
      | just(i), just(j) ->
        t.pp ++ "[" ++ toString(i) ++ ":" ++ toString(j) ++ "]"
      | just(i), nothing() ->
        t.pp ++ "[" ++ toString(i) ++ ":]"
      | nothing(), just(j) ->
        t.pp ++ "[:" ++ toString(j) ++ "]"
      | nothing(), nothing() -> t.pp ++ "[:]"
      end;

  local unify::TypeUnify =
      typeUnify(t.type, stringType(location=top.location),
                location=top.location);
  top.type = stringType(location=top.location);
  t.downSubst = top.downSubst;
  unify.downSubst = t.upSubst;
  top.upSubst = unify.upSubst;

  t.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;

  t.productionElements = top.productionElements;

  top.errors <-
      case t.type of
      | stringType() -> []
      | _ ->
        [errorMessage("Can only take substring of a string; found " ++
                      t.type.pp, location=top.location)]
      end;
}


abstract production nilTerm
top::Term ::=
{
  top.pp = "[]";

  top.type = listType(varType("X" ++ toString(genInt()),
                              location=top.location),
                      location=top.location);
  top.upSubst = top.downSubst;
}


abstract production consTerm
top::Term ::= hd::Term tl::Term
{
  top.pp = "(" ++ hd.pp ++ ")::(" ++ tl.pp ++ ")";

  local unify::TypeUnify =
      typeUnify(listType(hd.type, location=top.location), tl.type,
                location=top.location);
  top.type = listType(hd.type, location=top.location);
  hd.downSubst = top.downSubst;
  tl.downSubst = hd.upSubst;
  unify.downSubst = tl.upSubst;
  top.upSubst = unify.upSubst;

  hd.moduleName = top.moduleName;
  tl.moduleName = top.moduleName;

  hd.tyEnv = top.tyEnv;
  tl.tyEnv = top.tyEnv;
  hd.constructorEnv = top.constructorEnv;
  tl.constructorEnv = top.constructorEnv;

  hd.productionElements = top.productionElements;
  tl.productionElements = top.productionElements;
}


abstract production tupleTerm
top::Term ::= contents::TermList
{
  top.pp = "(|" ++ contents.pp ++ "|)";

  top.type = tupleType(toTypeList(contents.typeList, top.location),
                       location=top.location);
  contents.downSubst = top.downSubst;
  top.upSubst = contents.upSubst;

  contents.expectedTypes =
      just(map(\ x::Integer ->
                 varType("Tuple" ++ toString(x) ++ "_" ++
                         toString(genInt()),
                         location=top.location),
               range(1, contents.len + 1)));

  contents.moduleName = top.moduleName;

  contents.lastConstructor = toQName("tuple", top.location);
  contents.argumentIndex = 1;

  contents.tyEnv = top.tyEnv;
  contents.constructorEnv = top.constructorEnv;

  contents.productionElements = top.productionElements;
}





nonterminal TermList with
   pp,
   moduleName,
   tyEnv, constructorEnv, concreteEnv,
   productionElements,
   toList<Term>, len,
   typeList, upSubst, downSubst,
   expectedTypes, remainingTypes, lastConstructor,
   argumentIndex, nextArgumentIndex,
   errors,
   location;
propagate errors, concreteEnv on TermList;

abstract production singleTermList
top::TermList ::= t::Term
{
  top.pp = t.pp;

  top.toList = [t];
  top.len = 1;

  t.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;

  t.productionElements = top.productionElements;

  top.typeList = [t.type];

  local unify::TypeUnify =
      case top.expectedTypes of
      | just(hd::_) ->
        typeUnify(hd, t.type, location=top.location)
      | _ -> typeUnify(errorType(location=top.location),
                       errorType(location=top.location),
                       location=top.location)
      end;
  t.downSubst = top.downSubst;
  unify.downSubst = t.upSubst;
  top.upSubst = unify.upSubst;

  top.errors <-
      case top.expectedTypes of
      | nothing() -> []
      | just([]) ->
        [errorMessage("Too few arguments to constructor " ++
            top.lastConstructor.pp, location=top.location)]
      | just(h::tl) when h == t.type -> []
      | just(h::tl) ->
        [errorMessage("Argument " ++ toString(top.argumentIndex) ++
            " to " ++ top.lastConstructor.pp ++ " has type " ++
            t.type.pp ++ " but was expected to have type " ++
            h.pp, location=top.location)]
      end;
  top.nextArgumentIndex = top.argumentIndex + 1;
  top.remainingTypes =
      case top.expectedTypes of
      | just(_::tl) -> just(tl)
      | _ -> nothing()
      end;
}


abstract production branchTermList
top::TermList ::= t1::TermList t2::TermList
{
  top.pp = t1.pp ++ ", " ++ t2.pp;

  top.toList = t1.toList ++ t2.toList;
  top.len = t1.len + t2.len;

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t2.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.constructorEnv = top.constructorEnv;

  t1.productionElements = top.productionElements;
  t2.productionElements = top.productionElements;

  top.typeList = t1.typeList ++ t2.typeList;

  t1.downSubst = top.downSubst;
  t2.downSubst = t1.upSubst;
  top.upSubst = t2.upSubst;

  t1.argumentIndex = top.argumentIndex;
  t2.argumentIndex = t1.nextArgumentIndex;
  top.nextArgumentIndex = t2.nextArgumentIndex;

  t1.expectedTypes = top.expectedTypes;
  t2.expectedTypes = t1.remainingTypes;
  top.remainingTypes = t2.remainingTypes;

  t1.lastConstructor = top.lastConstructor;
  t2.lastConstructor = top.lastConstructor;
}
