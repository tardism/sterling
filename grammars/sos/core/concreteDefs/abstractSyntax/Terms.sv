grammar sos:core:concreteDefs:abstractSyntax;


nonterminal Term with
   pp,
   moduleName,
   type,
   tyEnv, constructorEnv,
   productionElements,
   errors,
   location;
propagate errors on Term;

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
  args.argumentIndex = 1;
  args.lastConstructor = name.fullConstrName;
  args.expectedTypes =
       if name.constrFound
       then just(name.constrTypeArgs.toList)
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
}


abstract production intTerm
top::Term ::= i::Integer
{
  top.pp = toString(i);

  top.type = intType(location=top.location);
}


abstract production toIntTerm
top::Term ::= t::Term
{
  top.pp = "$to_int(" ++ t.pp ++ ")";

  top.type = intType(location=top.location);

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

  local prodElem::[(QName, Type, Location)] =
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

  top.type = stringType(location=top.location);

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





nonterminal TermList with
   pp,
   moduleName,
   tyEnv, constructorEnv,
   productionElements,
   typeList,
   expectedTypes, remainingTypes, lastConstructor,
   argumentIndex, nextArgumentIndex,
   errors,
   location;
propagate errors on TermList;

abstract production singleTermList
top::TermList ::= t::Term
{
  top.pp = t.pp;

  t.moduleName = top.moduleName;

  t.tyEnv = top.tyEnv;
  t.constructorEnv = top.constructorEnv;

  t.productionElements = top.productionElements;

  top.typeList = [t.type];

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

  t1.moduleName = top.moduleName;
  t2.moduleName = top.moduleName;

  t1.tyEnv = top.tyEnv;
  t2.tyEnv = top.tyEnv;
  t1.constructorEnv = top.constructorEnv;
  t2.constructorEnv = top.constructorEnv;

  t1.productionElements = top.productionElements;
  t2.productionElements = top.productionElements;

  top.typeList = t1.typeList ++ t2.typeList;

  t1.argumentIndex = top.argumentIndex;
  t2.argumentIndex = t1.nextArgumentIndex;
  top.nextArgumentIndex = t2.nextArgumentIndex;

  t1.expectedTypes = top.expectedTypes;
  t2.expectedTypes = t1.remainingTypes;
  top.remainingTypes = t2.remainingTypes;

  t1.lastConstructor = top.lastConstructor;
  t2.lastConstructor = top.lastConstructor;
}
