grammar sos:core:main:abstractSyntax;

nonterminal Expr with
   pp,
   tyEnv, constructorEnv, judgmentEnv, concreteEnv, funEnv,
   translationEnv, moduleName,
   type, upSubst, downSubst, finalSubst, finalType,
   downVarTypes,
   errors,
   location;
propagate errors, funEnv, tyEnv, constructorEnv, judgmentEnv,
          concreteEnv, translationEnv, moduleName on Expr;
propagate finalSubst on Expr excluding deriveExpr;

synthesized attribute finalType::Type;
aspect default production top::Expr ::=
{
  top.finalType = performSubstitutionType(top.type, top.finalSubst);
}

abstract production letExpr
top::Expr ::= names::[String] e1::Expr e2::Expr
{
  top.pp = "Let " ++ implode(", ", names) ++ " := " ++ e1.pp ++
           " in " ++ e2.pp;

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes =
     case namesTy, names of
     | _, [x] -> (x, e1.type)::top.downVarTypes
     | tupleType(tys), _ ->
       zipWith(pair, names, tys.toList) ++ top.downVarTypes
     | _, _ -> error("Impossible")
     end;
  local namesTy::Type =
     tupleType(toTypeList(
                  map(\ x::String -> varType(x ++ toString(genInt()),
                                             location=top.location),
                      names), top.location), location=top.location);

  local unify::TypeUnify =
      case names of
      | [_] -> blankUnify(location=e1.location)
      | _ -> typeUnify(e1.type, namesTy, location=top.location)
      end;
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unify.downSubst = e2.upSubst;
  top.upSubst = unify.upSubst;

  top.type = e2.type;

  top.errors <-
      case e1.type of
      | tupleType(tys) ->
        if tys.len == length(names)
        then []
        else [errorMessage("Expected " ++ toString(tys.len) ++
                 " names but found " ++ toString(length(names)),
                 location=top.location)]
      | _ ->
        if length(names) > 1
        then [errorMessage("Can only have one name for result of " ++
                 "type " ++ e1.type.pp ++ " but found " ++
                 toString(length(names)), location=top.location)]
        else []
      end;
}


abstract production seqExpr
top::Expr ::= a::Expr b::Expr
{
  top.pp = "(" ++ a.pp ++ ") Before (" ++ b.pp ++ ")";

  a.downVarTypes = top.downVarTypes;
  b.downVarTypes = top.downVarTypes;

  local unify::TypeUnify =
      typeUnify(a.type, unitType(location=top.location),
                location=top.location);
  a.downSubst = top.downSubst;
  b.downSubst = a.upSubst;
  unify.downSubst = b.upSubst;
  top.upSubst = unify.upSubst;

  top.type = b.type;
}


abstract production ifExpr
top::Expr ::= cond::Expr th::Expr el::Expr
{
  top.pp = "If " ++ cond.pp ++ " Then " ++ th.pp ++ " Else " ++ el.pp;

  cond.downVarTypes = top.downVarTypes;
  th.downVarTypes = top.downVarTypes;
  el.downVarTypes = top.downVarTypes;

  top.type = if th.type == el.type
             then th.type
             else errorType(location=top.location);

  local unifyCond::TypeUnify =
      typeUnify(cond.type, boolType(location=top.location),
                location=top.location);
  local unifyBranches::TypeUnify =
      typeUnify(th.type, el.type, location=top.location);
  cond.downSubst = top.downSubst;
  th.downSubst = cond.upSubst;
  el.downSubst = th.upSubst;
  unifyCond.downSubst = el.upSubst;
  unifyBranches.downSubst = unifyCond.upSubst;
  top.upSubst = unifyBranches.upSubst;
}


abstract production printExpr
top::Expr ::= e::Expr
{
  top.pp = "Print " ++ e.pp;

  e.downVarTypes = top.downVarTypes;
  e.downSubst = top.downSubst;
  top.upSubst = e.upSubst;
  top.type = unitType(location=top.location);

  top.errors <-
      case performSubstitutionType(e.type, top.finalSubst) of
      | listType(_) ->
        [errorMessage("Cannot print lists", location=top.location)]
      | tupleType(_) ->
        [errorMessage("Cannot print tuples", location=top.location)]
      | unitType() -> [errorMessage("Cannot print " ++ e.type.pp,
                                    location=top.location)]
      | _ -> []
      end;
}


abstract production writeExpr
top::Expr ::= e::Expr file::Expr
{
  top.pp = "Write " ++ e.pp ++ " to " ++ file.pp;

  e.downVarTypes = top.downVarTypes;
  file.downVarTypes = top.downVarTypes;

  top.type = unitType(location=top.location);

  local unifyE::TypeUnify =
      typeUnify(e.type, stringType(location=top.location),
                location=top.location);
  local unifyFile::TypeUnify =
      typeUnify(file.type, stringType(location=top.location),
                location=top.location);
  e.downSubst = top.downSubst;
  file.downSubst = e.upSubst;
  unifyE.downSubst = file.upSubst;
  unifyFile.downSubst = unifyE.upSubst;
  top.upSubst = unifyFile.upSubst;
}


abstract production readExpr
top::Expr ::= file::Expr
{
  top.pp = "Read " ++ file.pp;

  file.downVarTypes = top.downVarTypes;

  top.type = stringType(location=top.location);

  local unify::TypeUnify =
      typeUnify(file.type, stringType(location=top.location),
                location=top.location);
  file.downSubst = top.downSubst;
  unify.downSubst = file.upSubst;
  top.upSubst = unify.upSubst;
}


--vars are the bindings we want out of the judgment
abstract production deriveExpr
top::Expr ::= j::Judgment useVars::[String] vars::[String]
{
  top.pp = "Derive {" ++ j.pp ++ "} " ++
           (if null(useVars) then ""
                             else "for " ++ implode(", ", useVars)) ++
           "assigning [" ++ implode(", ", vars) ++ "]";

  j.isConclusion = false;
  j.isExtensibleRule = false;
  j.isTranslationRule = false;

  j.downVarTypes =
      map(\ p::(String, Type) ->
            (p.1, performSubstitutionType(p.2, top.downSubst)),
          top.downVarTypes);
  j.downSubst = emptySubst();
  j.finalSubst = j.upSubst;
  top.upSubst = top.downSubst;

  local lkpVarsTys::[(String, Maybe<Type>)] =
      map(\ v::String -> (v, lookup(v, j.upVarTypes)), vars);
  local varsTys::[Type] =
      map(\ p::(String, Maybe<Type>) ->
            case p.2 of
            | nothing() -> errorType(location=top.location)
            | just(t) -> --substitute to get actual types
              performSubstitutionType(t, j.upSubst)
            end,
          lkpVarsTys);
  top.type =
      if null(vars)
      then boolType(location=top.location)
      else tupleType(
              toTypeList(boolType(location=top.location)::varsTys,
                         top.location),
              location=top.location);

  --check for duplicate var names being assigned
  top.errors <-
      flatMap(\ g::[String] ->
                if length(g) > 1
                then [errorMessage("Multiple declarations of " ++
                         "variable " ++ head(g) ++ " in Derive",
                         location=top.location)]
                else [],
              group(sort(vars)));
  --check for duplicate var names being sent in
  top.errors <-
      flatMap(\ g::[String] ->
                if length(g) > 1
                then [errorMessage("Multiple declarations of using" ++
                         " variable " ++ head(g) ++ " in Derive",
                         location=top.location)]
                else [],
              group(sort(useVars)));
  --check for vars being defined by the judgment
  top.errors <-
      flatMap(\ p::(String, Maybe<Type>) ->
                case p.2 of
                | just(_) -> []
                | nothing() ->
                  [errorMessage("Variable " ++ p.1 ++ " does not " ++
                                "occur in the derived judgment",
                                location=top.location)]
                end,
         lkpVarsTys ++ map(\ v::String ->
                             (v, lookup(v, j.upVarTypes)), useVars));
  --check for overlap between vars sent in and vars assigned
  top.errors <-
      flatMap(
         \ x::String ->
           if contains(x, vars)
           then [errorMessage("Variable " ++ x ++ " cannot both " ++
                    "be used and assigned", location=top.location)]
           else [], useVars);
  --check for a problem unifying in the judgment
  top.errors <- errorsFromSubst(j.upSubst);
}


--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
abstract production parseExpr
top::Expr ::= nt::QName parseString::Expr
{
  top.pp = "Parse " ++ nt.pp ++ " from " ++ parseString.pp ++ "\n";

  parseString.downVarTypes = top.downVarTypes;

  top.type = --(bool, parsed AST, error string)
      tupleType(
         consTypeList(boolType(location=top.location),
         consTypeList(if nt.concreteFound
                      then nt.concreteType
                      else errorType(location=top.location),
         consTypeList(stringType(location=top.location),
         nilTypeList(location=top.location),
                     location=top.location),
                     location=top.location),
                     location=top.location),
         location=top.location);

  local unify::TypeUnify =
      typeUnify(parseString.type, stringType(location=top.location),
                location=top.location);
  parseString.downSubst = top.downSubst;
  unify.downSubst = parseString.upSubst;
  top.upSubst = unify.upSubst;

  top.errors <-
      if !nt.concreteFound
      then [errorMessage("Unknown concrete nonterminal " ++ nt.pp,
            location=nt.location)]
      else if !nt.isConcreteNt
      then [errorMessage(nt.pp ++ " is not a concrete nonterminal " ++
               "but must be one to be parsed", location=nt.location)]
      else [];
}


abstract production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") || (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, boolType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, boolType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production andExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") && (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, boolType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, boolType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production ltExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") < (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production gtExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") > (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production leExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") <= (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production geExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") >= (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production eqExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") = (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unify::TypeUnify =
      typeUnify(e1.type, e2.type, location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unify.downSubst = e2.upSubst;
  top.upSubst = unify.upSubst;
}


abstract production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") + (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production minusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") - (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production multExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") * (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production divExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") / (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production modExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") % (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unifyLeft::TypeUnify =
      typeUnify(e1.type, intType(location=top.location),
                location=top.location);
  local unifyRight::TypeUnify =
      typeUnify(e2.type, intType(location=top.location),
                location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unifyLeft.downSubst = top.downSubst;
  unifyRight.downSubst = unifyLeft.upSubst;
  top.upSubst = unifyRight.upSubst;
}


abstract production appendExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") ++ (" ++ e2.pp ++ ")";

  top.type =
      varType("App" ++ toString(genInt()), location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  local unify::TypeUnify =
      typeUnify(e1.type, e2.type, location=top.location);
  e1.downSubst = top.downSubst;
  e2.downSubst = e1.upSubst;
  unify.downSubst = e2.upSubst;
  top.upSubst = unify.upSubst;

  top.errors <-
      case performSubstitutionType(e1.type, top.finalSubst) of
      | stringType() -> []
      | listType(_) -> []
      | errorType() -> []
      | t ->
        [errorMessage("Can only append strings and lists, not " ++
                      t.pp, location=top.location)]
      end;
}


abstract production varExpr
top::Expr ::= name::String
{
  top.pp = name;

  local lkp::Maybe<Type> = lookup(name, top.downVarTypes);
  top.upSubst = top.downSubst;
  top.type =
      case lkp of
      | nothing() -> errorType(location=top.location)
      | just(t) -> t
      end;

  top.errors <-
      case lkp of
      | just(_) -> []
      | nothing() ->
        [errorMessage("Unknown variable " ++ name,
                      location=top.location)]
      end;
}


abstract production intExpr
top::Expr ::= i::Integer
{
  top.pp = toString(i);

  top.upSubst = top.downSubst;
  top.type = intType(location=top.location);
}


abstract production stringExpr
top::Expr ::= s::String
{
  top.pp = "\"" ++ s ++ "\"";

  top.upSubst = top.downSubst;
  top.type = stringType(location=top.location);
}


abstract production tupleExpr
top::Expr ::= contents::Args
{
  top.pp = "(|" ++ contents.pp ++ "|)";

  contents.downVarTypes = top.downVarTypes;
  contents.lastFun = toQName("<tuple>", top.location);
  contents.expectedTypes =
      just(toTypeList(
              map(\ x::Integer ->
                    varType("X" ++ toString(genInt()),
                            location=top.location),
                  repeat(0, contents.len)), top.location));
  contents.downSubst = top.downSubst;
  top.upSubst = contents.upSubst;
  top.type = tupleType(contents.types, location=top.location);
}


abstract production funCall
top::Expr ::= fun::QName args::Args
{
  top.pp = fun.pp ++ "(" ++ args.pp ++ ")";

  top.type =
      if fun.functionFound
      then fun.functionRetType
      else errorType(location=top.location);
  args.expectedTypes =
      if fun.functionFound
      then just(fun.functionArgTypes)
      else nothing();
  args.lastFun = fun;
  args.downVarTypes = top.downVarTypes;

  args.downSubst = top.downSubst;
  top.upSubst = args.upSubst;

  top.errors <- fun.functionErrors;
}


abstract production trueExpr
top::Expr ::=
{
  top.pp = "true";

  top.upSubst = top.downSubst;
  top.type = boolType(location=top.location);
}


abstract production falseExpr
top::Expr ::=
{
  top.pp = "false";

  top.upSubst = top.downSubst;
  top.type = boolType(location=top.location);
}


abstract production listIndexExpr
top::Expr ::= l::Expr i::Expr
{
  top.pp = "(" ++ l.pp  ++ ")[" ++ i.pp ++ "]";

  local varty::Type =
      varType("X" ++ toString(genInt()), location=top.location);
  local unifyList::TypeUnify =
      typeUnify(listType(varty, location=top.location), l.type,
                location=top.location);
  local unifyInt::TypeUnify =
      typeUnify(intType(location=top.location), i.type,
                location=top.location);
  top.type = varty;

  l.downSubst = top.downSubst;
  i.downSubst = l.upSubst;
  unifyList.downSubst = i.upSubst;
  unifyInt.downSubst = unifyList.upSubst;
  top.upSubst = unifyInt.upSubst;

  l.downVarTypes = top.downVarTypes;
  i.downVarTypes = top.downVarTypes;
}





nonterminal Args with
   pp,
   len,
   types, upSubst, downSubst, finalSubst,
   downVarTypes,
   tyEnv, constructorEnv, judgmentEnv, concreteEnv, translationEnv,
   moduleName,
   funEnv, expectedTypes, lastFun,
   errors,
   location;
propagate errors, tyEnv, constructorEnv, judgmentEnv, concreteEnv,
          translationEnv, funEnv, lastFun, moduleName, finalSubst
   on Args;

abstract production nilArgs
top::Args ::=
{
  top.pp = "";

  top.len = 0;

  top.upSubst = top.downSubst;
  top.types = nilTypeList(location=top.location);

  top.errors <-
      case top.expectedTypes of
      | nothing() -> []
      | just(nilTypeList()) -> []
      | just(l) ->
        [errorMessage("Too few arguments to " ++ top.lastFun.pp,
                      location=top.location)]
      end;
}


abstract production consArgs
top::Args ::= e::Expr rest::Args
{
  top.pp = e.pp ++ if rest.pp == "" then "" else ", " ++ rest.pp;

  top.len = rest.len + 1;

  e.downVarTypes = top.downVarTypes;
  rest.downVarTypes = top.downVarTypes;

  top.types = consTypeList(e.type, nilTypeList(location=top.location),
                           location=top.location);

  local unify::TypeUnify =
      case top.expectedTypes of
      | just(consTypeList(ty, _)) ->
        typeUnify(e.type, ty, location=e.location)
      | _ -> blankUnify(location=e.location)
      end;
  e.downSubst = top.downSubst;
  rest.downSubst = e.upSubst;
  unify.downSubst = rest.upSubst;
  top.upSubst = unify.upSubst;

  rest.expectedTypes =
      case top.expectedTypes of
      | just(consTypeList(_, l)) -> just(l)
      | _ -> nothing()
      end;
  top.errors <-
      case top.expectedTypes of
      | just(nilTypeList()) ->
        [errorMessage("Too many arguments to " ++ top.lastFun.pp,
                      location=top.location)]
      | _ -> []
      end;
}
