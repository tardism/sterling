grammar sos:core:main:abstractSyntax;

nonterminal Expr with
   pp,
   type,
   tyEnv, constructorEnv, judgmentEnv, concreteEnv, funEnv,
   translationEnv, moduleName,
   downVarTypes,
   errors,
   location;
propagate errors, funEnv, tyEnv, constructorEnv, judgmentEnv,
          concreteEnv, translationEnv, moduleName on Expr;

abstract production letExpr
top::Expr ::= names::[String] e1::Expr e2::Expr
{
  top.pp = "Let " ++ implode(", ", names) ++ " := " ++ e1.pp ++
           " in " ++ e2.pp;

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes =
     case e1.type, names of
     | tupleType(tys), _ ->
       zipWith(pair, names, tys.toList) ++ top.downVarTypes
     | _, h::_ -> (h, e1.type)::top.downVarTypes
     | _, _ -> top.downVarTypes
     end;

  top.type = e2.type;

  top.errors <-
      case e1.type of
      | tupleType(tys) ->
        if tys.len == length(names)
        then []
        else [errorMessage("Expected " ++ toString(tys.len) ++
                 " name but found " ++ toString(length(names)),
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

  top.type = b.type;

  top.errors <-
      if a.type == unitType(location=bogusLoc())
      then []
      else [errorMessage("Before expected first expression to have" ++
               " type " ++ unitType(location=bogusLoc()).pp ++
               " but found " ++ a.type.pp, location=top.location)];
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

  top.errors <-
      if cond.type == boolType(location=bogusLoc())
      then []
      else [errorMessage("Condition must have type bool; found " ++
                         cond.type.pp, location=top.location)];
  top.errors <-
      if th.type == el.type
      then []
      else [errorMessage("Then and else branches must have same " ++
               "type; found " ++ th.type.pp ++ " and " ++ el.type.pp,
               location=top.location)];
}


abstract production printExpr
top::Expr ::= e::Expr
{
  top.pp = "Print " ++ e.pp;

  e.downVarTypes = top.downVarTypes;

  top.type = unitType(location=top.location);

  top.errors <-
      case e.type of
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

  top.errors <-
      if e.type == stringType(location=bogusLoc())
      then []
      else [errorMessage("Can only write strings to files, not " ++
                         e.type.pp, location=top.location)];
  top.errors <-
      if file.type == stringType(location=bogusLoc())
      then []
      else [errorMessage("Filenames being written must be strings," ++
               " not " ++ file.type.pp, location=top.location)];
}


abstract production readExpr
top::Expr ::= file::Expr
{
  top.pp = "Read " ++ file.pp;

  file.downVarTypes = top.downVarTypes;

  top.type = stringType(location=top.location);

  top.errors <-
      if file.type == stringType(location=bogusLoc())
      then []
      else [errorMessage("Filenames being read must be strings but" ++
               " found " ++ file.type.pp, location=top.location)];
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

  j.downVarTypes = top.downVarTypes;

  local lkpVarsTys::[(String, Maybe<Type>)] =
      map(\ v::String -> (v, lookup(v, j.upVarTypes)), vars);
  local varsTys::[Type] =
      map(\ p::(String, Maybe<Type>) ->
            case p.2 of
            | nothing() -> errorType(location=top.location)
            | just(t) -> t
            end,
          lkpVarsTys);
  top.type =
      if null(vars)
      then boolType(location=top.location)
      else tupleType(foldr(consTypeList(_, _, location=top.location),
                           nilTypeList(location=top.location),
                           boolType(location=top.location)::varsTys),
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
}


--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
abstract production parseExpr
top::Expr ::= nt::QName parseString::Expr
{
  top.pp = "Parse " ++ nt.pp ++ " from " ++ parseString.pp ++ "\n";

  parseString.downVarTypes = top.downVarTypes;

  top.type = --(bool, parsed AST)
      tupleType(
         consTypeList(boolType(location=top.location),
         consTypeList(if nt.concreteFound
                      then nt.concreteType
                      else errorType(location=top.location),
         nilTypeList(location=top.location),
                     location=top.location),
                     location=top.location),
         location=top.location);

  top.errors <-
      if !nt.concreteFound
      then [errorMessage("Unknown concrete nonterminal " ++ nt.pp,
            location=nt.location)]
      else if !nt.isConcreteNt
      then [errorMessage(nt.pp ++ " is not a concrete nonterminal " ++
               "but must be one to be parsed", location=nt.location)]
      else [];
  top.errors <-
      if parseString.type == stringType(location=bogusLoc())
      then []
      else [errorMessage("Expression being parsed must be of type " ++
               "string but found " ++ parseString.type.pp,
               location=top.location)];
}


abstract production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") || (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | boolType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Or (||) expected argument 1 to have type " ++
            "bool but found type " ++ t.pp, location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | boolType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Or (||) expected argument 2 to have type " ++
            "bool but found type " ++ t.pp, location=top.location)]
      end;
}


abstract production andExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") && (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | boolType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("And (&&) expected argument 1 to have type " ++
            "bool but found type " ++ t.pp, location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | boolType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("And (&&) expected argument 2 to have type " ++
            "bool but found type " ++ t.pp, location=top.location)]
      end;
}


abstract production ltExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") < (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Less than (<) expected argument 1 to have " ++
            "type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Less than (<) expected argument 2 to have " ++
            "type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production gtExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") > (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Greater than (>) expected argument 1 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Greater than (>) expected argument 2 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production leExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") <= (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Less than or equal (<=) expected argument " ++
            "1 to have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Less than or equal (<=) expected argument " ++
            "2 to have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production geExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") >= (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Greater than or equal (>=) expected argument" ++
            " 1 to have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Greater than or equal (>=) expected argument" ++
            " 2 to have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production eqExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") = (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      if e1.type == e2.type
      then []
      else [errorMessage("Checking equality requires both sides " ++
               "to have the same type; found " ++ e1.type.pp ++
               " and " ++ e2.type.pp, location=top.location)];
}


abstract production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") + (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Plus (+) expected argument 1 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Plus (+) expected argument 2 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production minusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") - (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Minus (-) expected argument 1 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Minus (-) expected argument 2 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production multExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") * (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Multiplication (*) expected argument 1 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Multiplication (*) expected argument 2 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production divExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") / (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Division (/) expected argument 1 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Division (/) expected argument 2 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production modExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") % (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Modulus (%) expected argument 1 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | intType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Modulus (%) expected argument 2 to " ++
            "have type int but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production appendExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") ++ (" ++ e2.pp ++ ")";

  top.type = stringType(location=bogusLoc());

  e1.downVarTypes = top.downVarTypes;
  e2.downVarTypes = top.downVarTypes;

  top.errors <-
      case e1.type of
      | stringType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Append (++) expected argument 1 to " ++
            "have type string but found type " ++ t.pp,
            location=top.location)]
      end;
  top.errors <-
      case e2.type of
      | stringType() -> []
      | errorType() -> []
      | t ->
        [errorMessage("Append (++) expected argument 2 to " ++
            "have type string but found type " ++ t.pp,
            location=top.location)]
      end;
}


abstract production varExpr
top::Expr ::= name::String
{
  top.pp = name;

  local lkp::Maybe<Type> = lookup(name, top.downVarTypes);
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

  top.type = intType(location=top.location);
}


abstract production stringExpr
top::Expr ::= s::String
{
  top.pp = "\"" ++ s ++ "\"";

  top.type = stringType(location=top.location);
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

  top.errors <- fun.functionErrors;
}


abstract production trueExpr
top::Expr ::=
{
  top.pp = "true";

  top.type = boolType(location=top.location);
}


abstract production falseExpr
top::Expr ::=
{
  top.pp = "false";

  top.type = boolType(location=top.location);
}


abstract production listIndexExpr
top::Expr ::= l::Expr i::Expr
{
  top.pp = "(" ++ l.pp  ++ ")[" ++ i.pp ++ "]";

  top.type =
      case l.type of
      | listType(ty) -> ty
      | _ -> errorType(location=top.location)
      end;

  l.downVarTypes = top.downVarTypes;
  i.downVarTypes = top.downVarTypes;

  top.errors <-
      case l.type of
      | listType(_) -> []
      | errorType() -> []
      | _ ->
        [errorMessage("Can only index list, not " ++ l.type.pp,
                      location=top.location)]
      end;
  top.errors <-
      if i.type == intType(location=bogusLoc())
      then []
      else [errorMessage("Can only index list using int, not " ++
                         i.type.pp, location=top.location)];
}





nonterminal Args with
   pp,
   types,
   tyEnv, constructorEnv, judgmentEnv, concreteEnv, translationEnv,
   moduleName,
   funEnv, downVarTypes, expectedTypes, lastFun,
   errors,
   location;
propagate errors, tyEnv, constructorEnv, judgmentEnv, concreteEnv,
          translationEnv, funEnv, downVarTypes, lastFun, moduleName
   on Args;

abstract production nilArgs
top::Args ::=
{
  top.pp = "";

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

  top.types = consTypeList(e.type, nilTypeList(location=top.location),
                           location=top.location);

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
      | just(consTypeList(ty, _)) when ty != e.type ->
        [errorMessage("Expected argument type " ++ ty.pp ++ " but " ++
            "found type " ++ e.type.pp, location=top.location)]
      | _ -> []
      end;
}
