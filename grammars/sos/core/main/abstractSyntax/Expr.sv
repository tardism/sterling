grammar sos:core:main:abstractSyntax;

nonterminal Expr with
   pp,
   type,
   funEnv, downVarTypes,
   errors,
   location;
propagate errors, funEnv, downVarTypes on Expr;

abstract production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") || (" ++ e2.pp ++ ")";

  top.type = boolType(location=top.location);

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


abstract production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") + (" ++ e2.pp ++ ")";

  top.type = intType(location=top.location);

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

  top.errors <- fun.functionErrors;
}


abstract production successExpr
top::Expr ::=
{
  top.pp = "?success";

  top.type = resultType(location=top.location);
}


abstract production failureExpr
top::Expr ::=
{
  top.pp = "?failure";

  top.type = resultType(location=top.location);
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





nonterminal Args with
   pp,
   types,
   funEnv, downVarTypes, expectedTypes, lastFun,
   errors,
   location;
propagate errors, funEnv, downVarTypes on Args;

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
