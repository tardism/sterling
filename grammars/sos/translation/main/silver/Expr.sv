grammar sos:translation:main:silver;

attribute
   ctxExpr,
   --name for accessing IO coming in
   precedingName,
   --name for accessing IO coming out
   thisName,
   stmtDefs,
   silverExpr, containsIO
occurs on Expr;
propagate ctxExpr on Expr;

--how to access the current eval ctx for the expr
inherited attribute ctxExpr::String;

--text of an equivalent Silver expression
synthesized attribute silverExpr::String;

--contains a call to a function with an IO type
synthesized attribute containsIO::Boolean;

aspect production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") || (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production andExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") && (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production ltExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") < (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production gtExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") > (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production leExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") <= (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production geExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") >= (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production eqExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") == (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") + (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production minusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") - (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production multExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") * (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production divExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") / (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production modExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") % (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production appendExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.silverExpr =
      "(" ++ e1.silverExpr ++ ") ++ (" ++ e2.silverExpr ++ ")";

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.precedingName;

  top.stmtDefs = e1.stmtDefs ++ e2.stmtDefs;
}


aspect production varExpr
top::Expr ::= name::String
{
  top.silverExpr =
      "case lookup(name, " ++ top.ctxExpr ++ ") of " ++
      "| just(x) -> x " ++
      "| nothing() -> error(\"" ++ name ++ " not defined\") " ++
      "end";

  top.containsIO = false;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production intExpr
top::Expr ::= i::Integer
{
  top.silverExpr = toString(i);

  top.containsIO = false;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production stringExpr
top::Expr ::= s::String
{
  top.silverExpr = "\"" ++ s ++ "\"";

  top.containsIO = false;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production funCall
top::Expr ::= fun::QName args::Args
{
  local funName::String = fun.fullFunction.name.silverFunName;

  top.containsIO = true;
}


aspect production trueExpr
top::Expr ::=
{
  top.silverExpr = "true";

  top.containsIO = false;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production falseExpr
top::Expr ::=
{
  top.silverExpr = "false";

  top.containsIO = false;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production listIndexExpr
top::Expr ::= l::Expr i::Expr
{
  top.containsIO = l.containsIO || i.containsIO;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}





attribute
   ctxExpr,
   --name for IO before evaluating current
   precedingName,
   --name for IO after evaluating current
   thisName,
   stmtDefs,
   silverArgs, containsIO
occurs on Args;
propagate ctxExpr on Args;

synthesized attribute silverArgs::String;

aspect production nilArgs
top::Args ::=
{
  top.silverArgs = "";

  top.containsIO = false;

  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production consArgs
top::Args ::= e::Expr rest::Args
{
  top.silverArgs =
      e.silverArgs ++ if rest.silverArgs == ""
                      then "" else ", " ++ rest.silverArgs;

  top.containsIO = e.containsIO || rest.containsIO;

  e.precedingName = top.precedingName;
  rest.precedingName = e.thisName;
  top.thisName = rest.thisName;

  top.stmtDefs = e.stmtDefs ++ rest.stmtDefs;
}
