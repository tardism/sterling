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

--name of previous action to access .io/.iovalue
inherited attribute precedingName::String;
--name of the current action being produced
synthesized attribute thisName::String;
--actual definitions
synthesized attribute stmtDefs::[StmtDef];

aspect production letExpr
top::Expr ::= names::[String] e1::Expr e2::Expr
{
  top.silverExpr = error("letExpr.silverExpr");

  top.containsIO = e1.containsIO || e2.containsIO;

  e1.precedingName = top.precedingName;
  e2.precedingName = e1.thisName;
  top.thisName = e2.thisName;

  top.stmtDefs = error("letExpr.stmtDefs");
}


aspect production seqExpr
top::Expr ::= a::Expr b::Expr
{
  top.silverExpr = error("seqExpr.silverExpr");

  top.containsIO = a.containsIO || b.containsIO;

  a.precedingName = top.precedingName;
  b.precedingName = a.thisName;
  top.thisName = b.thisName;

  top.stmtDefs = a.stmtDefs ++ b.stmtDefs;
}


aspect production ifExpr
top::Expr ::= cond::Expr th::Expr el::Expr
{
  top.silverExpr =
      "if " ++ cond.silverExpr ++ " then " ++ th.silverExpr ++
      " else " ++ el.silverExpr;

  top.containsIO = cond.containsIO || th.containsIO || el.containsIO;

  cond.precedingName = top.precedingName;
  th.precedingName = cond.thisName;
  el.precedingName = cond.thisName;
  top.thisName = error("ifExpr.thisName");

  top.stmtDefs = error("ifExpr.stmtDefs");
}


aspect production printExpr
top::Expr ::= e::Expr
{
  top.silverExpr = error("printExpr.silverExpr");

  top.containsIO = true;

  e.precedingName = top.precedingName;
  top.thisName = error("printExpr.thisName");

  top.stmtDefs = error("printExpr.stmtDefs");
}


aspect production writeExpr
top::Expr ::= e::Expr file::Expr
{
  top.silverExpr = error("writeExpr.silverExpr");

  top.containsIO = true;

  e.precedingName = top.precedingName;
  file.precedingName = e.thisName;
  top.thisName = error("writeExpr.thisName");

  top.stmtDefs = error("writeExpr.stmtDefs");
}


aspect production readExpr
top::Expr ::= file::Expr
{
  top.silverExpr = error("readExpr.silverExpr");

  top.containsIO = true;

  file.precedingName = top.precedingName;
  top.thisName = error("readExpr.thisName");

  top.stmtDefs = error("readExpr.stmtDefs");
}


--vars are the bindings we want out of the judgment
aspect production deriveExpr
top::Expr ::= j::Judgment vars::[String]
{
  top.silverExpr = error("deriveExpr.silverExpr");

  top.containsIO = true;

  top.thisName = error("deriveExpr.thisName");

  top.stmtDefs = error("deriveExpr.stmtDefs");
}


--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
aspect production parseExpr
top::Expr ::= nt::QName parseString::Expr
{
  top.silverExpr = error("parseExpr.silverExpr");

  top.containsIO = true;

  parseString.precedingName = top.precedingName;
  top.thisName = error("parseExpr.thisName");

  top.stmtDefs = error("parseExpr.stmtDefs");
}

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
  top.silverExpr = error("funCall.silverExpr");

  top.containsIO = true;

  args.precedingName = top.precedingName;
  top.thisName = error("top.thisName");

  top.stmtDefs = error("funCall.stmtDefs");
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
  top.silverExpr = error("listIndexExpr.silverExpr");

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
      e.silverExpr ++ if rest.silverArgs == ""
                      then "" else ", " ++ rest.silverArgs;

  top.containsIO = e.containsIO || rest.containsIO;

  e.precedingName = top.precedingName;
  rest.precedingName = e.thisName;
  top.thisName = rest.thisName;

  top.stmtDefs = e.stmtDefs ++ rest.stmtDefs;
}
