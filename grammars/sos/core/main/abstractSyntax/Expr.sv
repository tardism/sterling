grammar sos:core:main:abstractSyntax;

nonterminal Expr with pp, errors, location;
propagate errors on Expr;

abstract production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") || (" ++ e2.pp ++ ")";
}


abstract production andExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") && (" ++ e2.pp ++ ")";
}


abstract production ltExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") < (" ++ e2.pp ++ ")";
}


abstract production gtExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") > (" ++ e2.pp ++ ")";
}


abstract production leExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") <= (" ++ e2.pp ++ ")";
}


abstract production geExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") >= (" ++ e2.pp ++ ")";
}


abstract production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") + (" ++ e2.pp ++ ")";
}


abstract production minusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") - (" ++ e2.pp ++ ")";
}


abstract production multExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") * (" ++ e2.pp ++ ")";
}


abstract production divExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") / (" ++ e2.pp ++ ")";
}


abstract production modExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.pp = "(" ++ e1.pp ++ ") % (" ++ e2.pp ++ ")";
}


abstract production varExpr
top::Expr ::= name::String
{
  top.pp = name;
}


abstract production intExpr
top::Expr ::= i::Integer
{
  top.pp = toString(i);
}


abstract production funCall
top::Expr ::= fun::String args::Args
{
  top.pp = fun ++ "(" ++ args.pp ++ ")";
}


abstract production successExpr
top::Expr ::=
{
  top.pp = "?success";
}


abstract production failureExpr
top::Expr ::=
{
  top.pp = "?failure";
}





nonterminal Args with pp, errors, location;
propagate errors on Args;

abstract production nilArgs
top::Args ::=
{
  top.pp = "";
}


abstract production branchArgs
top::Args ::= a1::Args a2::Args
{
  top.pp = a1.pp ++ (if a2.pp == "" then "" else ", " ++ a2.pp);
}


abstract production oneArgs
top::Args ::= e::Expr
{
  top.pp = e.pp;
}
