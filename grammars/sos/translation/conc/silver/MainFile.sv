grammar sos:translation:conc:silver;

import sos:core:main:abstractSyntax;
import sos:core:semanticDefs:abstractSyntax only Judgment;

--types that we need to parse in the end
synthesized attribute parsedTypes::[QName] occurs on
   MainFile, MainDecls, FunDecl, Expr, Args;


aspect production mainFile
top::MainFile ::= moduleName::QName contents::MainDecls
{
  top.parsedTypes = contents.parsedTypes;
}





aspect production emptyMainDecls
top::MainDecls ::=
{
  top.parsedTypes = [];
}


aspect production branchMainDecls
top::MainDecls ::= d1::MainDecls d2::MainDecls
{
  top.parsedTypes = d1.parsedTypes ++ d2.parsedTypes;
}


aspect production funMainDecl
top::MainDecls ::= f::FunDecl
{
  top.parsedTypes = f.parsedTypes;
}





aspect production funDecl
top::FunDecl ::= name::String params::Params retTy::Type body::Expr
{
  top.parsedTypes = body.parsedTypes;
}





aspect production letExpr
top::Expr ::= names::[String] e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production seqExpr
top::Expr ::= a::Expr b::Expr
{
  top.parsedTypes = a.parsedTypes ++ b.parsedTypes;
}


aspect production ifExpr
top::Expr ::= cond::Expr th::Expr el::Expr
{
  top.parsedTypes =
      cond.parsedTypes ++ th.parsedTypes ++ el.parsedTypes;
}


aspect production printExpr
top::Expr ::= e::Expr
{
  top.parsedTypes = e.parsedTypes;
}


aspect production writeExpr
top::Expr ::= e::Expr file::Expr
{
  top.parsedTypes = e.parsedTypes ++ file.parsedTypes;
}


aspect production readExpr
top::Expr ::= file::Expr
{
  top.parsedTypes = file.parsedTypes;
}


--vars are the bindings we want out of the judgment
aspect production deriveExpr
top::Expr ::= j::Judgment useVars::[String] vars::[String]
{
  top.parsedTypes = [];
}


--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
aspect production parseExpr
top::Expr ::= nt::QName parseString::Expr
{
  top.parsedTypes =
      if nt.concreteFound
      then [nt.fullConcreteName]
      else error("Concrete NT not found; should not access");
}


aspect production orExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production andExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production ltExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production gtExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production leExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production geExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production eqExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production plusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production minusExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production multExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production divExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production modExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production appendExpr
top::Expr ::= e1::Expr e2::Expr
{
  top.parsedTypes = e1.parsedTypes ++ e2.parsedTypes;
}


aspect production varExpr
top::Expr ::= name::String
{
  top.parsedTypes = [];
}


aspect production intExpr
top::Expr ::= i::Integer
{
  top.parsedTypes = [];
}


aspect production stringExpr
top::Expr ::= s::String
{
  top.parsedTypes = [];
}


aspect production tupleExpr
top::Expr ::= contents::Args
{
  top.parsedTypes = contents.parsedTypes;
}


aspect production funCall
top::Expr ::= fun::QName args::Args
{
  top.parsedTypes = args.parsedTypes;
}


aspect production trueExpr
top::Expr ::=
{
  top.parsedTypes = [];
}


aspect production falseExpr
top::Expr ::=
{
  top.parsedTypes = [];
}


aspect production listIndexExpr
top::Expr ::= l::Expr i::Expr
{
  top.parsedTypes = l.parsedTypes ++ i.parsedTypes;
}





aspect production nilArgs
top::Args ::=
{
  top.parsedTypes = [];
}


aspect production consArgs
top::Args ::= e::Expr rest::Args
{
  top.parsedTypes = e.parsedTypes ++ rest.parsedTypes;
}
