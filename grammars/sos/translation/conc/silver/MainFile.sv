grammar sos:translation:conc:silver;

import sos:core:main:abstractSyntax;

--types that we need to parse in the end
synthesized attribute parsedTypes::[QName] occurs on
   MainFile, MainDecls, FunDecl, Stmt, Parse;


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
top::FunDecl ::= name::String params::Params retTy::Type body::Stmt
{
  top.parsedTypes = body.parsedTypes;
}





aspect production noop
top::Stmt ::=
{
  top.parsedTypes = [];
}


aspect production branchStmt
top::Stmt ::= s1::Stmt s2::Stmt
{
  top.parsedTypes = s1.parsedTypes ++ s2.parsedTypes;
}


aspect production parseStmt
top::Stmt ::= p::Parse
{
  top.parsedTypes = p.parsedTypes;
}


aspect production deriveRelStmt
top::Stmt ::= d::DeriveRelation
{
  top.parsedTypes = [];
}


aspect production assignStmt
top::Stmt ::= name::String e::Expr
{
  top.parsedTypes = [];
}


aspect production whileStmt
top::Stmt ::= cond::Expr body::Stmt
{
  top.parsedTypes = [];
}


aspect production ifStmt
top::Stmt ::= cond::Expr th::Stmt el::Stmt
{
  top.parsedTypes = th.parsedTypes ++ el.parsedTypes;
}


aspect production returnStmt
top::Stmt ::= e::Expr
{
  top.parsedTypes = [];
}


aspect production printStmt
top::Stmt ::= e::Expr
{
  top.parsedTypes = [];
}


aspect production writeStmt
top::Stmt ::= e::Expr file::Expr
{
  top.parsedTypes = [];
}


aspect production readStmt
top::Stmt ::= e::Expr var::String
{
  top.parsedTypes = [];
}





aspect production parse
top::Parse ::= result::String nt::QName varName::String
               parseString::Expr
{
  top.parsedTypes =
      if nt.concreteFound
      then [nt.fullConcreteName]
      else error("Concrete NT not found; should not access");
}
