grammar sos:core:main:abstractSyntax;

nonterminal Stmt with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   downVarTypes, upVarTypes,
   moduleName,
   errors,
   location;
propagate errors on Stmt;

abstract production noop
top::Stmt ::=
{
  top.pp = "";
}


abstract production branchStmt
top::Stmt ::= s1::Stmt s2::Stmt
{
  top.pp = s1.pp ++ s2.pp;
}


abstract production parseStmt
top::Stmt ::= p::Parse
{
  top.pp = p.pp;

  p.judgmentEnv = top.judgmentEnv;
  p.translationEnv = top.translationEnv;
  p.concreteEnv = top.concreteEnv;
  p.tyEnv = top.tyEnv;
  p.constructorEnv = top.constructorEnv;
}


abstract production deriveRelStmt
top::Stmt ::= d::DeriveRelation
{
  top.pp = d.pp;

  d.judgmentEnv = top.judgmentEnv;
  d.translationEnv = top.translationEnv;
  d.concreteEnv = top.concreteEnv;
  d.tyEnv = top.tyEnv;
  d.constructorEnv = top.constructorEnv;
}


abstract production assignStmt
top::Stmt ::= name::String e::Expr
{
  top.pp = name ++ " := " ++ e.pp ++ "\n";
}


abstract production whileStmt
top::Stmt ::= cond::Expr body::Stmt
{
  top.pp = "While " ++ cond.pp ++ " Do " ++ body.pp ++ " End\n";
}


abstract production ifStmt
top::Stmt ::= cond::Expr th::Stmt el::Stmt
{
  top.pp = "If " ++ cond.pp ++ " Then\n" ++ th.pp ++ " Else\n" ++
           el.pp ++ " End\n";
}


abstract production returnStmt
top::Stmt ::= e::Expr
{
  top.pp = "Return " ++ e.pp ++ "\n";
}


abstract production printStmt
top::Stmt ::= e::Expr
{
  top.pp = "Print " ++ e.pp ++ "\n";
}


abstract production writeStmt
top::Stmt ::= e::Expr file::Expr
{
  top.pp = "Write " ++ e.pp ++ " to " ++ file.pp ++ "\n";
}


abstract production readStmt
top::Stmt ::= e::Expr var::String
{
  top.pp = "Read " ++ e.pp ++ " to " ++ var ++ "\n";
}
