grammar sos:core:main:abstractSyntax;

nonterminal Stmt with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   funEnv,
   downVarTypes, upVarTypes,
   moduleName,
   errors,
   location;
propagate errors, judgmentEnv, translationEnv, concreteEnv, tyEnv,
          constructorEnv, funEnv on Stmt;

abstract production noop
top::Stmt ::=
{
  top.pp = "";

  top.upVarTypes = top.downVarTypes;
}


abstract production branchStmt
top::Stmt ::= s1::Stmt s2::Stmt
{
  top.pp = s1.pp ++ s2.pp;

  s1.downVarTypes = top.downVarTypes;
  s2.downVarTypes = s1.upVarTypes;
  top.upVarTypes = s2.upVarTypes;
}


abstract production parseStmt
top::Stmt ::= p::Parse
{
  top.pp = p.pp;
}


abstract production deriveRelStmt
top::Stmt ::= d::DeriveRelation
{
  top.pp = d.pp;
}


abstract production assignStmt
top::Stmt ::= name::String e::Expr
{
  top.pp = name ++ " := " ++ e.pp ++ "\n";

  e.downVarTypes = top.downVarTypes;
  top.upVarTypes = (name, e.type)::top.downVarTypes;
}


abstract production whileStmt
top::Stmt ::= cond::Expr body::Stmt
{
  top.pp = "While " ++ cond.pp ++ " Do " ++ body.pp ++ " End\n";

  cond.downVarTypes = top.downVarTypes;
  body.downVarTypes = top.downVarTypes;
  top.upVarTypes = top.downVarTypes;

  top.errors <-
      if cond.type == boolType(location=bogusLoc())
      then []
      else [errorMessage("Condition must have type bool; found " ++
                         cond.type.pp, location=top.location)];
}


abstract production ifStmt
top::Stmt ::= cond::Expr th::Stmt el::Stmt
{
  top.pp = "If " ++ cond.pp ++ " Then\n" ++ th.pp ++ " Else\n" ++
           el.pp ++ " End\n";

  cond.downVarTypes = top.downVarTypes;
  th.downVarTypes = top.downVarTypes;
  el.downVarTypes = top.downVarTypes;
  top.upVarTypes = top.downVarTypes;

  top.errors <-
      if cond.type == boolType(location=bogusLoc())
      then []
      else [errorMessage("Condition must have type bool; found " ++
                         cond.type.pp, location=top.location)];
}


abstract production returnStmt
top::Stmt ::= e::Expr
{
  top.pp = "Return " ++ e.pp ++ "\n";

  e.downVarTypes = top.downVarTypes;
}


abstract production printStmt
top::Stmt ::= e::Expr
{
  top.pp = "Print " ++ e.pp ++ "\n";

  e.downVarTypes = top.downVarTypes;
  top.upVarTypes = top.downVarTypes;
}


abstract production writeStmt
top::Stmt ::= e::Expr file::Expr
{
  top.pp = "Write " ++ e.pp ++ " to " ++ file.pp ++ "\n";

  e.downVarTypes = top.downVarTypes;
  file.downVarTypes = top.downVarTypes;
  top.upVarTypes = top.downVarTypes;

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


abstract production readStmt
top::Stmt ::= e::Expr var::String
{
  top.pp = "Read " ++ e.pp ++ " to " ++ var ++ "\n";

  e.downVarTypes = top.downVarTypes;
  top.upVarTypes = top.downVarTypes;

  top.errors <-
      if e.type == stringType(location=bogusLoc())
      then []
      else [errorMessage("Filenames being read must be strings, " ++
               "not " ++ e.type.pp, location=top.location)];
}





nonterminal Parse with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   type,
   errors,
   location;
propagate errors on Parse;

--nt is concrete nonterminal name
--varName is name to which we assign the parse result
--parseString is an object-level string to parse
abstract production parse
top::Parse ::= nt::QName varName::String parseString::Expr
{
  top.pp = "Parse " ++ nt.pp ++ " as " ++ varName ++ " from " ++
           parseString.pp ++ "\n";

  nt.concreteEnv = top.concreteEnv;
  top.errors <-
      if !nt.concreteFound
      then [errorMessage("Unknown concrete nonterminal " ++ nt.pp,
            location=nt.location)]
      else if !nt.isConcreteNt
      then [errorMessage(nt.pp ++ " is not a concrete nonterminal " ++
               "but must be one to be parsed", location=nt.location)]
      else [];
  top.type = nt.concreteType;
}





nonterminal DeriveRelation with
   pp,
   judgmentEnv, translationEnv, concreteEnv, tyEnv, constructorEnv,
   moduleName,
   errors,
   location;
propagate errors on DeriveRelation;

--resultVar is the variable into which we place the success/failure of
--deriving the relation
abstract production deriveRelation
top::DeriveRelation ::= resultVar::String j::Judgment
{
  top.pp = resultVar ++ " := " ++ j.pp;
}
