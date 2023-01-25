grammar sos:translation:main:silver;

{-

  Each statement (except noop and branch) is a local with type
  IOVal<[(String, Value)]>.

-}

attribute
   precedingName, thisName, stmtDefs
occurs on Stmt;

--name of previous action to access .io/.iovalue
inherited attribute precedingName::String;
--name of the current action being produced
synthesized attribute thisName::String;
--actual definitions
synthesized attribute stmtDefs::[StmtDef];

aspect production noop
top::Stmt ::=
{
  top.thisName = top.precedingName;

  top.stmtDefs = [];
}


aspect production branchStmt
top::Stmt ::= s1::Stmt s2::Stmt
{
  s1.precedingName = top.precedingName;
  s2.precedingName = s1.thisName;
  top.thisName = s2.thisName;

  top.stmtDefs = s1.stmtDefs ++ s2.stmtDefs;
}


aspect production parseStmt
top::Stmt ::= p::Parse
{
  p.precedingName = top.precedingName;
  top.thisName = p.thisName;

  top.stmtDefs = p.stmtDefs;
}


aspect production deriveRelStmt
top::Stmt ::= d::DeriveRelation
{
  d.precedingName = top.precedingName;
  top.thisName = d.thisName;

  top.stmtDefs = d.stmtDefs;
}


aspect production assignStmt
top::Stmt ::= name::String e::Expr
{
  top.thisName = "assign_" ++ toString(genInt());

  top.stmtDefs =
      [stmtDef(top.thisName,
          buildIOVal(top.precedingName ++ ".io",
             addAssigns([(name, e.silverExpr)],
                buildPrecedingCtx(top.precedingName))))];
}


aspect production whileStmt
top::Stmt ::= cond::Expr body::Stmt
{
  top.thisName = "while_" ++ toString(genInt());
}


aspect production ifStmt
top::Stmt ::= cond::Expr th::Stmt el::Stmt
{
  top.thisName = "if_" ++ toString(genInt());

  top.stmtDefs =
      th.stmtDefs ++ el.stmtDefs ++
      [stmtDef(top.thisName,
          "if " ++ cond.silverExpr ++ " then " ++ th.thisName ++
                                      " else " ++ el.thisName)];
}


aspect production returnStmt
top::Stmt ::= e::Expr
{
  top.thisName = "return_" ++ toString(genInt());
}


aspect production printStmt
top::Stmt ::= e::Expr
{
  top.thisName = "print_" ++ toString(genInt());

  local printIt::String =
      "printT(" ++ e.silverExpr ++ ", " ++
                   top.precedingName ++ ".io)";
  top.stmtDefs =
      [stmtDef(top.thisName,
          buildIOVal(printIt, buildPrecedingCtx(top.precedingName)))];
}


aspect production writeStmt
top::Stmt ::= e::Expr file::Expr
{
  top.thisName = "write_" ++ toString(genInt());

  local writeIt::String =
      "writeT(" ++ file.silverExpr ++ ", " ++
                   e.silverExpr ++ ", " ++
                   top.precedingName ++ ".io)";
  top.stmtDefs =
      [stmtDef(top.thisName,
          buildIOVal(writeIt, buildPrecedingCtx(top.precedingName)))];
}


aspect production readStmt
top::Stmt ::= e::Expr var::String
{
  top.thisName = "read_" ++ num;

  local num::String = toString(genInt());
  local readName::String = "read_helper_" ++ num;
  local readIt::String =
      "readFileT(" ++ e.silverExpr ++ ", " ++
                      top.precedingName ++ ".io)";
  top.stmtDefs =
      [stmtTypedDef(readName, "IOVal<String>", readIt),
       stmtDef(top.thisName,
          buildIOVal(readName ++ ".io",
             addAssigns([(var, readName ++ ".iovalue")],
                buildPrecedingCtx(top.precedingName))))];
}





attribute
   precedingName, thisName, stmtDefs
occurs on Parse;

aspect production parse
top::Parse ::= resultVar::String nt::QName varName::String
               parseString::Expr
{
  top.thisName = "parse_" ++ toString(genInt());
}





attribute
   precedingName, thisName, stmtDefs
occurs on DeriveRelation

aspect production deriveRelation
top::DeriveRelation ::= resultVar::String j::Judgement
{
  top.thisName = "derive_" ++ toString(genInt());
}
