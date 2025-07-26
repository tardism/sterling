grammar sos:translation:semantic:ocaml;

attribute ocamlExpr occurs on Judgment;
attribute ocamlBinOp occurs on BinOp;
attribute ocamlTopBinOp occurs on TopBinOp;
attribute ocamlConjunction occurs on JudgmentList;

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar(rel.ocamlString), args.ocamlExprs);
}

aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar("not"), 
      [ocamlApplication(ocamlVar(rel.ocamlString), args.ocamlExprs)]);
}

aspect production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.ocamlExpr = 
    ocamlApplication(ocamlVar("project_" ++ ty.ocamlString), 
      args.ocamlExprs ++ [t.ocamlExpr, projection.ocamlExpr]);
}

aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.ocamlExpr = op.ocamlBinOp(t1.ocamlExpr, t2.ocamlExpr, result.ocamlExpr);
}

aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.ocamlExpr = op.ocamlTopBinOp(t1.ocamlExpr, t2.ocamlExpr);
}

aspect production plusOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlInfixOp(result, "=", ocamlInfixOp(t1, "+", t2));
}

aspect production minusOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlInfixOp(result, "=", ocamlInfixOp(t1, "-", t2));
}

aspect production multOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlInfixOp(result, "=", ocamlInfixOp(t1, "*", t2));
}

aspect production divOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlInfixOp(result, "=", ocamlInfixOp(t1, "/", t2));
}

aspect production modOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlInfixOp(result, "=", ocamlInfixOp(t1, "mod", t2));
}

aspect production appendOp
top::BinOp ::=
{
  top.ocamlBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr result::OCamlExpr ->
      ocamlInfixOp(result, "=", ocamlInfixOp(t1, "@", t2));
}

aspect production eqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "=", t2);
}

aspect production neqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "<>", t2);
}

aspect production lessOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "<", t2);
}

aspect production greaterOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, ">", t2);
}

aspect production leqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, "<=", t2);
}

aspect production geqOp
top::TopBinOp ::=
{
  top.ocamlTopBinOp = 
    \ t1::OCamlExpr t2::OCamlExpr ->
      ocamlInfixOp(t1, ">=", t2);
}

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.ocamlConjunction = \ conclusion::OCamlExpr -> conclusion;
}

aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.ocamlConjunction = 
    \ conclusion::OCamlExpr ->
      ocamlInfixOp(j.ocamlExpr, " -> ", rest.ocamlConjunction(conclusion));
}