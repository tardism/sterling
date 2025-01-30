grammar sos:translation:semantic:latex;


attribute latex<LaTeXJudgment> occurs on Judgment;

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  top.latex = termLaTeXJudgment(macroLaTeXTerm(rel.latexRel, args.latex));
}


aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.latex = negationLaTeXJudgment(macroLaTeXTerm(rel.latexRel, args.latex));
}


aspect production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.latex = projLaTeXJudgment(ty.latexProj, args.latex, t.latex,
                                projection.latex);
}


aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.latex = binOpLaTeXJudgment(t1.latex, ^op, t2.latex, result.latex);
}


aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.latex = topBinOpLaTeXJudgment(t1.latex, ^op, t2.latex);
}




attribute ppLaTeX occurs on BinOp;

aspect production plusOp
top::BinOp ::=
{
  top.ppLaTeX = "+";
}


aspect production minusOp
top::BinOp ::=
{
  top.ppLaTeX = "-";
}


aspect production multOp
top::BinOp ::=
{
  top.ppLaTeX = "\\ensuremath{\\times}";
}


aspect production divOp
top::BinOp ::=
{
  top.ppLaTeX = "/";
}


aspect production modOp
top::BinOp ::=
{
  top.ppLaTeX = "\\textnormal{mod}";
}


aspect production appendOp
top::BinOp ::=
{
  top.ppLaTeX = "+";
}




attribute ppLaTeX occurs on TopBinOp;

aspect production eqOp
top::TopBinOp ::=
{
  top.ppLaTeX = "=";
}


aspect production neqOp
top::TopBinOp ::=
{
  top.ppLaTeX = "\\ensuremath{\\neq}";
}


aspect production lessOp
top::TopBinOp ::=
{
  top.ppLaTeX = "\\ensuremath{<}";
}


aspect production greaterOp
top::TopBinOp ::=
{
  top.ppLaTeX = "\\ensuremath{>}";
}


aspect production leqOp
top::TopBinOp ::=
{
  top.ppLaTeX = "\\ensuremath{\\leq}";
}


aspect production geqOp
top::TopBinOp ::=
{
  top.ppLaTeX = "\\ensuremath{\\geq}";
}
