grammar sos:translation:semantic:prolog;


attribute prolog<PrologFormula>, prologTerm, pcVar occurs on Judgment;

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  top.prolog = termPrologFormula(top.prologTerm);
  top.prologTerm =
      applicationTerm(
         if rel.isQualified
         then rel.prolog
         else rel.fullJudgment.name.prolog, args.prolog);
  top.pcVar = args.pcVar;
}


aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.prolog =
      negatePrologFormula(
         termPrologFormula(
            applicationTerm(if rel.isQualified
                            then rel.prolog
                            else rel.fullJudgment.name.prolog,
                            args.prolog)));
  top.prologTerm = error("No Prolog term for negationRelation");
  top.pcVar = args.pcVar;
}


aspect production projJudgment
top::Judgment ::= args::TermList ty::QName t::Term projection::Term
{
  top.prolog = termPrologFormula(top.prologTerm);
  top.prologTerm =
      applicationTerm(
         "projection___" ++ if ty.isQualified
                            then ty.prolog
                            else ty.fullTy.prolog,
         consPrologTermList(t.prolog,
         consPrologTermList(projection.prolog, args.prolog)));
  top.pcVar = error("Cannot access pcVar on projJudgment");
}


aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.prolog = op.prolog(t1.prolog, t2.prolog, result.prolog);
  top.prologTerm = error("No Prolog term for binOpJudgment");
  top.pcVar = error("Cannot access pcVar on binOpJudgment");
}


aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.prolog = binOpPrologFormula(t1.prolog, op.prolog, t2.prolog);
  top.prologTerm = error("No Prolog term for topBinOpJudgment");
  top.pcVar = error("Cannot access pcVar on topBinOpJudgment");
}




                                  --t1         t2         result
attribute prolog<(PrologFormula ::= PrologTerm PrologTerm PrologTerm)>
   occurs on BinOp;

aspect production plusOp
top::BinOp ::=
{
  top.prolog =
      \ t1::PrologTerm t2::PrologTerm result::PrologTerm ->
        isPrologFormula(t1, plusPrologIsBinOp(), t2, result);
}


aspect production minusOp
top::BinOp ::=
{
  top.prolog =
      \ t1::PrologTerm t2::PrologTerm result::PrologTerm ->
        isPrologFormula(t1, minusPrologIsBinOp(), t2, result);
}


aspect production multOp
top::BinOp ::=
{
  top.prolog =
      \ t1::PrologTerm t2::PrologTerm result::PrologTerm ->
        isPrologFormula(t1, multPrologIsBinOp(), t2, result);
}


aspect production divOp
top::BinOp ::=
{
  top.prolog =
      \ t1::PrologTerm t2::PrologTerm result::PrologTerm ->
        isPrologFormula(t1, divPrologIsBinOp(), t2, result);
}


aspect production modOp
top::BinOp ::=
{
  top.prolog =
      \ t1::PrologTerm t2::PrologTerm result::PrologTerm ->
        isPrologFormula(t1, modulusPrologIsBinOp(), t2, result);
}


aspect production appendOp
top::BinOp ::=
{
  top.prolog =
      \ t1::PrologTerm t2::PrologTerm result::PrologTerm ->
        termPrologFormula(
           applicationTerm("string_concat",
              consPrologTermList(t1,
              consPrologTermList(t2,
              consPrologTermList(result, nilPrologTermList())))));
}




attribute prolog<PrologBinOp> occurs on TopBinOp;

aspect production eqOp
top::TopBinOp ::=
{
  top.prolog = eqPrologBinOp();
}


aspect production neqOp
top::TopBinOp ::=
{
  top.prolog = neqPrologBinOp();
}


aspect production lessOp
top::TopBinOp ::=
{
  top.prolog = lessPrologBinOp();
}


aspect production greaterOp
top::TopBinOp ::=
{
  top.prolog = greaterPrologBinOp();
}


aspect production leqOp
top::TopBinOp ::=
{
  top.prolog = leqPrologBinOp();
}


aspect production geqOp
top::TopBinOp ::=
{
  top.prolog = geqPrologBinOp();
}

