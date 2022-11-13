grammar sos:translation:semantic:lambdaProlog;


attribute lp<LambdaPrologFormula>, lpTerm, pcVar occurs on Judgment;

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  top.lp = termLambdaPrologFormula(top.lpTerm);

  top.lpTerm =
      foldl(applicationLambdaPrologTerm,
               constLambdaPrologTerm(rel.fullJudgment.name.lpJudgmentName),
               args.lp);

  top.pcVar = args.pcVar;
}


aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.lp = notLambdaPrologFormula(
              termLambdaPrologFormula(top.lpTerm));

  top.lpTerm =
      foldl(applicationLambdaPrologTerm,
               constLambdaPrologTerm(rel.fullJudgment.name.lpJudgmentName),
               args.lp);

  top.pcVar = error("Should not access pcVar on a negation");
}


aspect production transJudgment
top::Judgment ::= args::TermList ty::QName t::Term translation::Term
{
  top.lp = termLambdaPrologFormula(top.lpTerm);

  top.lpTerm = foldl(applicationLambdaPrologTerm,
                     case ty.fullTy of
                     | nameType(q) ->
                       constLambdaPrologTerm(q.lpTranslationName)
                     | _ -> error("Must be a nameType")
                     end,
                     args.lp ++ [t.lp, translation.lp]);

  top.pcVar = error("Should not access pcVar on a translation");
}


aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.lp = isLambdaPrologFormula(t1.lp, op.lp, t2.lp, result.lp);

  top.lpTerm = error("Should not access lpTerm on binOpJudgment");

  top.pcVar = error("Should not access pcVar on binOpJudgment");
}


aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.lp = op.lp(t1.lp, t2.lp);

  top.lpTerm = error("Should not access lpTerm on topBinOpJudgment");

  top.pcVar = error("Should not access pcVar on topBinOpJudgment");
}





attribute lp<LambdaPrologIsBinOp> occurs on BinOp;

aspect production plusOp
top::BinOp ::=
{
  top.lp = plusLambdaPrologIsBinOp();
}


aspect production minusOp
top::BinOp ::=
{
  top.lp = minusLambdaPrologIsBinOp();
}


aspect production multOp
top::BinOp ::=
{
  top.lp = multLambdaPrologIsBinOp();
}


aspect production divOp
top::BinOp ::=
{
  top.lp = integerDivLambdaPrologIsBinOp();
}


aspect production modOp
top::BinOp ::=
{
  top.lp = modulusLambdaPrologIsBinOp();
}


aspect production appendOp
top::BinOp ::=
{
  top.lp = appendStringLambdaPrologIsBinOp();
}





attribute
   lp<(LambdaPrologFormula ::= LambdaPrologTerm LambdaPrologTerm)>
occurs on TopBinOp;

aspect production eqOp
top::TopBinOp ::=
{
  top.lp = \ t1::LambdaPrologTerm t2::LambdaPrologTerm ->
             binOpLambdaPrologFormula(t1, eqLambdaPrologBinOp(), t2);
}


aspect production neqOp
top::TopBinOp ::=
{
  top.lp = \ t1::LambdaPrologTerm t2::LambdaPrologTerm ->
             notLambdaPrologFormula(
                binOpLambdaPrologFormula(t1, eqLambdaPrologBinOp(),
                                         t2));
}


aspect production lessOp
top::TopBinOp ::=
{
  top.lp = \ t1::LambdaPrologTerm t2::LambdaPrologTerm ->
             binOpLambdaPrologFormula(t1, lessLambdaPrologBinOp(), t2);
}


aspect production greaterOp
top::TopBinOp ::=
{
  top.lp = \ t1::LambdaPrologTerm t2::LambdaPrologTerm ->
             binOpLambdaPrologFormula(t1, greaterLambdaPrologBinOp(),
                                      t2);
}


aspect production leqOp
top::TopBinOp ::=
{
  top.lp = \ t1::LambdaPrologTerm t2::LambdaPrologTerm ->
             binOpLambdaPrologFormula(t1, leqLambdaPrologBinOp(), t2);
}


aspect production geqOp
top::TopBinOp ::=
{
  top.lp = \ t1::LambdaPrologTerm t2::LambdaPrologTerm ->
             binOpLambdaPrologFormula(t1, geqLambdaPrologBinOp(), t2);
}

