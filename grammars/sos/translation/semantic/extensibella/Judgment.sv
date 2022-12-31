grammar sos:translation:semantic:extensibella;


attribute
   eb<Metaterm>, pcVar
occurs on Judgment;

aspect production relation
top::Judgment ::= rel::QName args::TermList
{
  top.eb = relationMetaterm(rel.ebJudgmentName, args.eb);

  top.pcVar = args.pcVar;
}


aspect production negationRelation
top::Judgment ::= rel::QName args::TermList
{
  top.eb =
      impliesMetaterm(relationMetaterm(rel.ebJudgmentName, args.eb),
                      falseMetaterm());

  top.pcVar = error("Can only access pcVar on relation");
}


aspect production transJudgment
top::Judgment ::= args::TermList ty::QName t::Term translation::Term
{
  top.eb = relationMetaterm(ty.ebTranslationName,
              args.eb ++ [t.eb, translation.eb]);

  top.pcVar = error("Can only access pcVar on relation");
}


aspect production binOpJudgment
top::Judgment ::= t1::Term op::BinOp t2::Term result::Term
{
  top.eb = op.eb([t1.eb, t2.eb, result.eb]);

  top.pcVar = error("Can only access pcVar on relation");
}


aspect production topBinOpJudgment
top::Judgment ::= t1::Term op::TopBinOp t2::Term
{
  top.eb = op.eb(t1.eb, t2.eb);

  top.pcVar = error("Can only access pcVar on relation");
}





attribute
  eb<(Metaterm ::= [ExtensibellaTerm])> --assume the right length
occurs on BinOp;

aspect production plusOp
top::BinOp ::=
{
  top.eb =
      \ l::[ExtensibellaTerm] -> relationMetaterm("$plus_integer", l);
}


aspect production minusOp
top::BinOp ::=
{
  top.eb =
      \ l::[ExtensibellaTerm] ->
        relationMetaterm("$minus_integer", l);
}


aspect production multOp
top::BinOp ::=
{
  top.eb =
      \ l::[ExtensibellaTerm] ->
        relationMetaterm("$multiply_integer", l);
}


aspect production divOp
top::BinOp ::=
{
  top.eb =
      \ l::[ExtensibellaTerm] ->
        relationMetaterm("$divide_integer", l);
}


aspect production modOp
top::BinOp ::=
{
  top.eb =
      \ l::[ExtensibellaTerm] ->
        relationMetaterm("$modulus_integer", l);
}


aspect production appendOp
top::BinOp ::=
{
  top.eb =
      \ l::[ExtensibellaTerm] -> relationMetaterm("$append", l);
}





attribute
   eb<(Metaterm ::= ExtensibellaTerm ExtensibellaTerm)>
occurs on TopBinOp;

aspect production eqOp
top::TopBinOp ::=
{
  top.eb = eqMetaterm;
}


aspect production neqOp
top::TopBinOp ::=
{
  top.eb = \ t1::ExtensibellaTerm t2::ExtensibellaTerm ->
             impliesMetaterm(eqMetaterm(t1, t2), falseMetaterm());
}


aspect production lessOp
top::TopBinOp ::=
{
  top.eb = \ t1::ExtensibellaTerm t2::ExtensibellaTerm ->
             relationMetaterm("$less_integer", [t1, t2]);
}


aspect production greaterOp
top::TopBinOp ::=
{
  top.eb = \ t1::ExtensibellaTerm t2::ExtensibellaTerm ->
             relationMetaterm("$greater_integer", [t1, t2]);
}


aspect production leqOp
top::TopBinOp ::=
{
  top.eb = \ t1::ExtensibellaTerm t2::ExtensibellaTerm ->
             relationMetaterm("$lesseq_integer", [t1, t2]);
}


aspect production geqOp
top::TopBinOp ::=
{
  top.eb = \ t1::ExtensibellaTerm t2::ExtensibellaTerm ->
             relationMetaterm("$greatereq_integer", [t1, t2]);
}

