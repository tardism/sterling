grammar sos:translation:semantic:lambdaProlog;


attribute lpRules, lpProjectionRules occurs on Rule;

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local p::LambdaPrologFormula =
        foldr1(andLambdaPrologFormula, premises.lp);
  top.lpRules =
      if null(premises.lp)
      then [factLambdaPrologRule(conclusion.lpTerm)]
      else [ruleLambdaPrologRule(conclusion.lpTerm, p)];

  top.lpProjectionRules = [];
}

aspect production projRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.lpRules = [];

  top.lpProjectionRules =
      [(conclusion.headRel, conclusion.pcVar,
        if null(premises.lp)
        then factLambdaPrologRule(conclusion.lpTerm)
        else ruleLambdaPrologRule(
                conclusion.lpTerm,
                foldr1(andLambdaPrologFormula, premises.lp)))];
}


aspect production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local p::LambdaPrologFormula =
        foldr1(andLambdaPrologFormula, premises.lp);
  top.lpRules =
      if null(premises.lp)
      then [factLambdaPrologRule(conclusion.lpTerm)]
      else [ruleLambdaPrologRule(conclusion.lpTerm, p)];

  top.lpProjectionRules = [];
}





attribute lp<[LambdaPrologFormula]> occurs on JudgmentList;

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.lp = [];
}


aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.lp = j.lp::rest.lp;
}

