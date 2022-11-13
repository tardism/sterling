grammar sos:translation:semantic:lambdaProlog;


attribute lpRules, lpTranslationRules occurs on Rule;

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local p::LambdaPrologFormula =
        foldr1(andLambdaPrologFormula, premises.lp);
  top.lpRules =
      if null(premises.lp)
      then [factLambdaPrologRule(conclusion.lpTerm)]
      else [ruleLambdaPrologRule(conclusion.lpTerm, p)];

  top.lpTranslationRules = [];
}

aspect production transRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.lpRules = [];

  top.lpTranslationRules = [(conclusion.headRel, conclusion.pcVar,
                             premises.lp, conclusion.lpTerm)];
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

  top.lpTranslationRules = [];
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

