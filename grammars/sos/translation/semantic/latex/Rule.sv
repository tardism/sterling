grammar sos:translation:semantic:latex;


attribute latex<LaTeXRule> occurs on Rule;

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.latex = latexRule(name, premises.latex, conclusion.latex);
}


aspect production transRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.latex = latexRule(name, premises.latex, conclusion.latex);
}


aspect production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.latex = latexRule(name, premises.latex, conclusion.latex);
}




attribute latex<LaTeXJudgmentList> occurs on JudgmentList;

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.latex = nilLaTeXJudgmentList();
}


aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.latex = consLaTeXJudgmentList(j.latex, rest.latex);
}
