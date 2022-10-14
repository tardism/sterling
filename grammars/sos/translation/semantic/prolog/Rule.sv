grammar sos:translation:semantic:prolog;


attribute prologTranslationRules, prologRules occurs on Rule;

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.prologTranslationRules = [];
  top.prologRules =
      if conclusion.isTransJudgment
      then [(baseName("translation___" ++
                conclusion.transType.prolog, location=top.location),
             premises.prolog, conclusion.prologTerm)]
      else [(conclusion.headRel.name, premises.prolog,
             conclusion.prologTerm)];
}


aspect production transRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.prologTranslationRules =
      [(conclusion.headRel, conclusion.pcVar, premises.prolog,
        conclusion.prologTerm)];
  top.prologRules = [];
}


aspect production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.prologTranslationRules = [];
  top.prologRules =
      [(conclusion.headRel.name, premises.prolog,
        conclusion.prologTerm)];
}




attribute prolog<Maybe<PrologFormula>> occurs on JudgmentList;

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.prolog = nothing();
}


aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.prolog =
      case rest.prolog of
      | nothing() -> just(j.prolog)
      | just(x) -> just(andPrologFormula(j.prolog, x))
      end;
}

