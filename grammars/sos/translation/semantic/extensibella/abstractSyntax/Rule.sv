grammar sos:translation:semantic:extensibella:abstractSyntax;

attribute
  ebRules, ebTranslationRules
occurs on Rule;

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local premise_vars::[String] =
      nub(flatMap((.vars), premises.eb));
  local conc_vars::[String] = conclusion.eb.vars;
  local binds::[String] = removeAll(conc_vars, premise_vars);
  top.ebRules =
      if null(premises.eb)
      then [factDef(conclusion.eb)]
      else if null(binds)
      then[ruleDef(conclusion.eb,
                   foldr1(andMetaterm, premises.eb))]
      else [ruleDef(conclusion.eb,
                    existsMetaterm(binds,
                       foldr1(andMetaterm, premises.eb)))];

  top.ebTranslationRules = [];
}

aspect production transRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.ebRules = [];

  top.ebTranslationRules =
      [(conclusion.headRel, conclusion.eb, premises.eb,
        conclusion.pcVar)];
}


aspect production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local premise_vars::[String] = nub(flatMap((.vars), premises.eb));
  local conc_vars::[String] = conclusion.eb.vars;
  local binds::[String] = removeAll(conc_vars, premise_vars);
  top.ebRules =
      if null(premises.eb)
      then [factDef(conclusion.eb)]
      else if null(binds)
      then[ruleDef(conclusion.eb,
                   foldr1(andMetaterm, premises.eb))]
      else [ruleDef(conclusion.eb,
                    existsMetaterm(binds,
                       foldr1(andMetaterm, premises.eb)))];

  top.ebTranslationRules = [];
}





attribute
  eb<[Metaterm]>
occurs on JudgmentList;

aspect production nilJudgmentList
top::JudgmentList ::=
{
  top.eb = [];
}


aspect production consJudgmentList
top::JudgmentList ::= j::Judgment rest::JudgmentList
{
  top.eb = j.eb::rest.eb;
}

