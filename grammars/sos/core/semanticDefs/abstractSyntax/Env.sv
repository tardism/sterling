grammar sos:core:semanticDefs:abstractSyntax;


nonterminal JudgmentEnvItem with
   name, types, isExtensible, pcIndex, pcType, isError;

synthesized attribute pcType::Type;

abstract production extJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList pcIndex::Integer
{
  top.name = name;

  top.types = args;

  top.isExtensible = true;

  top.pcIndex = pcIndex;
  top.pcType = head(drop(pcIndex, args.toList));

  top.isError = false;
}


abstract production fixedJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isExtensible = false;

  top.pcIndex = error("Should not access on non-extensible judgment");
  top.pcType = error("Should not access on non-extensible judgment");

  top.isError = false;
}


abstract production errorJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isExtensible = true; --default to assuming this

  top.pcIndex = 0; --default value
  top.pcType = errorType(location=bogusLoc());

  top.isError = true;
}





nonterminal ProjectionEnvItem with name, types, isError;

abstract production projectionEnvItem
top::ProjectionEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isError = false;
}





nonterminal RuleEnvItem with
   name, isExtensible, definedRel, isProjRule, isError;

--relation being defined
synthesized attribute definedRel::QName;
--projection rule for a relation
synthesized attribute isProjRule::Boolean;

--Rule for an extensible relation
abstract production extRuleEnvItem
top::RuleEnvItem ::= name::QName definedRel::QName
                     isProjRule::Boolean
{
  top.name = name;

  top.isExtensible = true;

  top.isError = false;

  top.definedRel = definedRel;

  top.isProjRule = isProjRule;
}


--Rule defining a projection of a construct
abstract production projectionRuleEnvItem
top::RuleEnvItem ::= name::QName ty::QName
{
  top.name = name;

  top.isExtensible = true;

  top.isError = false;

  top.definedRel = baseName("", location=bogusLoc());

  top.isProjRule = false;
}


--Rule for a fixed relation
abstract production fixedRuleEnvItem
top::RuleEnvItem ::= name::QName definedRel::QName
{
  top.name = name;

  top.isExtensible = false;

  top.isError = false;

  top.definedRel = definedRel;

  top.isProjRule = false;
}


--Rule which we can't determine the necessary information for a
--more-specific RuleEnvItem production
abstract production errorRuleEnvItem
top::RuleEnvItem ::= name::QName isProjRule::Boolean
{
  top.name = name;

  top.isExtensible = false;

  top.isError = true;

  top.definedRel = baseName("", location=bogusLoc());

  top.isProjRule = isProjRule;
}

