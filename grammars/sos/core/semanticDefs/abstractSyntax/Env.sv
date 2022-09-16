grammar sos:core:semanticDefs:abstractSyntax;


nonterminal JudgmentEnvItem with
   name, types, isExtensible, pcIndex, isError;

abstract production extJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList pcIndex::Integer
{
  top.name = name;

  top.types = args;

  top.isExtensible = true;

  top.pcIndex = pcIndex;

  top.isError = false;
}


abstract production fixedJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isExtensible = false;

  top.pcIndex = error("Should not access on non-extensible judgment");

  top.isError = false;
}


abstract production errorJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isExtensible = true; --default to assuming this

  top.pcIndex = 0; --default value

  top.isError = true;
}





nonterminal TranslationEnvItem with name, types, isError;

abstract production translationEnvItem
top::TranslationEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isError = false;
}





nonterminal RuleEnvItem with
   name, isExtensible, definedRel, isTransRule, isError;

--relation being defined
synthesized attribute definedRel::QName;
--translation rule for a relation
synthesized attribute isTransRule::Boolean;

--Rule for an extensible relation
abstract production extRuleEnvItem
top::RuleEnvItem ::= name::QName definedRel::QName
                     isTransRule::Boolean
{
  top.name = name;

  top.isExtensible = true;

  top.isError = false;

  top.definedRel = definedRel;

  top.isTransRule = isTransRule;
}


--Rule defining a translation of a construct
abstract production translationRuleEnvItem
top::RuleEnvItem ::= name::QName ty::QName
{
  top.name = name;

  top.isExtensible = true;

  top.isError = false;

  top.definedRel = baseName("", location=bogusLoc());

  top.isTransRule = false;
}


--Rule for a fixed relation
abstract production fixedRuleEnvItem
top::RuleEnvItem ::= name::QName definedRel::QName
{
  top.name = name;

  top.isExtensible = false;

  top.isError = false;

  top.definedRel = definedRel;

  top.isTransRule = false;
}


--Rule which we can't determine the necessary information for a
--more-specific RuleEnvItem production
abstract production errorRuleEnvItem
top::RuleEnvItem ::= name::QName isTransRule::Boolean
{
  top.name = name;

  top.isExtensible = false;

  top.isError = true;

  top.definedRel = baseName("", location=bogusLoc());

  top.isTransRule = isTransRule;
}

