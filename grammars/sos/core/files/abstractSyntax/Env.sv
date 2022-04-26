grammar sos:core:files:abstractSyntax;


type Env<a> = [a];


--Get all the entries for the name from the environment
function lookupEnv
attribute name {} occurs on a => [a] ::= name::QName env::Env<a>
{
  return
     case env of
     | [] -> []
     | x::rest when x.name == name -> x::lookupEnv(name, rest)
     | _::rest -> lookupEnv(name, rest)
     end;
}


--Why have this?  In case we change the definition of Env
function buildEnv
attribute name {} occurs on a => Env<a> ::= l::[a]
{
  return l;
}





nonterminal TypeEnvItem with name, isError;

abstract production typeEnvItem
top::TypeEnvItem ::= name::QName
{
  top.name = name;

  top.isError = false;
}




-- .type is built type
-- .types is arguments
nonterminal ConstructorEnvItem with name, type, types, isError;

abstract production constructorEnvItem
top::ConstructorEnvItem ::= name::QName builtType::Type args::TypeList
{
  top.name = name;

  top.type = builtType;
  top.types = args;

  top.isError = false;
}





nonterminal JudgmentEnvItem with
   name, types, isExtensible, pcIndex, isError;

synthesized attribute pcIndex::Integer; --zero-based

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





nonterminal RuleEnvItem with name, isExtensible, isError;

abstract production extRuleEnvItem
top::RuleEnvItem ::= name::QName
{
  top.name = name;

  top.isExtensible = true;

  top.isError = false;
}


abstract production fixedRuleEnvItem
top::RuleEnvItem ::= name::QName
{
  top.name = name;

  top.isExtensible = false;

  top.isError = false;
}

