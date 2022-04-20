grammar sos:core:abstractSyntax;


type Env<a> = [(QName, a)];


function lookupEnv
[a] ::= name::QName env::Env<a>
{
  return lookupAll(name, env);
}





nonterminal TypeEnvItem with name;

abstract production typeEnvItem
top::TypeEnvItem ::= name::QName
{
  top.name = name;
}




-- .type is built type
-- .types is arguments
nonterminal ConstructorEnvItem with name, type, types;

abstract production constructorEnvItem
top::ConstructorEnvItem ::= name::QName builtType::Type args::TypeList
{
  top.name = name;

  top.type = builtType;
  top.types = args;
}





nonterminal JudgmentEnvItem with name, types, isExtensible, pcIndex;

synthesized attribute pcIndex::Integer; --zero-based

abstract production extJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList pcIndex::Integer
{
  top.name = name;

  top.types = args;

  top.isExtensible = true;

  top.pcIndex = pcIndex;
}


abstract production fixedJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isExtensible = false;

  top.pcIndex = error("Should not access on non-extensible judgment");
}


abstract production errorJudgmentEnvItem
top::JudgmentEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;

  top.isExtensible = true; --default to assuming this

  top.pcIndex = 0; --default value
}





nonterminal TranslationEnvItem with name, types;

abstract production translationEnvItem
top::TranslationEnvItem ::= name::QName args::TypeList
{
  top.name = name;

  top.types = args;
}

