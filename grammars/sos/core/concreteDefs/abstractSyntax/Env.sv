grammar sos:core:concreteDefs:abstractSyntax;


nonterminal ConcreteEnvItem with name, type, isConcreteNt;

abstract production ignoreTerminalEnvItem
top::ConcreteEnvItem ::= r::Regex
{
  top.name = toQName("__ignoreTerminal", bogusLoc());

  --ignore terminals don't produce anything
  top.type = errorType(location=bogusLoc());

  top.isConcreteNt = false;
}


abstract production useTerminalEnvItem
top::ConcreteEnvItem ::= name::QName r::Regex
{
  top.name = name;

  --regular terminals produce strings
  top.type = stringType(location=bogusLoc());

  top.isConcreteNt = false;
}


abstract production concreteNT
top::ConcreteEnvItem ::= name::QName ty::Type
{
  top.name = name;

  top.type = ty;

  top.isConcreteNt = true;
}


abstract production errorConcreteNT
top::ConcreteEnvItem ::= name::QName
{
  top.name = name;

  top.type = errorType(location=bogusLoc());

  top.isConcreteNt = true;
}
