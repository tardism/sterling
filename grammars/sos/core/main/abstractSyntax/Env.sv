grammar sos:core:main:abstractSyntax;

nonterminal FunctionEnvItem with
   name, types, type, isError;

abstract production functionEnvItem
top::FunctionEnvItem ::= name::QName args::TypeList retTy::Type
{
  top.name = name;
  top.types = args;
  top.type = retTy;

  top.isError = false;
}


abstract production errorFunctionEnvItem
top::FunctionEnvItem ::= name::QName
{
  top.name = name;
  top.types = error("errorFunctionEnvItem.types");
  top.type = error("errorFunctionEnvItem.type");

  top.isError = true;
}
