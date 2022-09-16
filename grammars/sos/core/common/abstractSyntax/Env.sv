grammar sos:core:common:abstractSyntax;


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


--Find all the items in the Env for which f is true
function findAllEnv
[a] ::= f::(Boolean ::= a) e::Env<a>
{
  return filter(f, e);
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

