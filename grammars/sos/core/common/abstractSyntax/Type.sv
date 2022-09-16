grammar sos:core:common:abstractSyntax;


nonterminal Type with
   pp,
   tyEnv, errors, isError,
   type, --full type (e.g. ty becomes mod:ule:ty)
   location;
propagate errors on Type;

abstract production nameType
top::Type ::= name::QName
{
  top.pp = name.pp;

  name.tyEnv = top.tyEnv;
  top.errors <- name.tyErrors;
  top.isError = !name.tyFound;

  top.type =
      if name.tyFound
      then name.fullTy
      else errorType(location=top.location);
}


abstract production intType
top::Type ::=
{
  top.pp = "int";

  top.type = top;

  top.isError = false;
}


abstract production stringType
top::Type ::=
{
  top.pp = "string";

  top.type = top;

  top.isError = false;
}


abstract production errorType
top::Type ::=
{
  top.pp = "<error>";

  top.type = top;

  top.isError = true;
}





nonterminal TypeList with
   pp_comma, pp_space,
   tyEnv, errors,
   toList<Type>, len,
   types, --full types using Type.type
   location;
propagate errors on TypeList;

--To check whether the PC is an extensible type, the index of it
--When 0, it should be an extensible type
inherited attribute expectedPCIndex::Maybe<Integer> occurs on TypeList;

abstract production nilTypeList
top::TypeList ::=
{
  top.pp_comma = "";
  top.pp_space = "";

  top.toList = [];
  top.len = 0;

  top.types = nilTypeList(location=top.location);
}


abstract production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.pp_comma = if rest.pp_comma == ""
                 then t.pp else t.pp ++ ", " ++ rest.pp_comma;
  top.pp_space = if rest.pp_space == ""
                 then t.pp else t.pp ++ " " ++ rest.pp_space;

  t.tyEnv = top.tyEnv;
  rest.tyEnv = top.tyEnv;

  top.toList = t::rest.toList;
  top.len = 1 + rest.len;

  top.types = consTypeList(t.type, rest.types, location=top.location);
}

