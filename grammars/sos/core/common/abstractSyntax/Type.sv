grammar sos:core:common:abstractSyntax;


nonterminal Type with
   pp,
   tyEnv, errors, isError,
   type, --full type (e.g. ty becomes mod:ule:ty)
   location;
propagate errors on Type;

--"Equality" also accepts error type with anything
--Since this is intended for error checking, we assume the error for
--   the error type has already been produced
attribute compareTo, isEqual occurs on Type;

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

  top.isEqual =
      case top.compareTo of
      | nameType(x) -> x == name
      | errorType() -> true
      | _ -> false
      end;
}


abstract production intType
top::Type ::=
{
  top.pp = "int";

  top.type = top;

  top.isError = false;

  top.isEqual =
      case top.compareTo of
      | intType() -> true
      | errorType() -> true
      | _ -> false
      end;
}


abstract production stringType
top::Type ::=
{
  top.pp = "string";

  top.type = top;

  top.isError = false;

  top.isEqual =
      case top.compareTo of
      | stringType() -> true
      | errorType() -> true
      | _ -> false
      end;
}


abstract production listType
top::Type ::= ty::Type
{
  top.pp = "[" ++ ty.pp ++ "]";

  top.type = listType(ty.type, location=top.location);

  ty.tyEnv = top.tyEnv;
  top.isError = ty.isError;

  ty.compareTo =
      case top.compareTo of
      | listType(x) -> x
      | _ -> error("Should not access")
      end;
  top.isEqual =
      case top.compareTo of
      | listType(x) -> ty.isEqual
      | errorType() -> true
      | _ -> false
      end;
}


abstract production tupleType
top::Type ::= tys::TypeList
{
  top.pp = "(|" ++ tys.pp_comma ++ "|)";

  top.type = tupleType(tys.types, location=top.location);

  tys.tyEnv = top.tyEnv;
  top.isError = tys.isError;

  tys.compareTo =
      case top.compareTo of
      | tupleType(x) -> x
      | _ -> error("Should not access")
      end;
  top.isEqual =
      case top.compareTo of
      | tupleType(x) -> tys.isEqual
      | errorType() -> true
      | _ -> false
      end;
}


abstract production errorType
top::Type ::=
{
  top.pp = "<error>";

  top.type = top;

  top.isError = true;

  top.isEqual = true;
}





nonterminal TypeList with
   pp_comma, pp_space,
   tyEnv, errors, isError,
   toList<Type>, len,
   types, --full types using Type.type
   location;
propagate errors on TypeList;

attribute compareTo, isEqual occurs on TypeList;

abstract production nilTypeList
top::TypeList ::=
{
  top.pp_comma = "";
  top.pp_space = "";

  top.toList = [];
  top.len = 0;

  top.types = nilTypeList(location=top.location);

  top.isEqual =
      case top.compareTo of
      | nilTypeList() -> true
      | _ -> false
      end;

  top.isError = false;
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

  t.compareTo =
    case top.compareTo of
    | consTypeList(x, _) -> x
    | _ -> error("No type")
    end;
  rest.compareTo =
       case top.compareTo of
       | consTypeList(_, x) -> x
       | _ -> error("No type")
       end;
  top.isEqual =
      case top.compareTo of
      | consTypeList(x, _) -> t.isEqual && rest.isEqual
      | _ -> false
      end;

  top.isError = t.isError || rest.isError;
}

