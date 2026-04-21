grammar sos:translation:semantic:ocaml;

attribute ocamlType occurs on Type;
attribute ocamlTypes occurs on TypeList;

aspect production nameType
top::Type ::= name::QName
{
  top.ocamlType =
    ocamlVariantType(
      if name.isQualified
      then name.ocamlString
      else case name.fullTy of
           | nameType(fn) -> fn.ocamlString
           | _ -> name.ocamlString
           end);
}

aspect production varType
top::Type ::= name::String
{
  top.ocamlType = ocamlVariantType("'a");
}

aspect production intType
top::Type ::=
{
  top.ocamlType = ocamlIntType();
}

aspect production stringType
top::Type ::=
{
  top.ocamlType = ocamlStringType();
}

aspect production listType
top::Type ::= ty::Type
{
  top.ocamlType = ocamlListType(ty.ocamlType);
}

aspect production tupleType
top::Type ::= tys::TypeList
{
  top.ocamlType = ocamlTupleType(tys.ocamlTypes);
}

aspect production errorType
top::Type ::=
{
  top.ocamlType = ocamlVariantType("error");
}

aspect production nilTypeList
top::TypeList ::=
{
  top.ocamlTypes = [];
}

aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.ocamlTypes = t.ocamlType :: rest.ocamlTypes;
}
