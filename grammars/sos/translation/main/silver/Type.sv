grammar sos:translation:main:silver;

attribute
   silverType
occurs on Type;

synthesized attribute silverType::String

aspect production nameType
top::Type ::= name::QName
{
  top.silverType = "Term";
}


aspect production varType
top::Type ::= name::String
{
  top.silverType = error("Should not translate varType");
}


aspect production intType
top::Type ::=
{
  top.silverType = "Integer";
}


aspect production stringType
top::Type ::=
{
  top.silverType = "String";
}


aspect production errorType
top::Type ::=
{
  top.silverType = error("Should not translate with errors");
}


aspect production boolType
top::Type ::=
{
  top.silverType = "Boolean";
}


aspect production listType
top::Type ::= ty::Type
{
  top.silverType = "[" ++ ty.silverType ++ "]";
}





aspect production nilTypeList
top::TypeList ::=
{

}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{

}
