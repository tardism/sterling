grammar sos:translation:semantic:lambdaProlog;


attribute lp<LambdaPrologType> occurs on Type;

aspect production nameType
top::Type ::= name::QName
{
  top.lp = nameLPType(name.lpTypeName);
}


aspect production varType
top::Type ::= name::String
{
  top.lp = varLPType(name);
}


aspect production intType
top::Type ::=
{
  top.lp = intLPType();
}


aspect production stringType
top::Type ::=
{
  top.lp = stringLPType();
}


aspect production tupleType
top::Type ::= tys::TypeList
{
  top.lp =
      foldr1(\ ty::LambdaPrologType rest::LambdaPrologType ->
               appLPType(appLPType(nameLPType("pair*"), ty), rest),
             tys.lp);
}


aspect production listType
top::Type ::= ty::Type
{
  top.lp = appLPType(nameLPType("list"), ty.lp);
}


aspect production errorType
top::Type ::=
{
  top.lp = error("Should not translate in presence of errors");
}


aspect production pcType
top::Type ::= t::Type
{
  top.lp = t.lp;
}





attribute
   lp<[LambdaPrologType]>
occurs on TypeList;

aspect production nilTypeList
top::TypeList ::=
{
  top.lp = [];
}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.lp = t.lp::rest.lp;
}

