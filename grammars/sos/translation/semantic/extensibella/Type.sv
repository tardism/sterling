grammar sos:translation:semantic:extensibella;


attribute
   eb<ExtensibellaType>
occurs on Type;

aspect production nameType
top::Type ::= name::QName
{
  top.eb =
      case name.fullTy of
      | nameType(n) -> extensibellaNameTy(n.ebTypeName)
      | _ -> error("Not possible")
      end;
}


aspect production varType
top::Type ::= name::String
{
  top.eb = error("varType.eb");
}


aspect production intType
top::Type ::=
{
  top.eb = extensibellaIntTy();
}


aspect production stringType
top::Type ::=
{
  top.eb = extensibellaStringTy();
}


aspect production errorType
top::Type ::=
{
  top.eb = error("Should not translate in presence of errors");
}


aspect production pcType
top::Type ::= t::Type
{
  --get by copying from forward
}





attribute
   eb<[ExtensibellaType]>
occurs on TypeList;

aspect production nilTypeList
top::TypeList ::=
{
  top.eb = [];
}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.eb = t.eb::rest.eb;
}
