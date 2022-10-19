grammar sos:translation:semantic:latex;


attribute latex<LaTeXTerm> occurs on Type;

aspect production nameType
top::Type ::= name::QName
{
  top.latex = constLaTeXTerm(name.base);
}


aspect production varType
top::Type ::= name::String
{
  top.latex = error("Should not access latex on varType");
}


aspect production intType
top::Type ::=
{
  top.latex = constLaTeXTerm("num");
}


aspect production stringType
top::Type ::=
{
  top.latex = constLaTeXTerm("name");
}


aspect production errorType
top::Type ::=
{
  top.latex = error("Should not access latex on errorType");
}





attribute latex<LaTeXTermList> occurs on TypeList;

aspect production nilTypeList
top::TypeList ::=
{
  top.latex = nilLaTeXTermList();
}


aspect production consTypeList
top::TypeList ::= t::Type rest::TypeList
{
  top.latex = addLaTeXTermList(t.latex, rest.latex);
}
