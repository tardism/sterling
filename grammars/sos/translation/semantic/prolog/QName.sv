grammar sos:translation:semantic:prolog;


attribute prolog<String> occurs on QName;

flowtype prolog {} on QName;

aspect production baseName
top::QName ::= name::String
{
  top.prolog = name;
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.prolog = name ++ "_MODULE_" ++ rest.prolog;
}

