grammar sos:translation:main:silver;

attribute
   silverFunName
occurs on QName;

synthesized attribute silverFunName::String;

aspect production baseName
top::QName ::= name::String
{
  top.silverFunName = funName(name);
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.silverFunName = name ++ ":" ++ rest.silverFunName;
}
