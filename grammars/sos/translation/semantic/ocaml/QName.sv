grammar sos:translation:semantic:ocaml;

attribute ocamlString occurs on QName;

aspect production baseName
top::QName ::= name::String
{
  top.ocamlString = name;
}

aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.ocamlString = name ++ "_" ++ rest.ocamlString;
}
