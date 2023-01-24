grammar sos:translation:conc:silver;

attribute
   silverConc<String>, silverConcNt, silverConcTerminal,
   parserName, joinParserName
occurs on QName;

synthesized attribute silverConcNt::String;
synthesized attribute silverConcTerminal::String;

synthesized attribute parserName::String;
synthesized attribute joinParserName::String;

aspect production baseName
top::QName ::= name::String
{
  top.silverConcNt = "SilverConcNt_" ++ name ++ "_c";
  top.silverConcTerminal = "SilverConcTerimnal_" ++ name ++ "_c";
  top.silverConc =
      if top.isConcreteNt
      then top.silverConcNt
      else top.silverConcTerminal;

  top.parserName = error("Must have a qualified name");
  top.joinParserName = name;
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.silverConcNt =
      "silverConc:" ++ top.baselessName ++ ":" ++
      "SilverConcNt_" ++ rest.base ++ "_c";
  top.silverConcTerminal =
      "silverConc:" ++ top.baselessName ++ ":" ++
      "SilverConcTerminal_" ++ rest.base ++ "_c";
  top.silverConc =
      if top.isConcreteNt
      then top.silverConcNt
      else top.silverConcTerminal;

  top.parserName = "parse_" ++ top.joinParserName;
  top.joinParserName = name ++ "___" ++ rest.joinParserName;
}
