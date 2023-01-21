grammar sos:translation:conc:silver;

attribute
   silverConc<String>, silverConcNt, silverConcTerminal
occurs on QName;

synthesized attribute silverConcNt::String;
synthesized attribute silverConcTerminal::String;

aspect production baseName
top::QName ::= name::String
{
  top.silverConcNt = "SilverConcNt_" ++ name ++ "_c";
  top.silverConcTerminal = "SilverConcTerimnal_" ++ name ++ "_c";
  top.silverConc =
      if top.isConcreteNt
      then top.silverConcNt
      else top.silverConcTerminal;
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
}
