grammar sos:translation:conc:silver;

attribute
   silverConc<String>, silverConcNt, silverConcTerminal
occurs on QName;

synthesized attribute silverConcNt::String;
synthesized attribute silverConcTerminal::String;

aspect production baseName
top::QName ::= name::String
{
  top.silverConcNt = "ConcNt_" ++ name;
  top.silverConcTerminal = "Terimnal_" ++ name;
  top.silverConc =
      if top.isConcreteNt
      then top.silverConcNt
      else top.silverConcTerminal;
}


aspect production moduleLayerName
top::QName ::= name::String rest::QName
{
  top.silverConcNt =
      top.baselessName ++ ":" ++ "ConcNt_" ++ rest.base;
  top.silverConcTerminal =
      top.baselessName ++ ":" ++ "Terminal_" ++ rest.base;
  top.silverConc =
      top.baselessName ++ ":" ++
      if top.isConcreteNt
      then top.silverConcNt
      else top.silverConcTerminal;
}
