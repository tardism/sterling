grammar sos:translation:conc:silver;


attribute
   silverConc<SilverConcDecl>
occurs on TerminalDecl;

aspect production ignoreTerminal
top::TerminalDecl ::= r::Regex
{
  local concName::String = "IgnoreTerminal" ++ toString(genInt());
  top.silverConc = ignoreTerminalSilverConcDecl(concName, ^r);
}


aspect production useTerminal
top::TerminalDecl ::= name::String r::Regex
{
  local silverName::String =
      toQName(name, top.location).silverConcTerminal;
  top.silverConc = terminalSilverConcDecl(silverName, ^r);
}
