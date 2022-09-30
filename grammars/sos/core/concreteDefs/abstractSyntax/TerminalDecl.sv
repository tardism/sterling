grammar sos:core:concreteDefs:abstractSyntax;


nonterminal TerminalDecl with
   pp,
   moduleName,
   concreteDecls,
   concreteEnv,
   errors,
   location;
propagate errors on TerminalDecl;

abstract production ignoreTerminal
top::TerminalDecl ::= r::Regex
{
  top.pp = "ignore " ++ r.pp;

  r.moduleName = top.moduleName;

  top.concreteDecls = [ignoreTerminalEnvItem(r)];
}


abstract production useTerminal
top::TerminalDecl ::= name::String r::Regex
{
  top.pp = name ++ " " ++ r.pp;

  local fullName::QName = addQNameBase(top.moduleName, name);

  r.moduleName = top.moduleName;

  top.concreteDecls = [useTerminalEnvItem(fullName, r)];

  --Check there is only one declaration of this name
  local possibleConcretes::[ConcreteEnvItem] =
        lookupEnv(fullName, top.concreteEnv);
  top.errors <-
      case possibleConcretes of
      | [] -> error("Impossible:  Terminal " ++ fullName.pp ++
                    " must exist; we declared it")
      | [_] -> []
      | l ->
        [errorMessage("Found multiple declarations for concrete " ++
            "name " ++ fullName.pp, location=top.location)]
      end;
}





nonterminal Regex with
   pp,
   moduleName,
   errors,
   location;
propagate errors on Regex;

abstract production charRegex
top::Regex ::= c::String
{
  top.pp = c;
}


abstract production starRegex
top::Regex ::= r::Regex
{
  top.pp = "(" ++ r.pp ++ ")*";

  r.moduleName = top.moduleName;
}


abstract production plusRegex
top::Regex ::= r::Regex
{
  top.pp = "(" ++ r.pp ++ ")+";
  forwards to concatRegex(r, starRegex(r, location=top.location),
                          location=top.location);
}


abstract production concatRegex
top::Regex ::= r1::Regex r2::Regex
{
  top.pp = r1.pp ++ r2.pp;

  r1.moduleName = top.moduleName;
  r2.moduleName = top.moduleName;
}


abstract production alternateRegex
top::Regex ::= r1::Regex r2::Regex
{
  top.pp = "(" ++ r1.pp ++ ")|(" ++ r2.pp ++ ")";

  r1.moduleName = top.moduleName;
  r2.moduleName = top.moduleName;
}


abstract production groupRegex
top::Regex ::= g::RegexGroup
{
  top.pp = "[" ++ g.pp ++ "]";

  g.moduleName = top.moduleName;
}





nonterminal RegexGroup with
   pp,
   moduleName,
   errors,
   location;
propagate errors on RegexGroup;

abstract production emptyRegexGroup
top::RegexGroup ::=
{
  top.pp = "";
}


abstract production charRegexGroup
top::RegexGroup ::= c::String
{
  top.pp = c;
}


abstract production rangeRegexGroup
top::RegexGroup ::= c1::String c2::String
{
  top.pp = c1 ++ "-" ++ c2;

  top.errors <-
      if unescapeString(c1) < unescapeString(c2)
      then []
      else [errorMessage("Character range lower bonud is higher " ++
                         "than upper bound", location=top.location)];
}


abstract production branchRegexGroup
top::RegexGroup ::= g1::RegexGroup g2::RegexGroup
{
  top.pp = g1.pp ++ g2.pp;

  g1.moduleName = top.moduleName;
  g2.moduleName = top.moduleName;
}
