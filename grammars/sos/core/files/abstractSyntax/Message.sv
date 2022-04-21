grammar sos:core:files:abstractSyntax;


import silver:langutil;


nonterminal Message with pp, location;

abstract production errorMessage
top::Message ::= text::String
{
  top.pp = top.location.unparse ++ ":  Error:  " ++ text;
}


abstract production warningMessage
top::Message ::= text::String
{
  top.pp = top.location.unparse ++ ":  Warning:  " ++ text;
}

