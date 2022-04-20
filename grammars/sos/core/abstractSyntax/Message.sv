grammar sos:core:abstractSyntax;


nonterminal Message with pp, location;

abstract production errorMessage
top::Message ::= text::String
{
  --Error:  file line:column:  
  top.pp = "Error:  " ++ top.location.filename ++ " " ++
           toString(top.location.line) ++ ":" ++
           toString(top.location.column) ++ ":  " ++ text;
}


abstract production warningMessage
top::Message ::= text::String
{
  top.pp = "Warning:  " ++ top.location.filename ++ " " ++
           toString(top.location.line) ++ ":" ++
           toString(top.location.column) ++ ":  " ++ text;
}

