grammar sos:translation:semantic:ocaml;

attribute ocamlDecls occurs on Rule;

aspect production extRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local functionName::String = name;
  local functionBody::OCamlExpr = 
    premises.ocamlConjunction(conclusion.ocamlExpr);
  
  top.ocamlDecls = 
    [ocamlLetDeclaration(functionName, ["tessssssssst"], ^functionBody)];
}

aspect production defaultRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  top.ocamlDecls = [];
}

aspect production fixedRule
top::Rule ::= premises::JudgmentList name::String conclusion::Judgment
{
  local functionName::String = name;
  local functionBody::OCamlExpr = 
    premises.ocamlConjunction(conclusion.ocamlExpr);
  
  top.ocamlDecls = 
    [ocamlLetDeclaration(functionName, ["a"], ^functionBody)];
}
