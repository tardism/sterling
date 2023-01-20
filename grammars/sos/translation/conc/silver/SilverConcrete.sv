grammar sos:translation:conc:silver;


imports sos:core:common:abstractSyntax;
imports sos:core:concreteDefs:abstractSyntax;

imports sos:core:modules;

synthesized attribute silverConc<a>::a;





nonterminal SilverConcDecl with pp;

abstract production terminalSilverConcDecl
top::SilverConcDecl ::= name::String regex::Regex
{
  top.pp = "terminal " ++ name ++ "   " ++ regex.pp ++ ";";
}


abstract production ignoreTerminalSilverConcDecl
top::SilverConcDecl ::= name::String contents::Regex
{
  top.pp = "ignore terminal " ++ name ++ "   " ++ contents.pp ++ ";";
}


abstract production nonterminalSilverConcDecl
top::SilverConcDecl ::= name::String
{
  --all nonterminals here will have type ast
  top.pp = "closed nonterminal " ++ name ++ " with ast;";
}


abstract production productionSilverConcDecl
top::SilverConcDecl ::= name::String topName::String topTy::String
                        children::SilverConcProdChildren
                        body::SilverConcAstEq
{
  top.pp = "concrete production " ++ name ++ "\n" ++
           topName ++ "::" ++ topTy ++ " ::= " ++ children.pp ++
           "\n{\n" ++ body.pp ++ "\n}";
}


abstract production importSilverConcDecl
top::SilverConcDecl ::= g::String
{
  top.pp = "import " ++ g ++ ";";
}





nonterminal SilverConcProdChildren with pp;

abstract production emptySilverConcProdChildren
top::SilverConcProdChildren ::=
{
  top.pp = "";
}


abstract production oneSilverConcProdChildren
top::SilverConcProdChildren ::= name::String ty::String
{
  top.pp = name ++ "::" ++ ty;
}


abstract production branchSilverConcProdChildren
top::SilverConcProdChildren ::= a::SilverConcProdChildren
                                b::SilverConcProdChildren
{
  top.pp = a.pp ++ " " ++ b.pp;
}





nonterminal SilverConcAstEq with pp;

abstract production silverConcAstEq
top::SilverConcAstEq ::= topName::String body::SilverConcTerm
{
  top.pp = "  " ++ topName ++ ".ast = " ++ body.pp ++ ";\n";
}





nonterminal SilverConcTerm with pp;

abstract production appendSilverConcTerm
top::SilverConcTerm ::= t1::SilverConcTerm t2::SilverConcTerm
{
  top.pp = "(" ++ t1.pp ++ ") ++ (" ++ t2.pp ++ ")";
}


abstract production lexemeSilverConcTerm
top::SilverConcTerm ::= access::String
{
  top.pp = access ++ ".lexeme";
}


abstract production astSilverConcTerm
top::SilverConcTerm ::= access::String
{
  top.pp = access ++ ".ast";
}


abstract production stringLitSilverConcTerm
top::SilverConcTerm ::= lit::String
{
  top.pp = "\"" ++ lit ++ "\"";
}


abstract production intLitSilverConcTerm
top::SilverConcTerm ::= lit::Integer
{
  top.pp = toString(lit);
}


abstract production toIntSilverConcTerm
top::SilverConcTerm ::= t::SilverConcTerm
{
  top.pp = "toInteger(" ++ t.pp ++ ")";
}


abstract production substringSilverConcTerm
top::SilverConcTerm ::= ind1::SilverConcTerm ind2::SilverConcTerm
                        s::SilverConcTerm
{
  top.pp = "substring(" ++ ind1.pp ++ ", " ++ ind2.pp ++ ", " ++
                      s.pp ++ ")";
}


abstract production lengthSilverConcTerm
top::SilverConcTerm ::= s::SilverConcTerm
{
  top.pp = "length(" ++ s.pp ++ ")";
}


abstract production plusArithSilverConcTerm
top::SilverConcTerm ::= a::SilverConcTerm b::SilverConcTerm
{
  top.pp = "(" ++ a.pp ++ ") + (" ++ b.pp ++ ")";
}


abstract production minusArithSilverConcTerm
top::SilverConcTerm ::= a::SilverConcTerm b::SilverConcTerm
{
  top.pp = "(" ++ a.pp ++ ") - (" ++ b.pp ++ ")";
}
