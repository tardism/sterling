grammar sos:translation:semantic:latex;


attribute latex<LaTeXTerm> occurs on Term;

aspect production const
top::Term ::= name::QName
{
  top.latex = macroLaTeXTerm(name.latexConstr, nilLaTeXTermList());
}


aspect production var
top::Term ::= name::String
{
  top.latex = constLaTeXTerm(name);
}


aspect production num
top::Term ::= i::Integer
{
  top.latex = constLaTeXTerm(toString(i));
}


aspect production stringConst
top::Term ::= s::String
{
  top.latex = constLaTeXTerm("\"" ++ s ++ "\"");
}


aspect production appTerm
top::Term ::= constructor::QName args::TermList
{
  top.latex = macroLaTeXTerm(constructor.latexConstr, args.latex);
}


aspect production tupleTerm
top::Term ::= contents::TermList
{
  top.latex =
      constLaTeXTerm("(" ++
         implode(", ", map((.ppLaTeX), contents.latex.toList)) ++ ")");
}


aspect production nilTerm
top::Term ::=
{
  top.latex = constLaTeXTerm("\\[\\]");
}


aspect production consTerm
top::Term ::= hd::Term tl::Term
{
  top.latex =
      constLaTeXTerm(hd.latex.ppLaTeX ++ "::" ++ tl.latex.ppLaTeX);
}


aspect production ascriptionTerm
top::Term ::= tm::Term ty::Type
{
  top.latex = tm.latex;
}





attribute latex<LaTeXTermList> occurs on TermList;

aspect production nilTermList
top::TermList ::=
{
  top.latex = nilLaTeXTermList();
}


aspect production consTermList
top::TermList ::= t::Term rest::TermList
{
  top.latex = addLaTeXTermList(t.latex, rest.latex);
}
