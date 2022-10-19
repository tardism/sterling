grammar sos:translation:semantic:latex;


attribute latex<LaTeXAbsSyn> occurs on AbsSyntaxDecl;

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.latex = initLaTeXAbsSyn(type, constructors.latex);
}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  --assume there is only one type with the base name when we want to
  --   put it in a LaTeX document
  top.latex = addLaTeXAbsSyn(type.base, constructors.latex);
}




attribute latex<LaTeXAbsConstructorDecls> occurs on AbsConstructorDecls;

aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.latex = nilLaTeXAbsConstructorDecls();
}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  top.latex = branchLaTeXAbsConstructorDecls(d1.latex, d2.latex);
}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  top.latex = oneLaTeXAbsConstructorDecls(constrMacro(fullName),
                                          tyargs.latex);
}
