grammar sos:translation:semantic:prolog;


aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{

}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{

}




aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{

}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{

}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{

}

