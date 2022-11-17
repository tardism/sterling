grammar sos:translation:semantic:lambdaProlog;


attribute lp<[LambdaPrologDeclaration]> occurs on AbsSyntaxDecl;

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.lp = kindDeclaration(fullName.lpTypeName)::constructors.lp;
}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  --kind already declared, just add the new constructors
  top.lp = constructors.lp;
}





attribute lp<[LambdaPrologDeclaration]> occurs on AbsConstructorDecls;

inherited attribute builtLPType::LambdaPrologType;

aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.lp = [];
}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  top.lp = d1.lp ++ d2.lp;
}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  top.lp = [typeDeclaration(fullName.lpConstructorName,
                            foldr(arrowLPType(_, _),
                                  top.builtType.lp, tyargs.types.lp))];
}

