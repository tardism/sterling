grammar sos:translation:semantic:extensibella;


attribute
  ebKinds, ebConstrs
occurs on AbsSyntaxDecl;

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.ebKinds = [kindDecl(fullName.ebTypeName)];

  top.ebConstrs = constructors.ebConstrs;
  constructors.builtEBType = extensibellaNameTy(fullName.ebTypeName);
}


aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  --kind already declared
  top.ebKinds = [];

  top.ebConstrs = constructors.ebConstrs;
  constructors.builtEBType = extensibellaNameTy(type.ebTypeName);
}





attribute
  ebConstrs, builtEBType
occurs on AbsConstructorDecls;

inherited attribute builtEBType::ExtensibellaType;

aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.ebConstrs = [];
}


aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls
                             d2::AbsConstructorDecls
{
  d1.builtEBType = top.builtEBType;
  d2.builtEBType = top.builtEBType;
  top.ebConstrs = d1.ebConstrs ++ d2.ebConstrs;
}


aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  top.ebConstrs = [constrDecl(fullName.ebConstructorName, tyargs.eb,
                              top.builtEBType)];
}
