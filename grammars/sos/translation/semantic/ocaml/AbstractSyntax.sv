grammar sos:translation:semantic:ocaml;

attribute ocamlConstructors occurs on AbsConstructorDecls;
attribute ocamlDecls occurs on AbsSyntaxDecl;

aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  local constructorTypes::[OCamlType] = tyargs.ocamlTypes;
  top.ocamlConstructors = [ocamlVariantConstructor(name, constructorTypes)];
}

aspect production branchAbsConstructorDecls
top::AbsConstructorDecls ::= d1::AbsConstructorDecls d2::AbsConstructorDecls
{
  top.ocamlConstructors = d1.ocamlConstructors ++ d2.ocamlConstructors;
}

aspect production nilAbsConstructorDecls
top::AbsConstructorDecls ::=
{
  top.ocamlConstructors = [];
}

aspect production initialAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::String constructors::AbsConstructorDecls
{
  top.ocamlDecls = 
    [ocamlTypeDeclaration(ocamlVariantDecl(type, constructors.ocamlConstructors))];
}

aspect production addAbsSyntaxDecl
top::AbsSyntaxDecl ::= type::QName constructors::AbsConstructorDecls
{
  top.ocamlDecls = 
    [ocamlTypeDeclaration(ocamlVariantDecl(type.ocamlString, constructors.ocamlConstructors))];
}
