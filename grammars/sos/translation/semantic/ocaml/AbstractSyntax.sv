grammar sos:translation:semantic:ocaml;

attribute ocamlConstructors occurs on AbsConstructorDecls;
attribute ocamlDecls occurs on AbsSyntaxDecl;

function capitalizeFirst
String ::= s::String
{
  return if length(s) == 0 then s
         else if substring(0, 1, s) == "a" then "A" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "b" then "B" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "c" then "C" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "d" then "D" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "e" then "E" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "f" then "F" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "g" then "G" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "h" then "H" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "i" then "I" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "j" then "J" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "k" then "K" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "l" then "L" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "m" then "M" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "n" then "N" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "o" then "O" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "p" then "P" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "q" then "Q" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "r" then "R" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "s" then "S" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "t" then "T" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "u" then "U" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "v" then "V" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "w" then "W" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "x" then "X" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "y" then "Y" ++ substring(1, length(s), s)
         else if substring(0, 1, s) == "z" then "Z" ++ substring(1, length(s), s)
         else s;
}

aspect production oneConstructorDecl
top::AbsConstructorDecls ::= name::String tyargs::TypeList
{
  local constructorTypes::[OCamlType] = tyargs.ocamlTypes;
  local capitalizedName::String = capitalizeFirst(name);
  top.ocamlConstructors = [ocamlVariantConstructor(capitalizedName, constructorTypes)];
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
