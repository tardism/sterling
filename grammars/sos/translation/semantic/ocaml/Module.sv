grammar sos:translation:semantic:ocaml;

import sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;

attribute ocamlDecls occurs on Module, ModuleList, Files;

aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.ocamlDecls = files.ocamlDecls;
}

aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  -- Put type declarations first, then non-type declarations 
  local allDecls::[OCamlDecl] = m.ocamlDecls ++ rest.ocamlDecls;
  local typeDecls::[OCamlDecl] = filter(isTypeDecl, allDecls);
  local nonTypeDecls::[OCamlDecl] = filter(\ d::OCamlDecl -> !isTypeDecl(d), allDecls);
  top.ocamlDecls = typeDecls ++ nonTypeDecls;
}

aspect production module
top::Module ::= name::String files::Files
{
  top.ocamlDecls = files.ocamlDecls;
}

aspect production nilFiles
top::Files ::=
{
  top.ocamlDecls = [];
}

aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  local allDecls::[OCamlDecl] = f.ocamlDecls ++ rest.ocamlDecls;
  local typeDecls::[OCamlDecl] = filter(isTypeDecl, allDecls);
  local nonTypeDecls::[OCamlDecl] = filter(\ d::OCamlDecl -> !isTypeDecl(d), allDecls);
  
  local groupedByRuleType::[[OCamlDecl]] = 
    groupBy(\p1::OCamlDecl p2::OCamlDecl 
            -> p1.ocamlRuleType == p2.ocamlRuleType, nonTypeDecls);
  -- local groupedByRuleType2::[[OCamlDecl]] = 
    
  local temp::[OCamlDecl] = map( 
    \decls::[OCamlDecl] -> ocamlLetFull(decls), groupedByRuleType);  
  
  top.ocamlDecls = typeDecls ++ temp;
}

aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.ocamlDecls = rest.ocamlDecls;
}

aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.ocamlDecls = rest.ocamlDecls;
}