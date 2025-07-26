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
  top.ocamlDecls = m.ocamlDecls ++ rest.ocamlDecls;
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
  top.ocamlDecls = f.ocamlDecls ++ rest.ocamlDecls;
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