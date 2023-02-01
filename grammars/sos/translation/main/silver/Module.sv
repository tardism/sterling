grammar sos:translation:main:silver;

import sos:core:concreteDefs:abstractSyntax;
import sos:core:modules;


synthesized attribute silverFunDefsModules::[(String, [SilverFunDef])]
   occurs on ModuleList;


aspect production nilModuleList
top::ModuleList ::=
{
  top.silverFunDefsModules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.silverFunDefsModules =
      (m.modName, m.silverFunDefs)::rest.silverFunDefsModules;
}





attribute
   silverFunDefs
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  top.silverFunDefs = files.silverFunDefs;
}





attribute
   silverFunDefs
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.silverFunDefs = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.silverFunDefs = rest.silverFunDefs;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.silverFunDefs = rest.silverFunDefs;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.silverFunDefs = f.silverFunDefs ++ rest.silverFunDefs;
}
