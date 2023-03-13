grammar sos:translation:main:silver;

import sos:core:concreteDefs:abstractSyntax;
import sos:core:modules;


synthesized attribute silverFunDefsModules::[(String, [SilverFunDef])]
   occurs on ModuleList;


aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.silverFunDefsModules =
      [(stdLibName, files.silverFunDefs ++ hardSilverFunDefs)];
  local hardSilverFunDefs::[SilverFunDef] =
      map(\ e::FunctionEnvItem ->
            if e.name.base == "head"
            then silverFunDef(e.name.silverFunName, [("l", "[a]")],
                              "a", "return head(l);")
            else if e.name.base == "tail"
            then silverFunDef(e.name.silverFunName, [("l", "[a]")],
                              "a", "return tail(l);")
            else if e.name.base == "null"
            then silverFunDef(e.name.silverFunName, [("l", "[a]")],
                              "Boolean", "return null(l);")
            else error("Unexpected standard library function in " ++
                       "Silver translation of main files:  " ++
                       e.name.pp),
          hardFunDefs);
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
