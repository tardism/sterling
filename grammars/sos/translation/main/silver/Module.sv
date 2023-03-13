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
            {-We need to enumerate them to translate them correctly
              generally.  These three would work with taking the short
              name and applying it to l, but I want to catch if I add
              a function to it so I don't forget to add it here.-}
            if e.name.base == "head"
            then silverFunDef(e.name.base, [("l", "[a]")],
                              "a", "return ioval(ioin, head(l));")
            else if e.name.base == "tail"
            then silverFunDef(e.name.base, [("l", "[a]")],
                              "[a]", "return ioval(ioin, tail(l));")
            else if e.name.base == "null"
            then silverFunDef(e.name.base, [("l", "[a]")],
                              "Boolean", "return ioval(ioin, null(l));")
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
