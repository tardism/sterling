grammar sos:translation:main:silver;

import sos:core:concreteDefs:abstractSyntax;
import sos:core:modules;


aspect production nilModuleList
top::ModuleList ::=
{

}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{

}





aspect production module
top::Module ::= name::String files::Files
{

}





aspect production nilFiles
top::Files ::=
{

}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{

}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{

}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{

}
