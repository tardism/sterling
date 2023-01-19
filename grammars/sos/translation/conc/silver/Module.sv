grammar sos:translation:conc:silver;

--attribute   occurs on ModuleList;

aspect production nilModuleList
top::ModuleList ::=
{

}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{

}





--attribute   occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{

}





attribute
   silverConc<[SilverConcDecl]>
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.silverConc = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.silverConc = rest.silverConc;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.silverConc = f.silverCocn ++ rest.silverConc;
}
