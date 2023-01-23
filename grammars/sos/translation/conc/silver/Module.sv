grammar sos:translation:conc:silver;

import sos:core:semanticDefs:abstractSyntax only buildsOnDecls, File;
import sos:core:main:abstractSyntax only MainFile;

attribute
   silverConc<[(String, [SilverConcDecl])]>
occurs on ModuleList;

aspect production nilModuleList
top::ModuleList ::=
{
  top.silverConc = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.silverConc = m.silverConc::rest.silverConc;
}





attribute
   silverConc<(String, [SilverConcDecl])>
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  local concImports::[SilverConcDecl] =
      map(\ q::QName -> importSilverConcDecl("silverConc:" ++ q.pp),
          top.buildsOnDecls);
  top.silverConc = (name, concImports ++ files.silverConc);
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
  top.silverConc = f.silverConc ++ rest.silverConc;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.silverConc = rest.silverConc;
}
