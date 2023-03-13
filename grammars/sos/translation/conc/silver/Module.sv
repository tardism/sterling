grammar sos:translation:conc:silver;

import sos:core:semanticDefs:abstractSyntax only buildsOnDecls, File;
import sos:core:main:abstractSyntax only MainFile;

attribute
   silverConc<[(String, [SilverConcDecl])]>, parsedTypes
occurs on ModuleList;

aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.silverConc = [(stdLibName, files.silverConc)];

  top.parsedTypes = files.parsedTypes;
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.silverConc = m.silverConc::rest.silverConc;

  top.parsedTypes = m.parsedTypes ++ rest.parsedTypes;
}





attribute
   silverConc<(String, [SilverConcDecl])>, parsedTypes
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  local concImports::[SilverConcDecl] =
      map(\ q::QName -> importSilverConcDecl("silverConc:" ++ q.pp),
          top.buildsOnDecls);
  top.silverConc = (name, concImports ++ files.silverConc);

  top.parsedTypes = files.parsedTypes;
}





attribute
   silverConc<[SilverConcDecl]>, parsedTypes
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.silverConc = [];

  top.parsedTypes = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.silverConc = rest.silverConc;

  top.parsedTypes = rest.parsedTypes;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.silverConc = f.silverConc ++ rest.silverConc;

  top.parsedTypes = rest.parsedTypes;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.silverConc = rest.silverConc;

  top.parsedTypes = f.parsedTypes ++ rest.parsedTypes;
}
