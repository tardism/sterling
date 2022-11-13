grammar sos:translation:semantic:lambdaProlog;

imports sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;

attribute lpDecls, lpRules, lpTranslationRules occurs on ModuleList;

aspect production nilModuleList
top::ModuleList ::=
{
  top.lpDecls = [];
  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.lpDecls = m.lpDecls;
  top.lpRules = m.lpRules;
  top.lpTranslationRules =
      m.lpTranslationRules ++ rest.lpTranslationRules;

  m.lpTranslationRules_down = rest.lpTranslationRules;
}





attribute
   lpDecls, lpRules, lpTranslationRules, lpTranslationRules_down
occurs on Module;

aspect production module
top::Module ::= name::String files::Files
{
  files.lpTranslationRules_down = top.lpTranslationRules_down;

  top.lpDecls = files.lpDecls;
  top.lpRules = files.lpRules;
  top.lpTranslationRules = files.lpTranslationRules;
}





attribute
   lpDecls, lpRules, lpTranslationRules, lpTranslationRules_down
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.lpDecls = [];

  top.lpRules = [];
  top.lpTranslationRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.lpDecls = f.lpDecls ++ rest.lpDecls;

  f.lpTranslationRules_down = top.lpTranslationRules_down;
  rest.lpTranslationRules_down = top.lpTranslationRules_down;

  top.lpRules = f.lpRules ++ rest.lpRules;
  top.lpTranslationRules =
      f.lpTranslationRules ++ rest.lpTranslationRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.lpDecls = rest.lpDecls;

  rest.lpTranslationRules_down = top.lpTranslationRules_down;

  top.lpRules = rest.lpRules;
  top.lpTranslationRules = rest.lpTranslationRules;
}
