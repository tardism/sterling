grammar sos:translation:prolog;


import sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;


attribute
   prologTranslationRules, prologTranslationRules_down,
   prologRules
occurs on Module, ModuleList;

aspect production nilModuleList
top::ModuleList ::=
{
  top.prologTranslationRules = [];

  top.prologRules = [];
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.prologTranslationRules =
      m.prologTranslationRules ++ rest.prologTranslationRules;
  m.prologTranslationRules_down = top.prologTranslationRules;

  top.prologRules = m.prologRules ++ rest.prologRules;
}




aspect production module
top::Module ::= name::String files::Files
{
  top.prologTranslationRules = files.prologTranslationRules;
  files.prologTranslationRules_down = top.prologTranslationRules_down;

  top.prologRules = files.prologRules;
}



attribute
   prologRules, prologTranslationRules, prologTranslationRules_down
occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.prologTranslationRules = [];
  top.prologRules = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  f.prologTranslationRules_down = top.prologTranslationRules_down;
  rest.prologTranslationRules_down = top.prologTranslationRules_down;
  top.prologTranslationRules =
      f.prologTranslationRules ++ rest.prologTranslationRules;

  top.prologRules = f.prologRules ++ rest.prologRules;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.prologTranslationRules = rest.prologTranslationRules;
  rest.prologTranslationRules_down = top.prologTranslationRules_down;

  top.prologRules = rest.prologRules;
}

