grammar sos:translation:semantic:latex;


import sos:core:modules;
import sos:core:concreteDefs:abstractSyntax;
import sos:core:main:abstractSyntax only MainFile;


attribute ppLaTeX occurs on Module, ModuleList;

aspect production stdLibModuleList
top::ModuleList ::= files::Files
{
  top.ppLaTeX = "";
}


aspect production consModuleList
top::ModuleList ::= m::Module rest::ModuleList
{
  top.ppLaTeX = m.ppLaTeX;
}




aspect production module
top::Module ::= name::String files::Files
{
  local syntaxMacros::String =
        implode("\n", map((.ppLaTeX), top.constructorEnv));
  local transMacros::String =
        implode("\n", map((.ppLaTeX), top.translationEnv));
  local relMacros::String =
        implode("\n", map((.ppLaTeX), top.judgmentEnv));
  local latexMacros::String =
        syntaxMacros ++ "\n" ++ transMacros ++ "\n" ++ relMacros;
  top.ppLaTeX = latexMacros ++ "\n" ++
                implode("", map((.ppLaTeX), files.latexSyntax)) ++
                implode("", map((.ppLaTeX), files.latexRules));
}



attribute latexRules, latexSyntax occurs on Files;

aspect production nilFiles
top::Files ::=
{
  top.latexRules = [];
  top.latexSyntax = [];
}


aspect production consAbstractFiles
top::Files ::= filename::String f::File rest::Files
{
  top.latexRules = f.latexRules ++ rest.latexRules;
  top.latexSyntax = f.latexSyntax ++ rest.latexSyntax;
}


aspect production consConcreteFiles
top::Files ::= filename::String f::ConcreteFile rest::Files
{
  top.latexRules = rest.latexRules;
  top.latexSyntax = rest.latexSyntax;
}


aspect production consMainFiles
top::Files ::= filename::String f::MainFile rest::Files
{
  top.latexRules = rest.latexRules;
  top.latexSyntax = rest.latexSyntax;
}
