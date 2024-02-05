grammar sos:composed;


imports sos:core;
imports sos:core:semanticDefs:concreteSyntax;
imports sos:core:concreteDefs:concreteSyntax;
imports sos:core:main:concreteSyntax;
imports sos:translation:semantic:extensibella:concreteSyntax;

imports sos:translation:semantic:prolog;
imports sos:translation:semantic:latex;
imports sos:translation:semantic:lambdaProlog;
imports sos:translation:semantic:extensibella:abstractSyntax;

imports sos:translation:conc:silver;

imports sos:translation:main:silver;

--Don't include sos:testing because that isn't meant for use anywhere
--but in testing things work; it should not be used in an actual
--module.


parser p::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
  sos:translation:semantic:extensibella:concreteSyntax;
}

parser c::ConcreteFile_c {
  sos:core:common:concreteSyntax;
  sos:core:concreteDefs:concreteSyntax;
}

parser m::MainFile_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
  sos:core:main:concreteSyntax;
}



function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, p, c, m, ioin);
}

