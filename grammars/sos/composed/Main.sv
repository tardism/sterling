grammar sos:composed;


imports sos:core;
imports sos:core:semanticDefs:concreteSyntax;
imports sos:core:concreteDefs:concreteSyntax;

imports sos:translation:semantic:prolog;
imports sos:translation:semantic:latex;
imports sos:translation:semantic:lambdaProlog;
imports sos:translation:semantic:extensibella;

--Don't include sos:testing because that isn't meant for use anywhere
--but in testing things work; it should not be used in an actual
--module.


parser p::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
}

parser c::ConcreteFile_c {
  sos:core:common:concreteSyntax;
  sos:core:concreteDefs:concreteSyntax;
}



function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, p, c, ioin);
}

