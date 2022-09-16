grammar sos:composed;


imports sos:core;

imports sos:translation:prolog;

--Don't include sos:testing because that isn't meant for use anywhere
--but in testing things work; it should not be used in an actual
--module.


parser p::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
}



function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, p, ioin);
}

