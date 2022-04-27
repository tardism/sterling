grammar sos:testing;

imports sos:core;
imports sos:core:files:concreteSyntax;

imports sos:testing:concreteSyntax;
imports sos:testing:abstractSyntax;


parser testingParse::File_c {
  sos:core:files:concreteSyntax;
  sos:testing:concreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  --run() already checks errors, so we don't need to add anything else
  return run(args, testingParse, ioin);
}