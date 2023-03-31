grammar sos:testing;

imports sos:core;
imports sos:core:common:concreteSyntax;
imports sos:core:semanticDefs:concreteSyntax;
imports sos:core:concreteDefs:concreteSyntax;

imports sos:testing:semConcreteSyntax;
imports sos:testing:concConcreteSyntax;
imports sos:testing:abstractSyntax;


parser testingParse::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
  sos:testing:semConcreteSyntax;
}

parser testingParseConc::ConcreteFile_c {
  sos:core:common:concreteSyntax;
  sos:core:concreteDefs:concreteSyntax;
  sos:testing:concConcreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  --run() already checks errors, so we don't need to add anything else
  return run(args, testingParse, testingParseConc, mainFileParser,
             ioin);
}
