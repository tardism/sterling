grammar sos:testing;

imports sos:core;
imports sos:core:common:concreteSyntax;
imports sos:core:semanticDefs:concreteSyntax;

imports sos:testing:concreteSyntax;
imports sos:testing:abstractSyntax;


parser testingParse::File_c {
  sos:core:common:concreteSyntax;
  sos:core:semanticDefs:concreteSyntax;
  sos:testing:concreteSyntax;
}


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  --run() already checks errors, so we don't need to add anything else
  return run(args, testingParse, concreteSyntaxParser, mainFileParser,
             ioin);
}
