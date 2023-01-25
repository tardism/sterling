grammar sos:translation:main:silver;

import silver:util:cmdargs;
import sos:core;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _ _
{
  actions <-
      [actionSpec(
          runFun = runSilverMain,
          shouldDoFun =
             \ a::Decorated CmdArgs ->
               !null(a.concTranslations) && !null(a.semTranslations),
          actionDesc = "Silver Main Function Translation")];
}


function runSilverMain
IOVal<Integer> ::= m::ModuleList genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Silver output for main function\n\n", i);

  return ioval(printT("Not done yet\n", message), 0);
}
