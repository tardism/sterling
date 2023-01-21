grammar sos:translation:semantic:lambdaProlog;


import silver:util:cmdargs;
import sos:core:modules;
import sos:core;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _
{
  actions <- [runLambdaProlog];
}


function runLambdaProlog
IOVal<Integer> ::= m::ModuleList genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Lambda Prolog output\n", i);

  local moduleName::String = makeLPModuleName(a.generateModuleName);
  local sigFile::String = moduleName ++ ".sig";
  local modFile::String = moduleName ++ ".mod";
  local importedMods::[String] =
        remove(a.generateModuleName, m.nameList);
  local outputSig::IOToken =
        writeFileT(sigFile, buildSig(moduleName, importedMods,
                                     m.lpDecls), message);
  local outputMod::IOToken =
        writeFileT(modFile, buildMod(moduleName, importedMods,
                                     m.lpRules), outputSig);

  return if a.outputLambdaProlog
         then ioval(outputMod, 0)
         else ioval(i, 0);
}




synthesized attribute outputLambdaProlog::Boolean occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.outputLambdaProlog = false;
}


abstract production lambdaPrologFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLoc = rest.rootLoc;

  top.outputLambdaProlog = true;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  flags <-
     [flagSpec(name="--lprolog",
               paramString=nothing(),
               help="output Lambda Prolog translation",
               flagParser=flag(lambdaPrologFlag))];
}

