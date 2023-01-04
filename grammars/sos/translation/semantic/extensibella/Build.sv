grammar sos:translation:semantic:extensibella;


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
  actions <- [runExtensibella];
}


function runExtensibella
IOVal<Integer> ::= m::ModuleList genLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Extensibella output\n", i);

  local mkdir::IOVal<Boolean> = mkdirT(gendir, message);
  local gendir::String = genLoc ++ "extensibella/";
  --definition file
  local defFilename::String =
      gendir ++ a.generateModuleName ++ "___definition.thm";
  local outputDefFile::IOToken =
      writeFileT(defFilename, m.defFileContents, mkdir.io);
  --interface file
  local interfaceFilename::String =
      gendir ++ a.generateModuleName ++ "___interface.xthmi";
  local outputInterfaceFile::IOToken =
      writeFileT(interfaceFilename, m.interfaceFileContents,
         outputDefFile);

  return if a.outputExtensibella
         then ioval(outputInterfaceFile, 0)
         else ioval(i, 0);
}




synthesized attribute outputExtensibella::Boolean occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.outputExtensibella = false;
}


abstract production extensibellaFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLoc = rest.rootLoc;

  top.outputExtensibella = true;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  flags <-
     [flagSpec(name="--extensibella",
               paramString=nothing(),
               help="output Extensibella translation",
               flagParser=flag(extensibellaFlag))];
}
