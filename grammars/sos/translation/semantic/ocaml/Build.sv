grammar sos:translation:semantic:ocaml;

import silver:util:cmdargs;
import sos:core:modules;
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
  semTransActions <-
      [actionSpec(runFun = runOCaml,
                  shouldDoFun = \ a::Decorated CmdArgs ->
                                  a.outputOCaml,
                  actionDesc = "OCaml Translation")];
}

function runOCaml
IOVal<Integer> ::= m::ModuleList genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken = printT("Producing OCaml output\n", i);

  local ocamlProgram::OCamlProgram = buildOCamlProgram(^m);
  local ocamlString::String = ocamlProgram.pp;

  local dir::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      "ocaml/";
  local fileLoc::String = dir ++ a.generateModuleName ++ ".ml";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ dir, message);
  local output::IOToken =
      writeFileT(fileLoc, ocamlString, mkDirectory.io);

  return
      if mkDirectory.iovalue != 0
      then mkDirectory
      else ioval(output, 0);
}

function buildOCamlProgram
OCamlProgram ::= modules::ModuleList
{
  return ocamlProgram(modules.ocamlDecls);
}

synthesized attribute outputOCaml::Boolean occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.outputOCaml = false;
}

abstract production ocamlFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;
  top.generateModuleName = rest.generateModuleName;
  top.rootLocs = rest.rootLocs;
  top.outputOCaml = true;
  top.concTranslations = rest.concTranslations;
  top.semTranslations = "OCaml"::rest.semTranslations;

  forwards to @rest;
}

aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  semTransFlags <-
     [flagSpec(name="--ocaml",
               paramString=nothing(),
               help="output OCaml translation of semantics",
               flagParser=flag(ocamlFlag))];
}
