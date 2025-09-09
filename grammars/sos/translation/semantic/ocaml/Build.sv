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
function genSilverFunctions
IOVal<Integer> ::= genLoc::String module::String ocamlFile::String ocamlString::String
                   ioin::IOToken
{
  local grammarInfo::(String, String) =
    buildFinalGrammar(module, genLoc);

    local initFunction::String =
      s"""function init_derive
IOVal<DeriveConfig> ::= ioin::IOToken
{
   return ioval(ioin, ());
}""";
local ocamlFileStr::String = "\"" ++ ocamlFile ++ ".runtime.ml" ++ "\"";
local deriveFunction::String =
      s"""function derive
IOVal<Maybe<[(String, Term)]>> ::= d::DeriveConfig j::Judgment inArgs::[(String, Term)] ioin::IOToken
{ 
  -- OCaml doesn't need runtime derivation, just write a simple message
   local args::String = 
           foldr(\ p::(String, Term) rest::String -> 
              p.1 ++ "=" ++ p.2.ocamlExpr.pp ++ ", " ++ rest, 
                 "", inArgs);  
    local written::IOToken =
      writeFileT(${ocamlFileStr}, args, ioin);
  return ioval(written, nothing());
}""";

local endFunction::String =
      s"""function end_derive
IOToken ::= d::DeriveConfig ioin::IOToken
{
   return ioin;
}""";

  local completeContents::String =
      s"""grammar ${grammarInfo.2};
import silver:util:subprocess;
import sos:core:common:abstractSyntax;
import sos:core:semanticDefs:abstractSyntax;
import sos:translation:semantic:ocaml;
type DeriveConfig = ();

${initFunction}

${deriveFunction}

${endFunction}
""";
  --write it out
  local filename::String = grammarInfo.1 ++ "/Derive.sv";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ grammarInfo.1, ioin);
  local written::IOToken =
      writeFileT(filename, completeContents, mkDirectory.io);

  return
      if mkDirectory.iovalue == 0
      then ioval(written, 0)
      else mkDirectory;
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
  local fileLoc::String = dir ++ a.generateModuleName;
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ dir, message);
  local output::IOToken =
      writeFileT(fileLoc++".ml", ocamlString, mkDirectory.io);
  --write Silver pieces for running
  local genDerive::IOVal<Integer> =
      genSilverFunctions(genLoc, a.generateModuleName, fileLoc, ocamlString,
                         output);
  return
      if mkDirectory.iovalue != 0
      then mkDirectory
      else genDerive;
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
