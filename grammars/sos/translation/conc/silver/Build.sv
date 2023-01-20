grammar sos:translation:conc:silver;

import silver:util:cmdargs;
import sos:core;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _
{
  actions <- [runSilverConc];
}


function runSilverConc
IOVal<Integer> ::= m::ModuleList genLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Silver output for concrete syntax", i);
  local genGrammars::IOVal<Boolean> =
      genSilverConcGrammars(m.silverConc, genLoc, message);
  --continue from here to compile it

  return
      if !genGrammars.iovalue
      then ioval(genGrammars.io, 2)
      else error("Not done");
}


function genSilverConcGrammars
IOVal<Boolean> ::= mods::[(String, [SilverConcDecl])] genLoc::String
                   i::IOToken
{
  local decls::String = implode("\n", map((.pp), head(mods).2));
  local contents::String =
      "grammar " ++ head(mods).1 ++ ";\n" ++ decls;
  local modSplit::[String] = explode(":", head(mods).1);
  local dir::String =
      genLoc ++ "/silverConc/" ++ implode("/", modSplit);
  local mkDirectory::IOVal<Boolean> = mkdirT(dir, i);
  local filename::String = dir ++ "/Concrete.sv";
  local writeGrammarFile::IOToken =
      writeFileT(filename, contents, mkDirectory.io);
  local rest::IOVal<Boolean> =
      genSilverConcGrammars(tail(mods), genLoc, writeGrammarFile);

  return
      case mods of
      | [] -> ioval(i, true)
      | _::_ ->
        if mkDirectory.iovalue
        then rest
        else ioval(mkDirectory.io, false)
      end;
}




synthesized attribute outputSilverConc::Boolean occurs on CmdArgs;

aspect production endCmdArgs
top::CmdArgs ::= l::[String]
{
  top.outputSilverConc = false;
}


abstract production silverConcFlag
top::CmdArgs ::= rest::CmdArgs
{
  top.errors = rest.errors;

  top.generateModuleName = rest.generateModuleName;

  top.rootLoc = rest.rootLoc;

  top.outputSilverConc = true;

  forwards to rest;
}


aspect function parseArgs
Either<String  Decorated CmdArgs> ::= args::[String]
{
  flags <-
     [flagSpec(name="--silver-concrete",
               paramString=nothing(),
               help="output Silver translation of concrete syntax",
               flagParser=flag(silverConcFlag))];
}
