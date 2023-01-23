grammar sos:translation:conc:silver;

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
  actions <- [runSilverConc];
}


function runSilverConc
IOVal<Integer> ::= m::ModuleList genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Silver output for concrete syntax\n", i);
  local silverGenLoc::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      "silverConc";
  local genGrammars::IOVal<Boolean> =
      genSilverConcGrammars(m.silverConc, silverGenLoc, message);
  local genGrammarsError::IOToken =
      printT("Error producing Silver grammar files\n", genGrammars.io);
  --compile it
  local compileCmd::String =
      "silver -I " ++ genLoc ++ " " ++
             "-I " ++ grmmrsLoc ++ " " ++
             "-o " ++ silverGenLoc ++ "/test.jar " ++
             "silverConc:" ++ a.generateModuleName;
  local compile::IOVal<Integer> = systemT(compileCmd, genGrammars.io);
  local printCompileError::IOToken =
      printT("Error compiling Silver concrete translation\n" ++
             "  (command: " ++ compileCmd ++ "; returned " ++
             toString(compile.iovalue) ++ ")\n", compile.io);

  return
      if !a.outputSilverConc
      then ioval(i, 0)
      else if !genGrammars.iovalue
      then ioval(genGrammarsError, 2)
      else if compile.iovalue != 0
      then ioval(printCompileError, 2)
      else compile;
}


function genSilverConcGrammars
IOVal<Boolean> ::= mods::[(String, [SilverConcDecl])] genLoc::String
                   i::IOToken
{
  local decls::String = implode("\n", map((.pp), head(mods).2));
  local contents::String =
      "grammar silverConc:" ++ head(mods).1 ++ ";\n" ++
      "import sos:core:common:abstractSyntax;\n" ++
      "import sos:core:semanticDefs:abstractSyntax;\n" ++
      (if length(mods) == 1 --declare ast in first grammar
       then "synthesized attribute ast::Term;\n"
       else "") ++
      decls;
  local modSplit::[String] = explode(":", head(mods).1);
  local dir::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      implode("/", modSplit);
  local mkDirectory::IOVal<Boolean> = --mkdirT(dir, i);
      let run::IOVal<Integer> = systemT("mkdir -p " ++ dir, i)
      in
        ioval(run.io, run.iovalue == 0)
      end;
  local filename::String = dir ++ "/Concrete.sv";
  local writeGrammarFile::IOToken =
      writeFileT(filename, contents, mkDirectory.io);
  local rest::IOVal<Boolean> =
      genSilverConcGrammars(tail(mods), genLoc, writeGrammarFile);

  return
      case mods of
      | [] -> ioval(i, true)
      | _::_ -> if mkDirectory.iovalue
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
