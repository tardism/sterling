grammar sos:translation:main:silver;

import silver:util:cmdargs;
import sos:core;
import sos:core:modules only ModuleList;


function main
IOVal<Integer> ::= args::[String] ioin::IOToken
{
  return run(args, abstractSyntaxParser, concreteSyntaxParser,
             mainFileParser, ioin);
}


aspect function run
IOVal<Integer> ::= _ _ _ _ _
{
  mainTransActions <-
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

  local silverGenLoc::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      "silverMain";
  local genGrammars::IOVal<Boolean> =
      genSilverMainGrammars(m.silverFunDefsModules, silverGenLoc,
                            message);
  local genGrammarsError::IOToken =
      printT("Error producing Silver grammar files\n", genGrammars.io);
  --generate pieces for running
  local genMain::IOVal<Integer> =
      genSilverMainFunction(genLoc, a.generateModuleName,
         map(fst,  m.silverFunDefsModules), genGrammars.io);

  return
      if !genGrammars.iovalue
      then ioval(genGrammarsError, 2)
      else genMain;
}


function genSilverMainGrammars
IOVal<Boolean> ::= mods::[(String, [SilverFunDef])] genLoc::String
                   i::IOToken
{
  local decls::String = implode("\n", map((.pp), head(mods).2));
  local grmmr::String = "silverMain:" ++ head(mods).1;
  local contents::String =
      "grammar " ++ grmmr ++ ";\n" ++
      "import sos:core:common:abstractSyntax;\n" ++
      "import sos:core:semanticDefs:abstractSyntax;\n" ++
      decls;
  local modSplit::[String] = explode(":", head(mods).1);
  local dir::String =
      genLoc ++ (if endsWith("/", genLoc) then "" else "/") ++
      implode("/", modSplit);
  local mkDirectory::IOVal<Boolean> =
      let run::IOVal<Integer> = systemT("mkdir -p " ++ dir, i)
      in
        ioval(run.io, run.iovalue == 0)
      end;
  local filename::String = dir ++ "/Main.sv";
  local writeGrammarFile::IOToken =
      writeFileT(filename, contents, mkDirectory.io);
  local rest::IOVal<Boolean> =
      genSilverMainGrammars(tail(mods), genLoc, writeGrammarFile);

  return
      case mods of
      | [] -> ioval(i, true)
      | _::_ -> if mkDirectory.iovalue
                then rest
                else ioval(mkDirectory.io, false)
      end;
}


function genSilverMainFunction
IOVal<Integer> ::= genLoc::String module::String allGrmmrs::[String]
                   ioin::IOToken
{
  --Silver imports
  local importGrammars::[String] =
      map(\ s::String -> "import silverMain:" ++ s ++ ";", allGrmmrs);

  --main function
  local mainFunction::String =
      "function main\nIOVal<Integer> ::= " ++
      "args::[String] ioin::IOToken\n{\n" ++
      "   local startP::IOVal<ParserConfig> = init_parse(ioin);\n" ++
      "   local startD::IOVal<DeriveConfig> = init_derive(ioin);\n" ++
      "   local _parseConfig_::ParserConfig = startP.iovalue;\n" ++
      "   local _deriveConfig_::DeriveConfig = startD.iovalue;\n" ++
      "   local run::IOVal<Integer> = " ++
              module ++ ":" ++ funName("main") ++
                                 "(args, startD.io);\n" ++
      "   local endP::IOToken = end_parse(_parseConfig_, run.io);\n" ++
      "   local endD::IOToken = end_derive(_deriveConfig_, endP);\n" ++
      "   return ioval(endD, run.iovalue);\n" ++
      "}";

  local completeContents::String =
      implode("\n", importGrammars) ++ "\n" ++ mainFunction ++ "\n";

  local grammarInfo::(String, String) =
      buildFinalGrammar(module, genLoc);

  --write it out
  local filename::String = grammarInfo.1 ++ "/Main.sv";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ grammarInfo.1, ioin);
  local written::IOToken =
      writeFileT(filename, completeContents, mkDirectory.io);

  return
      if mkDirectory.iovalue == 0
      then ioval(written, 0)
      else mkDirectory;
}
