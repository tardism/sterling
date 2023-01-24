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
  actions <-
      [actionSpec(runFun = runSilverConc,
                  shouldDoFun = \ a::Decorated CmdArgs ->
                                  a.outputSilverConc,
                  actionDesc = "Silver Concrete Syntax Translation")];
}


function runSilverConc
IOVal<Integer> ::= m::ModuleList genLoc::String grmmrsLoc::String
                   a::Decorated CmdArgs i::IOToken
{
  local message::IOToken =
      printT("Producing Silver output for concrete syntax\n\n", i);
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

  --generate pieces for running
  local genParse::IOVal<Integer> =
      genSilverFunctions(genLoc, a.generateModuleName, m.parsedTypes,
                         m.nameList, compile.io);

  return
      if !genGrammars.iovalue
      then ioval(genGrammarsError, 2)
      else if compile.iovalue != 0
      then ioval(printCompileError, 2)
      else genParse;
}


--Generate the Silver grammars containing the concrete syntax
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


--Generate the pieces for running a language using this
function genSilverFunctions
IOVal<Integer> ::= genLoc::String module::String parsedNTs::[QName]
                   allGrmmrs::[String] ioin::IOToken
{
  --Silver imports
  local importGrammars::[String] =
      map(\ s::String -> "import silverConc:" ++ s ++ ";\n",
          allGrmmrs);

  --parsers
  local reducedParseNTs::[QName] = nub(parsedNTs);
  local parsers::[String] =
      map(\ q::QName ->
            "parser " ++ q.parserName ++ "::Term_c{\n   " ++
            implode(";\n   ", allGrmmrs) ++ "\n};",
          reducedParseNTs);

  --nonterminal for parser configuration and its constructor
  local parserConfig::String =
      "nonterminal ParserConfig;\n" ++
      "abstract production parserConfig\n" ++
      "top::ParserConfig ::=\n{}";

  --init function for parser---does nothing
  local initFunction::String =
      "function init_parse\nIOVal<ParserConfig> ::= " ++
      "c::Decorated CmdArgs ioin::IOToken\n{\n" ++
      "   return ioval(ioin, parserConfig());\n}";

  --parse function
  local parseFunction::String =
      "function parse\n" ++
      "IOVal<Either<String Term>> ::= p::ParserConfig s::String " ++
                                     "q::QName ioin::IOToken\n{\n" ++
              --parser reserved word
      "   local parseR::(ParseResult<Term_c> ::= String String) = " ++
              foldr(\ q::QName rest::String ->
                      "if q.pp = " ++ q.pp ++
                     " then " ++ q.parserName ++
                     " else " ++ rest,
                    "error(\"Impossible\")", parsedNTs) ++ ";\n" ++
      "   local parsed::ParseResult<Term> = " ++
             "parseR(s, \"<<input>>\");\n" ++
      "   return ioval(ioin, if parsed.parseSuccess " ++
                            "then right(parsed.ast) " ++
                            "else left(parsed.parseErrors));" ++
      "\n}";

  --end function for parser---does nothing
  local endFunction::String =
      "function end_parse\nIOToken ::= p::ParserConfig " ++
      "ioin::IOToken\n{\n   return ioin\n}";

  local grammarInfo::(String, String) =
      buildFinalGrammar(module, genLoc);

  --contents of the Parse.sv file
  local completeContents::String =
      "grammar " ++ grammarInfo.2 ++ ";\n" ++
      implode("", importGrammars) ++
      implode("\n", parsers) ++ "\n" ++
      parserConfig ++ "\n" ++
      initFunction ++ "\n" ++
      parseFunction ++ "\n" ++
      endFunction ++ "\n";

  --write it out
  local filename::String = grammarInfo.1 ++ "/Parse.sv";
  local mkDirectory::IOVal<Integer> =
      systemT("mkdir -p " ++ grammarInfo.1, ioin);
  local written::IOToken =
      writeFileT(filename, completeContents, mkDirectory.io);

  return
      if mkDirectory.iovalue == 0
      then ioval(written, 0)
      else mkDirectory;
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
